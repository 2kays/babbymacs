;;;; babbymacs.lisp

(in-package #:babbymacs)

"
Easy REPL setup - why doesn't paredit like #| |# ?
(ql:quickload '(:swank :cl-charms) :silent t)
(swank:create-server :port 4006 :dont-close t)
"

;; Concepts:
;;  * STATE - array of strings (lines), the buffer contents
;;  * CURSOR - x, y, constrained to the bounds of the state
;;           cursor is moved for standard editing commands
;;
;; TODO:
;;  * Unit tests!!!
;;  * Multiple ncurses windows: buffer (with scrolling), modeline (DONE-ISH)
;;     - Modeline should be have color option
;;     - External formatting of the modeline (DONE-ISH)
;;  * Handle meta key properly, with more complex keymaps (DONE-MOSTLY)
;;  * Potentially implement major/minors? I want to have modes for editing, but
;;    also for general stuff like no-input, unbounded cursor movement, etc.
;;  * Primitive CL mode with a SWANK client!
;;  * Colours!
;;  * Convert to CLOS? Modes might be a lot easier
;;  * Refactor movement and insertion commands (IN-PROGRESS)
;;  * Multiple buffers
;;  * Popup window like Emacs' popwin.el for completions/command result etc.
;;  * Refactor command popup window into something more flexible than hijacking
;;    the key input loop for its own purposes.
;;     - Implement as an instance of a floating/popup window?
;;     - Display the result of a command if one is provided.
;;
;;  * Hooks, especially on cursor position change (for autoscroll, bounding, ..)

(defclass buffer ()
  ((name :accessor buf-name
         :initarg :name
         :type string)
   (state :accessor buf-state
          :initarg :state)
   (cursor-x :accessor buf-cursor-x :initform 0 :type integer)
   (cursor-y :accessor buf-cursor-y :initform 0 :type integer)
   (furthest-x :accessor buf-furthest-x :initform 0 :type integer)
   (view :accessor buf-view :initform 0 :type integer)))

(defclass editor ()
  ((current :accessor editor-current :initform 0 :type integer)
   (buffers :accessor editor-buffers :initform nil :type list)
   (running :accessor editor-running :initform t :type boolean)
   (bufcount :accessor editor-bufcount :initform 0 :type integer)
   (message :accessor editor-msg :initform " " :type string)
   (current-window :accessor current-window)))

(defclass window ()
  ((id :initform 0 :type integer)))

(defclass editor-window (window)
  ((window-pad-ptr :initarg :pointer
                   :accessor ed-window-pad-ptr
                   :documentation "Pointer to the curses pad.")
   (lines :initarg :lines
          :accessor ed-window-lines)
   (cols :initarg :cols
         :accessor ed-window-cols)
   (height :initarg :height)
   (width :initarg :width)
   (buffer :initarg :buffer :accessor ed-window-buffer)))

(defun make-editor-window (&key buffer lines cols height width)
  "Create an editor window instance."
  (let ((pointer (charms/ll:newpad lines cols)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate pad for editor window."))
    (make-instance 'editor-window :buffer buffer :height height :width width
                   :pointer pointer :lines lines :cols cols)))

;; (charms/ll:pnoutrefresh pad view 0 0 0 (1- winh) (- twidth 1))

(defgeneric refresh-window (window)
  (:documentation ""))

(defmethod refresh-window ((ed-win editor-window))
  ""
  (with-slots (window-pad-ptr lines cols buffer height width) ed-win
    (charms/ll:prefresh (ed-window-pad-ptr ed-win) (buf-view buffer) 0 0 0
                        height width)))

(defun terminal-dimensions ()
  (let (theight twidth)
    (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
    (values theight twidth)))

(defun terminal-height ()
  (multiple-value-bind (height width) (terminal-dimensions)
    (declare (ignore width))
    height))

(defun terminal-width ()
  (multiple-value-bind (height width) (terminal-dimensions)
    (declare (ignore height))
    width))

(defun line-at (y)
  "Returns the line at Y."
  (elt (buf-state (current-buffer)) y))

(defun (setf line-at) (new y)
  "SETF expander for the line at Y."
  (setf (elt (buf-state (current-buffer)) y) new))

(defparameter *welcomes*
  '("Babbymacs welcomes you!"
    "You have entered Babbymacs."
    "Yes, this is Babbymacs."
    "Babbymacs needs no intro.")
  "A list of greetings to warm the heart.")

(defparameter *editor-instance* nil
  "Global editor instance.")

(defun make-buffer (&optional name state)
  "Creates a BUFFER with name NAME and state STATE (\"\" default)."
  (with-slots (bufcount) *editor-instance*
    (make-instance 'buffer
                   :name (or name (format nil "buffer~a" (incf bufcount)))
                   :state (or state (make-array 1 :element-type 'string
                                      :initial-element ""
                                      :adjustable t
                                      :fill-pointer t)))))

(defun current-buffer ()
  "Returns the current buffer."
  (elt (editor-buffers *editor-instance*) (editor-current *editor-instance*)))

(defparameter *modeline-height* 1
  "The height of the mode line.")

(defparameter *modeline-format* " %p% (%x,%y) : %b "
  "Describes the format of the modeline at various sizes.")

(defun modeline-formatter (string)
  "Returns the formatted modeline string."
  (with-accessors ((name buf-name) (x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state))
      (current-buffer)
    ;; TODO: this is awful, refactor
    (loop :with modified := string
       :with spec := `(("%p" . ,(write-to-string
                                 (truncate (* (/ y (max 1 (1- (length state))))
                                              100))))
                       ("%x" . ,(write-to-string x))
                       ("%y" . ,(write-to-string y))
                       ("%b" . ,name))
       :for (k . v) :in spec
       :do (setf modified (replace-all modified k v))
       :finally (return modified))))

(defparameter *current-keymap* nil
  "The current keymap for input lookups.")

(defun state-to-string (state &key newline)
  "Reduce editor state by flattening STATE to a string with newlines, with the
key argument NEWLINE specifying if an additional newline is added to the end."
  (concat (format nil "~{~a~^~%~}" (coerce state 'list)) (and newline (string #\nl))))

;;; Beginning of editor commands

(defun adjust-view ()
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (view buf-view))
      (current-buffer)
    (let (theight twidth)
      (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
      ;; Scroll if cursor exceeds view
      (if (> y (+ view (- theight *modeline-height* 1)))
          (scroll (floor (/ theight 2))))
      (if (< y view)
          (scroll (- (floor (/ theight 2))))))))

(defun forward (&optional (delta 1))
  "Moves the cursor forward."
  (labels ((forward-1 (del)
             (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                              (state buf-state) (fx buf-furthest-x))
                 (current-buffer)
               ;; don't go further back if we're at the start
               ;; ...or further forward if we're at the end
               (unless (or (and (= x 0) (= y 0) (> 0 del))
                           (and (= x (length (elt state y)))
                                (= y (1- (length state)))
                                (< 0 del)))
                 (incf x del))
               (cond ((and (> x (length (elt state y)))
                           (< y (1- (length state))))
                      ;; wrap to next line if we aren't on the last one
                      (incf y)
                      (setf x 0))
                     ((and (< x 0) (> y 0))
                      ;; wrap to the previous line if we're not on the first
                      (decf y)
                      (setf x (length (elt state y)))))
               (setf fx x))))
    ;; get the sign of delta, loop for |delta| and multiply by sign
    ;; allows us to move backward without separate handling of neg delta
    (let ((sign (signum delta)))
      (dotimes (v (abs delta))
        (forward-1 (* 1 sign))))))

(defun backward (&optional (delta 1))
  "Moves the cursor backward."
  (forward (* delta -1)))

(defun down (&optional (delta 1))
  "Moves the cursor down."
  (labels ((down-1 (del)
             (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                              (state buf-state) (fx buf-furthest-x)
                              (view buf-view))
                 (current-buffer)
               ;; bounds checking: first clause is up, second is down
               (when (or (and (< del 0) (> y 0))
                         (and (> del 0) (< y (1- (length state)))))
                 (incf y del)
                 ;; handle furthest column
                 (setf x (min fx (length (elt state y))))))))
    (let ((sign (signum delta)))
      (dotimes (v (abs delta))
        (down-1 (* 1 sign))))))

(defun up (&optional (delta 1))
  "Moves the cursor up."
  (down (* delta -1)))

(defun join-lines (&optional (offset 1))
  "Join the current line with the line at OFFSET."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (when (<= 0 (+ y offset) (1- (length state)))
     (let ((line (line-at y))
           (jline (line-at (+ y offset))))
       (setf (line-at (+ y offset)) (concat jline line))
       (remove-from-array state y)))))

(defun backspace ()
  "Backspaces from cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state) (fx buf-furthest-x)
                   (view buf-view))
      (current-buffer)
    (cond ((zerop x)
           (setf x (1+ (length (elt state (1- y))))
                 fx x)
           (join-lines -1)
           (decf y)
           (adjust-view))
          (t (setf (line-at y) (remove-at (line-at y) (1- x)))))
    (backward)))

(defun delete-char ()
  "Deletes char at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state) (fx buf-furthest-x))
      (current-buffer)
    (cond ((= x (length (elt state y)))
           (join-lines 1)
           (setf x 0
                 fx 0))
          (t (setf (line-at y) (remove-at (line-at y) x))))))

(defun newline ()
  "Inserts a newline at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (destructuring-bind (s1 s2) (split-at (elt state y) x)
      (setf (line-at y) s1)
      (insert-into-array state s2 (1+ y))
      (down)
      (line-beginning))))

(defun line-end ()
  "Jumps to the end of a line."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state) (fx buf-furthest-x))
      (current-buffer)
    (setf x (length (elt state y))
          fx x)))

(defun line-beginning ()
  "Jumps to the beginning of a line."
  (setf (buf-cursor-x (current-buffer)) 0
        (buf-furthest-x (current-buffer)) 0))

(defun insert-char (c)
  "Inserts a character at the cursor position."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (destructuring-bind (s1 s2)
        (split-at (elt state y) x)
      (setf (line-at y) (format nil "~a~a~a" s1 c s2))
      (forward))))

(defun quit-command ()
  (setf (editor-msg *editor-instance*) "Quit!"))

(defun popup (prompt height)
  "Retrieves an input from the user. Hijacks the current key input."
  (let ((theight 1) (twidth 1)
        (typed (make-array 0 :fill-pointer t :adjustable t
                           :element-type 'character)))
    (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
    (let* (;; (ratio (floor (/ theight 3)))
           (cmdwin (charms/ll:newwin height
                                     (1- twidth) (- theight height 1) 0)))
      ;; (charms/ll:wattron cmdwin (charms/ll:color-pair 1))
      (charms/ll:wbkgd cmdwin (charms/ll:color-pair 1))
      (loop :named cmd-loop
         :while (editor-running *editor-instance*)
         :for c := (charms:get-char charms:*standard-window* :ignore-error t)
         :do
         (charms/ll:werase cmdwin)
         (charms/ll:waddstr cmdwin (concat prompt typed))
         (cond ((null c) nil)
               ((and (> (char-code c) 31)
                     (< (char-code c) 127))
                (vector-push-extend c typed))
               ((eql c #\Bel) (setf typed nil) (return-from cmd-loop))
               ((eql c #\Del) (vector-pop typed))
               (t (return-from cmd-loop)))
         (charms/ll:wrefresh cmdwin))
      ;; (charms/ll:wattron cmdwin (charms/ll:color-pair 1))
      (charms/ll:delwin cmdwin)
      (charms/ll:erase)
      ;;(charms/ll:refresh)
      typed)))

(defun run-command ()
  "Run a command input by the user. Hijacks the current key input."
  (let ((result (popup " Eval: " 1)))
    (when result
      (setf (editor-msg *editor-instance*)
            (format nil " => ~S" (eval (read-from-string result)))))))

(defun exit-editor (&optional force)
  "Exits the editor."
  (if (not force)
      (if (equal (popup " Are you sure you want to quit? (y/n) " 1) "y")
          (setf (editor-running *editor-instance*) nil))
      (setf (editor-running *editor-instance*) nil)))

(defun scroll (amount)
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (view buf-view))
      (current-buffer)
    (incf view amount)
    (setf view (max view 0))))

;; TODO: scroll-page-up/down needs reimplementing cleanly, with cursor bound
;;       checking and correct behaviour.
(defun scroll-page-down ()
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (view buf-view) (fx buf-furthest-x) (state buf-state))
      (current-buffer)
    (let (theight twidth)
      (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
      (scroll (- (1+ theight) *modeline-height*))
      (incf y (- (1+ theight) *modeline-height*))
      (setf x 0
            fx 0))))

(defun scroll-page-up ()
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (view buf-view) (fx buf-furthest-x))
      (current-buffer)
    (let (theight twidth)
      (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
      (scroll (- (- (1+ theight) *modeline-height*)))
      (decf y (- (1+ theight) *modeline-height*))
      (setf x 0
            fx 0))))

;;; End of editor commands

(defparameter *meta-map*
  `((#\x . run-command)                 ; M-x
    (#\v . scroll-page-up)              ; M-v
    ))

(defparameter *c-x-map*
  `((#\Etx . exit-editor)               ; C-x C-c
    ))

(defparameter *root-keymap*
  `((#\Ack . forward)                   ; C-f
    (#\Stx . backward)                  ; C-b
    (#\So  . down)                      ; C-n
    (#\Dle . up)                        ; C-p
    (#\Del . backspace)                 ; backspace
    (#\Eot . delete-char)               ; C-d
    (#\Bs  . delete-char)               ; delete
    (#\Can . ,*c-x-map*)                ; C-x
    (#\Lf  . newline)                   ; return
    (#\Enq . line-end)                  ; C-e
    (#\Soh . line-beginning)            ; C-a
    (#\Esc . ,*meta-map*)               ; meta key (alt/esc)
    (#\Bel . quit-command)              ; C-g
    (#\Syn . scroll-page-down)          ; C-v
    ))

(defun printablep (char)
  "Checks if CHAR is a printable ASCII character."
  (< 31 (char-code char) 127))

(defun prettify-char (char)
  "Prettify CHAR (e.g. #\Bel -> \"C-g\")."
  (cond ((printablep char) char)
        ((char= char #\Esc) "ESC")
        ((char= char #\Backspace) "<backspace>")
        ((< 0 (char-code char) 32)
         (concat "C-" (string (code-char (+ 96 (char-code char))))))
        (t (concat "\\" (write-to-string (char-code char))))))

(defun resolve-key (c)
  "Resolves an input key C to a command or nested keymap according to the
current global keymap."
  (let* ((entry-pair (assoc c (if *current-keymap*
                                  *current-keymap*
                                  (prog1 *root-keymap*
                                    ;; If it's the root keymap, reset the msg
                                    (setf (editor-msg *editor-instance*)
                                          " "))))))
    ;; if the entry for the keymap has resolved to something
    ;; if it's a function/symbol, run it
    ;; if it's a list, set the current keymap to it
    (if entry-pair
        (let ((entry (cdr entry-pair)))
          (cond ((or (functionp entry)
                     (symbolp entry))
                 (funcall entry)
                 (setf *current-keymap* nil))
                ((consp entry)
                 (setf *current-keymap* entry)
                 (concatf (editor-msg *editor-instance*)
                          (string (prettify-char c)) " "))
                (t (setf *current-keymap* nil)
                   (concatf (editor-msg *editor-instance*)
                            (string (prettify-char c)) " is invalid."))))
        (progn (setf *current-keymap* nil)
               (concatf (editor-msg *editor-instance*)
                        (string (prettify-char c)) " is unbound.")))))

(defun main (&optional argv)
  "True entrypoint for the editor. Sets up the C-c condition handler."
  ;; This is likely wrong of me, however it does work...
  (handler-bind ((sb-sys:interactive-interrupt
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'editor-sigint))))
    (%main argv)))

(defun %main (&optional argv)
  "Entrypoint for the editor. ARGV should contain a file path."
  (setf *editor-instance* (make-instance 'editor))
  (setf (editor-msg *editor-instance*)
        (concat " " (random-from-list *welcomes*)))
  (setf *current-keymap* nil)
  ;; if argv is set, open that file, else create an empty buffer
  (push (if argv
            (make-buffer argv (file-to-array argv))
            (make-buffer))
        (editor-buffers *editor-instance*))
  (charms:with-curses ()
    (charms/ll:start-color)
    (charms/ll:curs-set 2)
    (charms/ll:werase charms/ll:*stdscr*)
    (let* (
           (theight 1)
           (twidth 1))
      ;; Set initial terminal size
      (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
      (charms/ll:use-default-colors)
      (charms/ll:init-pair 1 charms/ll:color_white charms/ll:color_black)
      ;; Build the pad according to the file state
      ;; TODO: programmatically determine column max (150 is reasonable for now)
      ;; EXPLANATION FOR FUTURE ME
      ;; Say we have theight of 50, file is 70 lines. We want to allocate the
      ;; pad to be a multiple of theight that is enough to accomodate the lines.
      ;; So we take `ceil(theight / lines)`, which gives us that multiple.
      ;; We multiply theight by that for the pad height.
      ;; TODO: dynamically react to terminal height changes when allocating pad
      (let* ((page-cnt (ceiling (length (buf-state (current-buffer))) theight))
             (ed-win (make-editor-window :buffer (first (editor-buffers *editor-instance*))
                                         :height (- theight *modeline-height* 1)
                                         :width (- twidth 1)
                                         :lines (* theight page-cnt) :cols twidth))
             (pad (ed-window-pad-ptr ed-win))
             (mlwin (charms/ll:newwin *modeline-height* (1- twidth)
                                      (- theight *modeline-height*) 0)))
        ;; Set up terminal behaviour
        ;; (charms:clear-window charms:*standard-window* :force-repaint t)
        (charms:disable-echoing)
        (charms:enable-raw-input :interpret-control-characters t)
        (charms:enable-non-blocking-mode charms:*standard-window*)
        (loop :named driver-loop
           :while (editor-running *editor-instance*)
           :for c := (charms:get-char charms:*standard-window* :ignore-error t)
           :do
           ;; Set up our happy C-c/SIGINT handling restart
           ;; TODO: refactor this to be less bleurgh
           (restart-case
               (progn
                 ;; Update terminal height and width
                 (let ((last-theight theight)
                       (last-twidth twidth))
                   (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
                   (when (or (/= theight last-theight)
                             (/= twidth last-twidth))
                     (charms/ll:wresize mlwin *modeline-height* (1- twidth))
                     (charms/ll:mvwin mlwin (- theight *modeline-height*) 0)))
                 (with-accessors ((name buf-name) (x buf-cursor-x)
                                  (y buf-cursor-y) (state buf-state)
                                  (view buf-view))
                     (current-buffer)
                   ;; if we previously pressed the meta key, resolve commands from the
                   ;; meta map, otherwise use the standard root key map
                   ;;(if c (format t "received ~s~%" c))
                   (cond ((null c) nil) ; ignore nils
                         ;; 32->126 are printable, so print c if it's not a part of
                         ;; a meta command
                         ((and (printablep c) (not *current-keymap*))
                          (insert-char c))
                         (t (resolve-key c)))
                   ;; write the updated file state to the pad and display it at the
                   ;; relevant y level
                   (let* ((mlh *modeline-height*)
                          (winh (- theight mlh))
                          (mstr (modeline-formatter *modeline-format*)))
                     ;; Draw the modeline
                     (unless (zerop mlh)
                       (charms/ll:werase mlwin)
                       (charms/ll:wbkgd mlwin (charms/ll:color-pair 1))
                       ;; (charms/ll:wattron mlwin (charms/ll:color-pair 1))
                       (charms/ll:mvwaddstr mlwin 0 0 (editor-msg *editor-instance*))
                       (charms/ll:mvwaddstr mlwin 0 (- twidth (length mstr) 1) mstr)
                       ;; (charms/ll:wattroff mlwin (charms/ll:color-pair 1))
                       (charms/ll:wnoutrefresh mlwin)
                       )
                     ;; (charms/ll:wbkgd mlwin (charms/ll:color-pair 1))
                     (charms/ll:werase pad)
                     (charms/ll:mvwaddstr pad 0 0 (state-to-string state))
                     (charms/ll:wmove pad y x)

                     ;; (charms/ll:pnoutrefresh pad view 0 0 0 (1- winh) (- twidth 1))
                     (refresh-window ed-win)
                     (charms/ll:doupdate))))
             (editor-sigint ()
               (resolve-key #\Etx))))
        ;; Cleanup
        ;; (charms/ll:init-pair 1 charms/ll:color_black charms/ll:color_white)
        (charms/ll:delwin pad)
        (charms/ll:delwin mlwin)
        (charms/ll:standend)))))

