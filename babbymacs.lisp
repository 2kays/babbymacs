;;;; babbymacs.lisp

(in-package #:babbymacs)

"
Easy REPL setup - why doesn't paredit like #| |# ?
(ql:quickload '(:swank :babbymacs) :silent t)
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
;;     - Implement as an instance of a floating/popup window? (DONE)
;;     - Display the result of a command if one is provided. (DONE)
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
   (current-window :accessor current-window)
   (windows :accessor editor-windows :initform nil :type list)))

;; I think it would be a lot better to implement this as a generic window class,
;; with mixins for different behaviour,
;; e.g. temporary, popup, pad-backed, input-capturing, ...
;;      editor windows would be pad-backed & input-capturing
;;      modeline is window-backed
;;      prompt windows are window-backed, popup & input-capturing

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

(defclass popup-window (window)
  ((window-ptr :initarg :pointer
               :accessor popwin-ptr)
   (height :initarg :height
           :accessor popwin-height)
   (width :initarg :width
          :accessor popwin-width)))

(defclass modeline-window (window)
  ((window-ptr :initarg :pointer :accessor mlwin-ptr)
   (left-text :initarg :left-text
              :accessor mlwin-left-text :type string)
   (right-text :initarg :right-text
               :accessor mlwin-right-text :type string)
   (width :initarg :width
          :accessor mlwin-width)))

(defun make-editor-window (buffer height width)
  "Create an editor window instance."
  ;; TODO: programmatically determine column max (150 is reasonable for now)
  ;; EXPLANATION FOR FUTURE ME
  ;; Say we have theight of 50, file is 70 lines. We want to allocate the
  ;; pad to be a multiple of theight that is enough to accomodate the lines.
  ;; So we take `ceil(theight / lines)`, which gives us that multiple.
  ;; We multiply theight by that for the pad height.
  (let* ((page-cnt (ceiling (length (buf-state buffer)) height))
         (pointer (charms/ll:newpad (* height page-cnt) width)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate pad for editor window."))
    (make-instance 'editor-window :pointer pointer
                   :buffer buffer :height height :width width)))

(defun make-modeline-window (width)
  "Create a modeline window instance."
  (let* ((pointer (charms/ll:newwin *modeline-height* (1- width)
                                    (- (terminal-height) *modeline-height*) 0)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate pad for editor window."))
    (make-instance 'modeline-window :pointer pointer
                   :width width)))

;; (charms/ll:pnoutrefresh pad view 0 0 0 (1- winh) (- twidth 1))

(defgeneric refresh-window (window)
  (:documentation ""))

(defmethod refresh-window ((edwin editor-window))
  ""
  (with-slots (window-pad-ptr lines cols buffer height width) edwin
    (charms/ll:prefresh (ed-window-pad-ptr edwin) (buf-view buffer) 0 0 0
                        height width)))

(defmethod refresh-window ((popwin popup-window))
  ""
  (with-slots (window-ptr height width) popwin
    (charms/ll:wrefresh window-ptr)))

(defmethod refresh-window ((mlwin modeline-window))
  ""
  (with-slots (window-ptr) mlwin
    (charms/ll:wnoutrefresh window-ptr)))

(defgeneric update-window (window)
  (:documentation ""))

(defmethod update-window ((edwin editor-window))
  (with-slots (window-pad-ptr buffer) edwin
    (charms/ll:werase window-pad-ptr)
    (charms/ll:mvwaddstr window-pad-ptr 0 0
                         (state-to-string (buf-state buffer)))
    (charms/ll:wmove window-pad-ptr
                     (buf-cursor-y buffer)
                     (buf-cursor-x buffer))))

(defmethod update-window ((popwin popup-window))
  nil)

(defmethod update-window ((mlwin modeline-window))
  ""
  (with-slots (window-ptr left-text right-text width) mlwin
    (charms/ll:wresize window-ptr *modeline-height* (1- width))
    (charms/ll:mvwin window-ptr (- (terminal-height) *modeline-height*) 0)
    (charms/ll:werase window-ptr)
    ;;(charms/ll:attron charms/ll:a_reverse)
    (charms/ll:wbkgd window-ptr charms/ll:a_reverse)
    (charms/ll:mvwaddstr window-ptr 0 0 left-text)
    (charms/ll:mvwaddstr window-ptr 0 (- width (length right-text) 1)
                         right-text)
    ;;(charms/ll:mvwaddstr window-ptr 0 0 (editor-msg *editor-instance*))
    ;;(charms/ll:mvwaddstr window-ptr 0 (- twidth (length mstr) 1) mstr)
    ))

(defgeneric delete-window (window)
  (:documentation ""))

(defmethod delete-window ((edwin editor-window))
  (charms/ll:delwin (ed-window-pad-ptr edwin)))
      
(defmethod delete-window ((mlwin modeline-window))
  (charms/ll:delwin (mlwin-ptr mlwin)))

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
  (multiple-value-bind (theight twidth) (terminal-dimensions)
    (let ((typed (make-array 0 :fill-pointer t :adjustable t
                             :element-type 'character)))
      (let* ((cmdwin (charms/ll:newwin height
                                       (1- twidth) (- theight height 1) 0)))
        ;; (charms/ll:wattron cmdwin (charms/ll:color-pair 1))
        ;; (charms/ll:wbkgd cmdwin (charms/ll:color-pair 1))
        (charms/ll:wbkgd cmdwin charms/ll:a_reverse)
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
        typed))))

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

(defun set-cursor-blink (boolean)
  "Specify if the cursor should blink."
  (charms/ll:curs-set (if boolean 2 1)))

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

(defmacro with-curses ((&key (cursor-blink t)) &body body)
  "Modification of CHARMS:WITH-CURSES."
  `(unwind-protect
        (progn
          (force-output *terminal-io*)
          (let ((stdscr (charms/ll:initscr)))
            (when (cffi:null-pointer-p stdscr)
              (error "Call to CHARMS/LL:INITSCR failed.")))
          ;; Clear the screen
          (charms/ll:wrefresh charms/ll:*stdscr*)
          (charms/ll:werase charms/ll:*stdscr*)
          ;; Initialise colors if available
          (charms/ll:use-default-colors)
          (when (charms/ll:has-colors) (charms/ll:start-color))
          ;; Blinking cursor
          (set-cursor-blink ,cursor-blink)
          (let ((charms:*standard-window* (charms:standard-window)))
            ;; Standard terminal setup
            (charms:disable-echoing)
            (charms:enable-raw-input :interpret-control-characters t)
            (charms:enable-non-blocking-mode charms:*standard-window*)
            ,@body))
     (charms/ll:endwin)
     (charms/ll:standend)))

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
  ;; Set up terminal behaviour
  (with-curses ()
    ;;(charms/ll:init-pair 1 charms/ll:color_white charms/ll:color_black)
    (loop :with (theight twidth) := (multiple-value-list (terminal-dimensions))
       :with ed-win := (make-editor-window (first (editor-buffers *editor-instance*))
                                           (- theight *modeline-height* 1)
                                           (- twidth 1))
       :with ml-win := (make-modeline-window twidth)
       :while (editor-running *editor-instance*)
       :for c := (charms:get-char charms:*standard-window* :ignore-error t)
       :do
       ;; Set up our happy C-c/SIGINT handling restart
       ;; TODO: refactor this to be less bleurgh
       (restart-case
           (with-accessors ((name buf-name) (x buf-cursor-x)
                            (y buf-cursor-y) (state buf-state)
                            (view buf-view))
               (current-buffer)
             ;; Update terminal dimensions
             (multiple-value-setq (theight twidth) (terminal-dimensions))
             (cond ((null c) nil)     ; ignore nils
                   ;; 32->126 are printable, so print c if it's not a part of
                   ;; a meta command
                   ((and (printablep c) (not *current-keymap*))
                    (insert-char c))
                   (t (resolve-key c)))
             ;; Draw the modeline
             (unless (zerop *modeline-height*)
               (setf (mlwin-left-text ml-win)
                     (editor-msg *editor-instance*))
               (setf (mlwin-right-text ml-win)
                     (modeline-formatter *modeline-format*))
               (setf (mlwin-width ml-win) twidth)
               (update-window ml-win)
               (refresh-window ml-win))
             ;; (charms/ll:wbkgd mlwin (charms/ll:color-pair 1))
             (setf (ed-window-cols ed-win) theight)
             (setf (ed-window-lines ed-win) twidth)
             (update-window ed-win)
             (refresh-window ed-win)
             (charms/ll:doupdate))
         (editor-sigint ()
           (resolve-key #\Etx)))
       :finally
       ;; Cleanup
       (delete-window ed-win)
       (delete-window ml-win))))

