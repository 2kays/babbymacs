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
   (filename :accessor buf-filename :initarg :filename :type string)
   (state :accessor buf-state
          :initarg :state)
   (cursor-x :accessor buf-cursor-x :initform 0 :type integer)
   (cursor-y :accessor buf-cursor-y :initform 0 :type integer)
   (furthest-x :accessor buf-furthest-x :initform 0 :type integer)
   (view :accessor buf-view :initform 0 :type integer)))
 
(defclass editor ()
  ((running :accessor editor-running :initform t :type boolean)
   (buffers :accessor editor-buffers :initform nil :type list)
   (bufcount :accessor editor-bufcount :initform 0 :type integer)
   (message :accessor editor-msg :initform " " :type string)
   (current-window :accessor editor-current-win :type integer)
   (windows :accessor editor-windows :initform nil :type list)))

;; I think it would be a lot better to implement this as a generic window class,
;; with mixins for different behaviour,
;; e.g. temporary, popup, pad-backed, input-capturing, ...
;;      editor windows would be pad-backed & input-capturing
;;      modeline is window-backed
;;      prompt windows are window-backed, popup & input-capturing

(defclass window ()
  ((focusable :accessor win-focusable :initarg :focusable
              :initform t :type boolean)))

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
   (buffer :initarg :buffer :accessor ed-window-buffer :type integer)))

(defclass popup-window (window)
  ((window-ptr :initarg :pointer
               :accessor popwin-ptr)
   (height :initarg :height
           :accessor popwin-height)
   (width :initarg :width
          :accessor popwin-width)
   (prompt :initarg :prompt :accessor popwin-prompt :initform "" :type string)
   (input-string :initarg :input :accessor popwin-input :initform "" :type string)))

(defclass modeline-window (window)
  ((window-ptr :initarg :pointer :accessor mlwin-ptr)
   (left-text :initarg :left-text
              :accessor mlwin-left-text :type string)
   (right-text :initarg :right-text
               :accessor mlwin-right-text :type string)
   (width :initarg :width
          :accessor mlwin-width)))

(defun make-editor-window (bufno height width)
  "Create an editor window instance."
  ;;(check-type buffer buffer)
  ;; TODO: programmatically determine column max (150 is reasonable for now)
  ;; EXPLANATION FOR FUTURE ME
  ;; Say we have theight of 50, file is 70 lines. We want to allocate the
  ;; pad to be a multiple of theight that is enough to accomodate the lines.
  ;; So we take `ceil(theight / lines)`, which gives us that multiple.
  ;; We multiply theight by that for the pad height.
  (let* ((buffer (get-message-buffer))
         (page-cnt (ceiling (length (buf-state buffer)) height))
         (pointer (charms/ll:newpad (* height page-cnt) width)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate pad for editor window."))
    (let ((edwin (make-instance 'editor-window :pointer pointer
                                :buffer bufno :height height :width width)))
      (push edwin (editor-windows *editor-instance*))
      edwin)))

(defun make-modeline-window (width)
  "Create a modeline window instance."
  (let* ((pointer (charms/ll:newwin *modeline-height* (1- width)
                                    (- (terminal-height) *modeline-height*) 0)))
    (when (cffi:null-pointer-p pointer)
      (error "Failed to allocate pad for editor window."))
    ;; ensure that a modeline instance can never be in focus
    (let ((ml (make-instance 'modeline-window :pointer pointer
                                 :width width :focusable nil)))
      (push ml (editor-windows *editor-instance*))
      ml)))

(defun make-popup-window (prompt &key (height 1) (width 50))
  (let ((typed (make-array 0 :fill-pointer t :adjustable t
                           :element-type 'character))
        (pointer (charms/ll:newwin height (1- width)
                                   (- (terminal-height) height 1) 0)))
    (charms/ll:wbkgd pointer charms/ll:a_reverse)
    (let ((popwin (make-instance 'popup-window :pointer pointer
                                 :prompt prompt :height height :width width
                                 :input typed)))
      (push popwin (editor-windows *editor-instance*))
      popwin)))

;; (charms/ll:pnoutrefresh pad view 0 0 0 (1- winh) (- twidth 1))

(defgeneric refresh-window (window)
  (:documentation ""))

(defmethod refresh-window ((edwin editor-window))
  ""
  (with-slots (window-pad-ptr lines cols buffer height width) edwin
    (charms/ll:prefresh (ed-window-pad-ptr edwin)
                        (buf-view (elt (editor-buffers *editor-instance*) buffer)) 0 0 0
                        height width)))

(defmethod refresh-window ((popwin popup-window))
  ""
  (with-slots (window-ptr height width) popwin
    (charms/ll:werase (popwin-ptr popwin))
    (charms/ll:waddstr (popwin-ptr popwin)
                       (concat (popwin-prompt popwin) (popwin-input popwin)))
    (charms/ll:wrefresh window-ptr)))

(defmethod refresh-window ((mlwin modeline-window))
  ""
  (with-slots (window-ptr) mlwin
    (charms/ll:wnoutrefresh window-ptr)))

(defgeneric update-window (window)
  (:documentation ""))

(defmethod update-window ((edwin editor-window))
  (with-slots (window-pad-ptr buffer cols lines) edwin
    (setf cols (terminal-height))
    (setf lines (terminal-width))
    (let ((actual-buffer (elt (editor-buffers *editor-instance*) buffer)))
      (charms/ll:werase window-pad-ptr)
      (charms/ll:mvwaddstr window-pad-ptr 0 0
                           (state-to-string (buf-state actual-buffer)))
      (charms/ll:wmove window-pad-ptr
                       (buf-cursor-y actual-buffer)
                       (buf-cursor-x actual-buffer)))))

(defmethod update-window ((popwin popup-window))
  (charms/ll:wrefresh (popwin-ptr popwin)))

(defmethod update-window ((mlwin modeline-window))
  ""
  (unless (zerop *modeline-height*)
   (with-slots (window-ptr left-text right-text width) mlwin
     
     (setf left-text (editor-msg *editor-instance*))
     (setf right-text (modeline-formatter *modeline-format*))
     (setf width (terminal-width))
    
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
     )))

(defgeneric delete-window (window)
  (:documentation ""))

(defmethod delete-window ((edwin editor-window))
  (charms/ll:delwin (ed-window-pad-ptr edwin)))
      
(defmethod delete-window ((mlwin modeline-window))
  (charms/ll:delwin (mlwin-ptr mlwin)))

(defmethod delete-window ((popwin popup-window))
  (charms/ll:delwin (popwin-ptr popwin)))

(defgeneric consume-input (window character)
  (:documentation ""))

(defmethod consume-input ((win window) char)
  (setf (editor-msg *editor-instance*) "Consumed1"))

(defmethod consume-input ((edwin editor-window) char)
  (cond ((null char) nil)                  ; ignore nils
        ;; 32->126 are printable, so print c if it's not a part of
        ;; a meta command
        ((and (printablep char) (not *current-keymap*))
         (insert-char char))
        (t (resolve-key char))))

(defmethod consume-input ((popwin popup-window) c)
  (cond ((null c) nil)
        ((printablep c)
         (concatf (popwin-input popwin) (string c)))
        ((eql c #\Lf)
         (setf (editor-msg *editor-instance*) (popwin-input popwin))
         (delete-window popwin)
         ;; TODO: more idiotproof method for adding/removing windows
         (pop (editor-windows *editor-instance*)))
        ((eql c #\Del) (setf (popwin-input popwin)
                             (subseq (popwin-input popwin) 0
                                     (1- (length (popwin-input popwin))))))))

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

(defun make-buffer (&optional name state filename)
  "Creates a BUFFER with name NAME and state STATE (\"\" default)."
  (with-slots (bufcount) *editor-instance*
    ;; If provided state is a list, convert to resizeable vector
    (when (listp state)
      (setf state (make-array 1
                              :element-type 'string
                              :initial-contents state
                              :adjustable t
                              :fill-pointer t)))
    (make-instance 'buffer
                   :name (or name (format nil "buffer~a" (incf bufcount)))
                   :filename filename
                   :state (or state (make-array 1 :element-type 'string
                                                :initial-element ""
                                                :adjustable t
                                                :fill-pointer t)))))

(defun current-window ()
  "Gets the instance of the current window."
  (elt (remove-if-not #'win-focusable (editor-windows *editor-instance*))
       (editor-current-win *editor-instance*)))

(defun current-buffer ()
  "Returns the current buffer in the current window."
  (elt (editor-buffers *editor-instance*)
       (ed-window-buffer (current-window))))

(defparameter *modeline-height* 1
  "The height of the mode line.")

(defparameter *modeline-format* " %p% (%x,%y) : %b "
  "Describes the format of the modeline at various sizes.")

(defun modeline-formatter (string)
  "Returns the formatted modeline string."
  (typecase (current-window)
    (editor-window
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
    (popup-window " ^ ")))

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
  (let ((popwin (make-popup-window prompt
                                   :height height
                                   :width (terminal-width))))
    ;; TODO: properly set the focused window and dispatch key input
    (setf (editor-current-win *editor-instance*) 0)))

(defun run-command* ()
  "Run a command input by the user. Hijacks the current key input."
  (let ((result (popup " Eval: " 1)))
    (when result 
      (setf (editor-msg *editor-instance*)
            (format nil " => ~S" (eval (read-from-string result)))))))

(defun run-command ()
  "Run a command input by the user. Hijacks the current key input."
  (popup " What is your quest? " 1))

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

(defun next-buffer (&optional (delta 1))
  "Jump to the next (+ DELTA)  buffer in the buffer list."
  ;; TODO: cleanup
  (setf (ed-window-buffer (elt (editor-windows *editor-instance*)
                               (editor-current-win *editor-instance*)))
        (mod (+ (ed-window-buffer (elt (editor-windows *editor-instance*)
                                       (editor-current-win *editor-instance*))) delta)
             (length (editor-buffers *editor-instance*)))))

(defun write-buffer-to-file (&optional path)
  "Writes the current buffer to either the path associated with the buffer, 
PATH, or the result of prompting the user for a filepath."
  (with-slots (name filename state) (current-buffer)
    (let ((dest-file (or path filename (popup "Write buffer to path: " 1))))
      (array-to-file dest-file state)
      (setf (editor-msg *editor-instance*)
            (concat "Wrote " name " to " dest-file)))))
;;; End of editor commands

(defparameter *meta-map*
  `((#\x . run-command)                 ; M-x
    (#\v . scroll-page-up)              ; M-v
    ))

(defparameter *c-x-map*
  `((#\Etx . exit-editor)               ; C-x C-c
    (#\o . next-buffer)                 ; C-x o
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
        (t (concat "[" (write-to-string (char-code char)) "]"))))

(defun resolve-key (c &optional override-keymap)
  "Resolves an input key C to a command or nested keymap according to the
current global keymap."
  (let* ((keymap (or override-keymap *current-keymap* *root-keymap*))
         (entry-pair (assoc c keymap)))
    ;; On the root keymap? Reset the editor msg
    (when (equal keymap *root-keymap*)
      (setf (editor-msg *editor-instance*) " "))
    ;; if the entry for the keymap has resolved to something
    ;; if it's a function/symbol, run it
    ;; if it's a list, set the current keymap to it
    (if entry-pair
        (let ((entry (cdr entry-pair)))
          (typecase entry
            ((or function symbol)
             (funcall entry)
             (setf *current-keymap* nil))
            (cons (setf *current-keymap* entry)
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

(defun get-message-buffer ()
  (find "*Messages*" (editor-buffers *editor-instance*)
        :key #'buf-name :test #'string=))

(defun create-message-buffer ()
  (let ((msg-buffer (make-buffer "*Messages*" '("Test"))))
    (push msg-buffer (editor-buffers *editor-instance*))))

(defun main-loop ()
  "Entrypoint for the editor. ARGV should contain a file path."
  ;; Set up terminal behaviour
  (with-curses ()
    (loop :initially
       (multiple-value-bind (theight twidth) (terminal-dimensions)
         (make-editor-window 0 (- theight *modeline-height* 1) (- twidth 1))
         (make-modeline-window twidth))
       :while (editor-running *editor-instance*)
       :for c := (charms:get-char charms:*standard-window* :ignore-error t)
       :do
       ;; Set up our happy C-c/SIGINT handling restart
       ;; TODO: refactor this to be less bleurgh
       (restart-case
           (progn
             ;; Dispatch key input to the current window          
             (consume-input (current-window) c)
             ;; Draw all windows
             (dolist (window (editor-windows *editor-instance*))
               (update-window window)
               (refresh-window window))
             (charms/ll:doupdate))
         (editor-sigint ()
           (resolve-key #\Etx)))
       :finally
       ;; Cleanup
       (mapc #'delete-window (editor-windows *editor-instance*)))))

(defun main (&optional argv)
  "True entrypoint for the editor. Sets up the C-c condition handler."
  ;; This is likely wrong of me, however it does work...
  (setf *editor-instance* (make-instance 'editor))
  (setf (editor-msg *editor-instance*)
        (concat " " (random-from-list *welcomes*)))
  (setf *current-keymap* nil)
  (create-message-buffer)
  (setf (editor-current-win *editor-instance*) 0)  
  ;; if argv is set, open that file, else create an empty buffer
  (push (if argv
            (make-buffer argv (file-to-array argv) argv)
            (make-buffer))
        (editor-buffers *editor-instance*))
  (handler-bind ((sb-sys:interactive-interrupt
                  (lambda (c)
                    (declare (ignore c))
                    (invoke-restart 'editor-sigint))))
    (main-loop)))
