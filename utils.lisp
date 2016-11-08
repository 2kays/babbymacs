;;;; utils.lisp

(in-package #:babbymacs)

;; General utilities for Babbymacs.

(defun random-from-list (list)
  "Selects a random element from LIST."
  (elt list (random (length list))))

(defun concat (&rest args)
  "Concatenates ARGS to a string."
  (apply #'concatenate 'string args))

(define-modify-macro concatf (&rest args)
  concat "Concatenate strings into place.")

(defun stream-to-array (stream)
  "Return string stream STREAM as a vector of lines."
  (loop :with vector := (make-array 0 :element-type 'string
                                    :adjustable t :fill-pointer t)
     :for (l s) := (multiple-value-list (read-line stream nil))
     :do (vector-push-extend (if s (or l "") l) vector)
     :until s
     :finally (return vector)))

(defun stream-to-list (stream)
  "Returns a string stream STREAM to a list, handling empty lines followed by
EOF properly."
  (loop :for (l s) := (multiple-value-list (read-line stream nil))
     :collect (if s (or l "") l)
     :until s))

(defun file-to-list (path)
  "Returns a list of lines of the file at PATH."
  (with-open-file (f path)
    (stream-to-list f)))

(defun file-to-array (path)
  "Returns a list of lines of the file at PATH."
  (with-open-file (f path)
    (stream-to-array f)))

(defun file-to-string (path)
  "Dumps file at PATH to a string and returns it."
  (with-open-file (f path)
    (let ((str (make-string (file-length f))))
      (read-sequence str f)
      str)))

;; split a sequence SEQ at POS
(defun split-at (seq pos)
  "Split a sequence SEQ at position POS."
  (let ((s1 (subseq seq 0 pos))
        (s2 (subseq seq pos)))
    (list s1 s2)))

(defun split-many (seq &rest positions)
  "Split a sequence SEQ at POSITIONS."
  (labels ((sm-helper (seq offset positions)
             (if (null positions)
                 (list seq)
                 (let ((pos (first positions)))
                   (destructuring-bind (s1 s2)
                       (split-at seq (- pos offset))
                     (cons s1 (sm-helper s2 pos (rest positions))))))))
    (sm-helper seq 0 positions)))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
          :for old-pos = 0 :then (+ pos part-length)
          :for pos := (search part string
                            :start2 old-pos
                            :test test)
          :do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          :when pos :do (write-string replacement out)
       :while pos)))

(defun remove-at (seq pos &key (count 1))
  "Removes COUNT entries at position POS of SEQ."
  (remove-if (constantly t) seq :start pos :count count))

(defun string-insert-at* (string pos elem)
  "Inserts ELEM into STRING at POS."
  (format nil "~a~a~a" (subseq string 0 pos) elem (subseq string pos)))

(defun insert-into-array (vector value position)
  "Inserts VALUE into VECTOR at POSITION."
  (replace vector vector :start2 position :start1 (1+ position) 
           :end2 (vector-push-extend value vector))
  (setf (aref vector position) value) 
  vector)

(defun remove-from-array (vector position)
  "Removes element at POSITION from VECTOR."
  (replace vector vector :start2 (1+ position) :start1 position)
  (vector-pop vector)
  vector)
