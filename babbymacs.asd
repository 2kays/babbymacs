;;;; babbymacs.asd

(asdf:defsystem #:babbymacs
  :description "A little emacs-like editor."
  :author "Kyle K <twokays>"
  :license "MIT"
  :depends-on (:cl-charms)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "babbymacs")))

