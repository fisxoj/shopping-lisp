#|
  This file is a part of shopping-lisp project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

#|
  Lisp reimplementation of the backend of the android shopping list app.

  Author: Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage shopping-lisp-asd
  (:use :cl :asdf))
(in-package :shopping-lisp-asd)

(defsystem shopping-lisp
  :version "0.1"
  :author "Matt Novenstern"
  :license ""
  :depends-on (:nest
               :mito
               :alexandria
               :defclass-std
               :jonathan
               :validate
               :cl-pass
               :lack-middleware-accesslog)
  :components ((:module "src"
                :components
                ((:file "constants")
                 (:file "db")
                 (:file "shopping-lisp"))))
  :description "Lisp reimplementation of the backend of the android shopping list app."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op shopping-lisp-test))))
