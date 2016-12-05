#|
  This file is a part of shopping-lisp project.
  Copyright (c) 2016 Matt Novenstern (fisxoj@gmail.com)
|#

(in-package :cl-user)
(defpackage shopping-lisp-test-asd
  (:use :cl :asdf))
(in-package :shopping-lisp-test-asd)

(defsystem shopping-lisp-test
  :author "Matt Novenstern"
  :license ""
  :depends-on (:shopping-lisp
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "shopping-lisp"))))
  :description "Test system for shopping-lisp"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
