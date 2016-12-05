(defpackage #:shopping-lisp.constants
  (:use #:cl)
  (:export #:+backend-version+

           #:+api-success-list+
           #:+api-success-list-empty+
           #:+api-success-update+
           #:+api-success-favorite+
           #:+api-success-delete+
           #:+api-success-save+
           #:+api-success-delete+

           #:+api-error-server+
           #:+api-error-404+
           #:+api-error-403+
           #:+api-error-missing-function+
           #:+api-error-no-database+
           #:+api-error-config+
           #:+api-error-unknown+
           #:+api-error-database-connect+
           #:+api-error-missing-parameter+
           #:+api-error-function-not-specified+
           #:+api-error-not-configured+
           #:+api-error-update+
           #:+api-error-favorite+
           #:+api-error-delete+
           #:+api-error-save+
           #:+api-error-clear+))

(in-package #:shopping-lisp.constants)

;;;; Constants
;;;;
;;;; These are response codes that the server uses to communicate to the app.  The app seems to expect
;;;; a certain version for the backend and won't accept communication without that set.
;;;;
;;;; The other codes mostly don't do anything special, as far as I can tell, aside
;;;; from +api-success-list-empty+, which communicates that the shopping list is empty.


(defconstant +backend-version+ 1.0)
(defconstant +api-success-list+ 1000)
(defconstant +api-success-list-empty+ 1001)
(defconstant +api-success-update+ 1002)
(defconstant +api-success-favorite+ 1003)
(defconstant +api-success-delete+ 1004)
(defconstant +api-success-save+ 1005)
(defconstant +api-success-clear+ 1006)
(defconstant +api-error-server+ 5000)
(defconstant +api-error-404+ 5001)
(defconstant +api-error-403+ 5002)
(defconstant +api-error-missing-function+ 5003)
(defconstant +api-error-no-database+ 5004)
(defconstant +api-error-config+ 5005)
(defconstant +api-error-unknown+ 5006)
(defconstant +api-error-database-connect+ 5012)
(defconstant +api-error-missing-parameter+ 5013)
(defconstant +api-error-function-not-specified+ 5014)
(defconstant +api-error-not-configured+ 5015)

(defconstant +api-error-update+ 6001)
(defconstant +api-error-favorite+ 6002)
(defconstant +api-error-delete+ 6003)
(defconstant +api-error-save+ 6004)
(defconstant +api-error-clear+ 6005)
