(in-package :cl-user)
(defpackage shopping-lisp
  (:use :cl :nest :alexandria :shopping-lisp.constants)
  (:export #:run))
(in-package :shopping-lisp)

(defapp app
    :middlewares (lack.middleware.accesslog:*lack-middleware-accesslog*))

(defun api-response (&key type content items status)
  "Generates a JSON object matching the shopping list api response style, which always has a 'type' field, which is a status code as in shopping-lisp.constants, and optionally 'items' (when returning database items) and 'content' if there is an explanatory string."

  (let ((resp (list :|type| type)))
    (when content
      (setf resp (nconc resp (list :|content| content))))
    (when items
      (setf resp (nconc resp (list :|items| items))))
    (respond-json resp
                  :status (or status 200)
                  :headers `(:|ShoLiBackendVersion| ,+backend-version+))))

(defmacro validated (&body body)
  `(handler-case
       (progn ,@body)
     (v:<validation-error> (e)
       (declare (ignore e))
       (api-response :type +api-error-unknown+
                     :content "Invalid parameter"))))

(defmacro with-password-check (&body body)
  `(if (cl-pass:check-password (body-parameter :auth "") (config :auth-hash))
       (progn ,@body)
       (api-response :type +api-error-403+
                     :content "Authentication failed.")))

(defun validate-item (item)
  "Validate an item as accepted by 'save' and 'saveMultiple'"

  (list :name (v:str (getf item :|itemTitle|) :min-length 1 :max-length 256)
        :count (v:int (getf item :|itemCount|))))

(defun api-handler ()
  ;; (format t "~&Body parameters: ~a~%" (nest.request::keywordize (lack.request:request-body-parameters (nest.request::ensure-request))))
  (with-password-check
    (switch ((body-parameter :function "") :test #'string-equal)
      ("listall"
       (let ((items (mito:select-dao 'shopping-lisp.db:shopping-item)))
         (if items
             (api-response :type +api-success-list+ :items items)
             (api-response :type +api-success-list-empty+))))

       ("save"
        (validated
          (let ((item (validate-item (list :|itemTitle| (body-parameter :item)
                                           :|itemCount| (body-parameter :count)))))
            (shopping-lisp.db:upsert item)
            (api-response :type +api-success-save+
                          :content (format nil "~a saved." (getf item :name))))))

       ("saveMultiple"
        (validated
          (let* ((items (mapcar #'validate-item (v:list (body-parameter :jsonArray)))))
            (shopping-lisp.db:save-list items)
            (api-response :type +api-success-save+
                          :content "Multiple items saved"))))

       ("deleteMultiple"
        (validated
          (let* ((items (v:list (body-parameter :jsonArray)))
                 (names (mapcar (lambda (i) (getf i :|itemTitle|)) items)))
            (shopping-lisp.db:delete-by-list names))
          (api-response :type +API-SUCCESS-DELETE+
                        :content "Multiple items deleted")))

       ("update"
        (with-body-params (item count)
          (validated
            (let ((item-name (v:str item :min-length 1 :max-length 256))
                  (item-count (v:int count)))
              (shopping-lisp.db:update-by-name item-name item-count)))))

       ("delete"
        (with-body-params (item)
          (validated
            (let ((item-name (v:str item :min-length 1 :max-length 256)))
              (shopping-lisp.db:delete-by-name item-name)))))

       ("clear"
        (shopping-lisp.db:clear-db)
        (api-response :type +api-success-clear+
                      :content "List cleared"))

       (t
           (api-response :type +api-error-missing-function+
                         :content "Unknown function")))))

(defroute app ("/api.php" :post)
  (api-handler))

(defroute app ("/" :post)
  (api-handler))


(defmethod nest:start :before ((app app) &rest args)
  (declare (ignore args))
  (shopping-lisp.db:connect))


(defun run ()
  (start app
         :debug (not (production-p))
         :address "0.0.0.0"))
