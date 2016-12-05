(defpackage #:shopping-lisp.db
  (:use #:cl #:mito #:defclass-std #:alexandria)
  (:shadow #:count)
  (:export #:connect

           #:upsert
           #:clear-db
           #:delete-by-name
           #:delete-by-list
           #:save-list
           #:update-by-name

           #:shopping-item))
(in-package #:shopping-lisp.db)

(defclass/std shopping-item ()
  ((name :col-type :text)
   (count :col-type :integer))
  (:metaclass dao-table-class)
  (:unique-key item))

(defmethod jojo:%to-json ((item shopping-item))
  (jojo:with-object
    (jojo:write-key-value "itemTitle" (name item))
    (jojo:write-key-value "itemCount" (princ-to-string (count item)))
    (jojo:write-key-value "checked" :false)))

(defun connect ()
  (apply #'mito:connect-toplevel (nest:config :database-spec))
  (ensure-table-exists 'shopping-item))

(defun clear-db ()
  (mito:execute-sql (sxql:delete-from :shopping_item)))

(defun upsert (item)
  (if-let ((db-item (find-dao 'shopping-item :name (getf item :name))))
    (progn
      (setf (count db-item) (getf item :count))
      (save-dao db-item))
    (apply #'create-dao 'shopping-item item)))

(defun delete-by-name (name)
  (mito:execute-sql
   (sxql:delete-from :shopping_item
     (sxql:where (:= :name name)))))

(defun delete-by-list (names)
  (mito:execute-sql
   (sxql:delete-from :shopping_item
     (sxql:where (:in :name names)))))

(defun save-list (items)
  (map nil (lambda (i)
             (create-dao 'shopping-item
                         :item (getf i :item)
                         :count (getf i :count)))
       items))

(defun update-by-name (name count)
  (if-let ((item (find-dao 'shopping-item :name name)))
    (progn
      (setf (count item) count)
      (save-dao item))
    nil))
