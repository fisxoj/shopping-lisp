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
  (:auto-pk nil)
  (:primary-key name)
  (:documentation "Shopping list item definition.  `name` is a unique key and is used as the primary key by the app."))

(defmethod jojo:%to-json ((item shopping-item))
  ;; Serializes items to the format the android app expects

  (jojo:with-object
    (jojo:write-key-value "itemTitle" (name item))
    (jojo:write-key-value "itemCount" (princ-to-string (count item)))
    (jojo:write-key-value "checked" :false)))

(defun connect ()
  "Connects to the database with the cl-dbi connection spec in the current configuration, then creates tables."

  (apply #'mito:connect-toplevel (nest:config :database-spec))
  (ensure-table-exists 'shopping-item))

(defun clear-db ()
  "Delete everything from the db."

  (mito:execute-sql (sxql:delete-from :shopping_item)))

(defun upsert (item)
  "Upsert an item keyed by name."

  (if-let ((db-item (find-dao 'shopping-item :name (getf item :name))))
    (progn
      (setf (count db-item) (getf item :count))
      (save-dao db-item))
    (apply #'create-dao 'shopping-item item)))

(defun delete-by-name (name)
  "Deletes a single record by name."

  (mito:execute-sql
   (sxql:delete-from :shopping_item
     (sxql:where (:= :name name)))))

(defun delete-by-list (names)
  "Expects a list of strings that are item names to delete from the db.  Constructs an efficient SQL query for multiple deletion."

  (mito:execute-sql
   (sxql:delete-from :shopping_item
     (sxql:where (:in :name names)))))

(defun save-list (items)
  "Expects validated plists like `(:name ... :count ...)` and stores them in the db."

  (map nil (lambda (i)
             (apply #'create-dao
                    'shopping-item
                    i))
       items))

(defun update-by-name (name count)
  "Finds item by `name` and updates `count`."

  (if-let ((item (find-dao 'shopping-item :name name)))
    (progn
      (setf (count item) count)
      (save-dao item))
    nil))
