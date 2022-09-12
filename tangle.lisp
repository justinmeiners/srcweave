(in-package :srcweave)

(defun dependency-pairs-from-blocks (block-table)
  "Returns a list of pairs which describe the dependencies
  between blocks in the table. This is a helper for tsort"

  (let ((pairs nil))
    (maphash (lambda (key block)
               (dolist (name (textblock-referenced-titles block))
                 (push (cons (textblock-slug name) key)
                       pairs)))
             block-table)
    pairs))

(defun dependency-count-references (dependency-pairs)
  "count how many times each block is referenced."
  (let ((usages (make-hash-table :test #'equalp)))
    (loop for pair in dependency-pairs do
          (incf (gethash (car pair) usages 0)))
    usages))

(defun topological-sort-dependencies (pairs)
  "Returns a list of block ids in dependency order. Uses UNIX tsort for topological order."
  (let ((s (make-string-output-stream)))
    (loop for pair in pairs do
          (format s "~a ~a~%" (car pair) (cdr pair)))
    (multiple-value-bind (output error status)
        (uiop:run-program (list "tsort")
                          :ignore-error-status t
                          :output :lines
                          :error-output t
                          :input (make-string-input-stream (get-output-stream-string s)))
      (if (= status 0)
          output
          (error 'user-error :format-control "dependency resolution failed")))))

(defun resolve-includes (block-table sorted-id-list)
  "perform inclusion on all blocks in the table"
  (dolist (id sorted-id-list)
    (multiple-value-bind (block present)
        (gethash id block-table)
      (when (not present)
        (error 'user-error
         :format-control "attempting to include unknown block ~s"
         :format-arguments (list id)))
      (setf (gethash id block-table)
            (textblock-include block block-table)))))
 

(defun tangle-output-block (block &optional (stream t))
  (let ((first t))
    (loop for line across (textblock-lines block) do
          (if first
              (setf first nil)
              (write-line "" stream))
          (loop for expr in line do
                (if (stringp expr)
                    (write-string expr stream)
                    (error "block has not been fully resolved ~s" expr)))))

  (when (and *trailing-newline*
             (not (alexandria-2:emptyp (textblock-lines block))))
    (when (not (null (alexandria-2:last-elt (textblock-lines block))))
     (write-line "" stream))))

(defun tangle-build-pathname (title base)
  (assert (uiop:string-prefix-p "/" title))
  (uiop:merge-pathnames* 
    (uiop:ensure-pathname (subseq title 1)) base))

(defun tangle (defs output-dir &key (ignore-dates nil)) 
  (let* ((output-dir (uiop:ensure-directory-pathname output-dir))
         (defs-to-tangle (remove-if-not (lambda (def) (eq (textblockdef-kind def) :CODE)) defs))
         (root-defs (remove-if-not (lambda (def)
                                     (and (eq (textblockdef-operation def) :DEFINE)
                                          (textblockdef-is-file def))
                                     ) defs-to-tangle))
         (block-table (textblockdefs-apply defs-to-tangle))
         (dependencies (dependency-pairs-from-blocks block-table)))
  
    (resolve-includes
      block-table
      (topological-sort-dependencies dependencies))

  
    (loop for def in root-defs do
          (let* ((title (textblockdef-title def))
                 (file-path (tangle-build-pathname title output-dir))
                 (block (gethash (textblock-slug title) block-table)))


            (assert block)
            (if (or ignore-dates
                    (>= (textblock-modify-date block)
                        (file-output-date-safe file-path) ))
                (progn 
                  (format t "writing source: ~a~%" file-path)
                  (ensure-directories-exist file-path) 
                  (with-open-file (s file-path
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :supersede)
                    (tangle-output-block block s)))
                (format t "up to date: ~a~%" file-path))))


    ; Check for warning and errors
    (when (null root-defs)
      (format *error-output* "warning: no file blocks to tangle~%"))

    (let ((reference-counts (dependency-count-references dependencies)))
      (loop for def in root-defs do
        (incf (gethash (textblock-slug (textblockdef-title def)) reference-counts 0)))
      (maphash (lambda (k _)
                 (when (= (gethash k reference-counts 0) 0)
                   (format *error-output* "warning: block ~s was never used.~%" k)))
               block-table)
      )
))


