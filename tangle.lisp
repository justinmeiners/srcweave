(in-package :srcweave)

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

  (when *trailing-newline*
    (when (not (null (alexandria-2:last-elt (textblock-lines block))))
     (write-line "" stream))))

(defun tangle (defs output-dir &key (ignore-dates nil)) 
  (let* ((code-defs (remove-if-not (lambda (def) (eq (textblockdef-kind def) :CODE)) defs))
         (root-defs (remove-if-not (lambda (def)
                                     (and (eq (textblockdef-operation def) :DEFINE)
                                          (textblockdef-is-file def))
                                     ) defs))
         (block-table (textblockdefs-apply code-defs)))

   
    (textblock-resolve-includes block-table (textblock-include-order block-table))

    (loop for def in root-defs do
          (let* ((title (textblockdef-title def))
                 (file-path (merge-pathnames (pathname title) output-dir))
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
                (format t "up to date: ~a~%" file-path))))))

 
