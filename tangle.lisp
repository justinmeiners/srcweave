(in-package :srcweave)

; Design: DSL for manpulating textblocks.
; This means well defined operations that are ideally closed.
; For example:
; - append two blocks and get a block
; - include references in a block to get a new block

(defun textblock-concat (a b)
  "concatenate the text of two blocks. The modification date is the most recent of the two dates."
  (make-textblock
    :modify-date (max (textblock-modify-date a)
                      (textblock-modify-date b))
    :lines (concatenate 'vector
                        (textblock-lines a)
                        (textblock-lines b))))

(defun leading-whitespace (code-line)
  (if (stringp (car code-line))
      (multiple-value-bind (match groups)
          (ppcre:scan-to-strings "(\\s*)" (car code-line))
        (assert match)
        (aref groups 0)) ""))

(defun include-lines (block prefix whitespace output)
  (let* ((src (textblock-lines block))
         (n (length src)))
    (cond ((= n 0) prefix)
          ((= n 1) (append prefix (aref src 0)))
          (t
           (vector-push-extend (append prefix (aref src 0)) output)
           (loop for i from 1 below (- (length src) 1) do
                 (vector-push-extend (cons whitespace (aref src i)) output))
           (cons whitespace (aref src (- n 1)))))))

(defun include-helper (line output block-table)
  ; Handling white space properly in block inclusion is tricky.
  ; Suppose you have something like this:
  ; int main() {
  ;     @{body}
  ; }
  ; We want all the lines from body to be indented at same level.
  ; So we need to record the whitespace prefix and prepend that to each line.
  ; See the tests for additional examples.

  ; What should the following do if body has multiple lines?
  ; int main() { @{body} }

  (let ((prefix '()))
    (loop for expr in line do
          (cond ((stringp expr) (setf prefix (append prefix (list expr))))
                ((commandp expr)
                 (case (first expr)
                   (:INCLUDE
                     (multiple-value-bind (other-block present)
                         (gethash (second expr) block-table)
                       (if present
                           (setf prefix (include-lines
                                          other-block
                                          prefix
                                          (leading-whitespace line) output))
                           (warn "attempting to include unknown block: ~s" (second expr)))))
                   (otherwise (error "unknown code command ~S" (first expr)))))
                (t (error "unknown structure"))))
    (vector-push-extend prefix output)))

(defun textblock-include (root block-table)
  "form a new block by including the contents of all immediate dependencies (nonrecursive)."
  (let ((output (make-array 16 :fill-pointer 0 :adjustable t)))
    (loop for line across (textblock-lines root) do
          (include-helper line output block-table))

    (let* ((syms (textblock-references root))
           (dependencies (remove-if #'null (mapcar (lambda (sym)
                                                     (gethash sym block-table))
                                                   syms))))
      (make-textblock :modify-date (reduce #'max
                                           (mapcar #'textblock-modify-date dependencies)
                                           :initial-value (textblock-modify-date root))
                      :lines output))))


(defun textblockdefs-apply (defs)
  "construct a table of blocks by evaluating the block  operations (include, concat, etc)."
  (let ((block-table (make-hash-table)))
    (loop for def in defs do
          (let ((block (textblockdef-block def))
                 (title (textblockdef-title def))
                 (sym (textblockdef-title-sym def))
                 (line  (textblockdef-line-number def)))
            (multiple-value-bind (current present) (gethash sym block-table)
              (setf (gethash sym block-table)
                    (case (textblockdef-operation def)
                      (:DEFINE (if (not present)
                                  block
                                  (error 'user-error
                                         :format-control "block already defined ~S ~S"
                                         :format-arguments (list line title))))
                      (:REDEFINE (if present
                                    block
                                    (error
                                      'user-error
                                      :format-control "redefine before definition ~S ~S"
                                      :format-arguments (list line title))))
                      (:APPEND (if present
                                  (textblock-concat current block)
                                  (error
                                    'user-error
                                    :format-control "appending to undefined block ~S ~S"
                                    :format-arguments (list line title))))
                      )))))
    block-table))


(defun dependency-pairs-from-blocks (block-table)
  "Returns a list of pairs which describe the dependencies
  between blocks in the table. This is a helper for tsort"
  (let ((pairs nil))
    (maphash (lambda (key block)
               (dolist (sym (textblock-references block))
                 (push (cons sym key) pairs)))
             block-table)
    pairs))

; END BLOCK OPS

(defun resolve-includes (block-table ordered-id-list)
  "perform inclusion on all blocks in the table.
  sorted-id-list should be topolgically sorted"

  (dolist (id ordered-id-list)
    (multiple-value-bind (block present)
        (gethash id block-table)

      ; If a referenced block id doesn't exist in the table.
      ; just ignore it for now. The inclusion code will warn
      ; if it's a problem.
      (when present
        (setf (gethash id block-table)
              (textblock-include block block-table))))))

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
             (not (alexandria-2:emptyp (textblock-lines block)))
             (not (null (alexandria-2:last-elt (textblock-lines block)))))
     (write-line "" stream)))

(defun tangle-build-pathname (title base)
  (assert (uiop:string-prefix-p "/" title))
  (uiop:merge-pathnames*
    (uiop:ensure-pathname (subseq title 1)) base))

(defun tangle (defs output-dir &key (ignore-dates nil))
  (let* ((output-dir (uiop:ensure-directory-pathname output-dir))
         (defs-to-tangle (remove-if-not (lambda (def) (and (eq (textblockdef-kind def) :CODE)
                                                           (textblockdef-tanglable def))) defs))
         (root-defs (remove-if-not (lambda (def)
                                     (and (eq (textblockdef-operation def) :DEFINE)
                                          (textblockdef-is-file def))
                                     ) defs-to-tangle))
         (block-table (textblockdefs-apply defs-to-tangle))
         (dependencies (dependency-pairs-from-blocks block-table)))

    (resolve-includes
      block-table
      (top-sort dependencies))

    (loop for def in root-defs do
          (let* ((sym (textblockdef-title-sym def))
                 (file-path (tangle-build-pathname (textblockdef-title def) output-dir))
                 (block (gethash sym block-table)))

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
      (warn "no file blocks to tangle"))

    (let ((reference-counts (make-indegree-table dependencies)))
      (loop for def in root-defs do
        (incf (gethash (textblockdef-title-sym def) reference-counts 0)))

      (maphash (lambda (sym _)
                 (when (= (gethash sym reference-counts 0) 0)
                   (warn "block ~s was never used." (symbol-name sym))))
               block-table))))
