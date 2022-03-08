; Copyright (c) 2022 Justin Meiners
; 
; This program is free software: you can redistribute it and/or modify  
; it under the terms of the GNU General Public License as published by  
; the Free Software Foundation, version 2.
;
; This program is distributed in the hope that it will be useful, but 
; WITHOUT ANY WARRANTY; without even the implied warranty of 
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License 
; along with this program. If not, see <http://www.gnu.org/licenses/>.

(in-package :srcweave)

; Design: DSL for manpulating textblocks.
; This means well defined operations that are ideally closed.

; For example:
; - append two blocks and get a block
; - include references in a block to get a new block

(defstruct textblock
  "stores a vector of lines. Each line is a list of strings or other commands."
  (lines (make-array 16 :fill-pointer 0 :adjustable t) :type array)
  (modify-date 0 :type integer))

(defun textblock-referenced-titles (block)
  "returns all titles which this block references for inclusion"
  (let ((list nil))
    (loop for line across (textblock-lines block) do
          (loop for expr in line do
                (when (and (commandp expr) (eq (first expr) :INCLUDE))
                  (push (second expr) list))))
    list))

(defun textblock-slug (title) (string-to-slug title))
 
(defun leading-whitespace (code-line)
  (if (stringp (car code-line))
      (multiple-value-bind (match groups)
          (ppcre:scan-to-strings "(\\s*)" (car code-line))
        (assert match)
        (aref groups 0)) ""))

(defun textblock-concat (a b)
  "concatenate the text of two blocks"
  (make-textblock
    :modify-date (max (textblock-modify-date a)
                      (textblock-modify-date b))
    :lines (concatenate 'vector
                        (textblock-lines a)
                        (textblock-lines b))))
 
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
  (let ((prefix '()))
    (loop for expr in line do
          (cond ((stringp expr) (setf prefix (append prefix (list expr))))
                ((commandp expr) 
                 (case (first expr)
                   (:INCLUDE
                     (multiple-value-bind (other present)
                         (gethash (textblock-slug (second expr)) block-table)
                       (if present
                           (setf prefix (include-lines
                                          other
                                          prefix
                                          (leading-whitespace line) output))
                           (error 'user-error
                                  :format-control "cannot find block to include: ~s"
                                  :format-arguments (list (second expr))))))
                   (otherwise (error "unknown code command ~S" (first expr)))))
                (t (error "unknown structure"))))
    (vector-push-extend prefix output)))

(defun textblock-include (root block-table)
  "form a new block by including the contents of all immediate dependencies (nonrecursive)."
  (let* ((titles (textblock-referenced-titles root))
         (dependencies (mapcar (lambda (title)
                                 (gethash (textblock-slug title) block-table)) titles))
         (output (make-array 16 :fill-pointer 0 :adjustable t)))

    (loop for line across (textblock-lines root) do
          (include-helper line output block-table))

    (make-textblock :modify-date (reduce #'max
                                         (mapcar #'textblock-modify-date dependencies)
                                         :initial-value (textblock-modify-date root))
                    :lines output)))

(defun textblock-find-title (block)
  (find-map (lambda (line)
              (find-map (lambda (expr)
                         (when (and (commandp expr) (eq (first expr) :TITLE))
                           (second expr)))
                        line))
            (textblock-lines block)))

(defun table-dependency-pairs (block-table)
  "helper for tsort"
  (let ((pairs nil))
    (maphash (lambda (key block)
               (dolist (name (textblock-referenced-titles block))
                 (push (cons (textblock-slug name) key) pairs)))
             block-table)
    pairs))

(defun textblock-include-order (block-table)
  "Returns a list of block ids in dependency order. Uses UNIX tsort for topological order."
  (let ((pairs (table-dependency-pairs block-table))
        (s (make-string-output-stream)))

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

(defun textblock-resolve-includes (block-table sorted-id-list)
  "perform inclusion on all blocks in the table"
  (dolist (id sorted-id-list)
    (multiple-value-bind (block present)
        (gethash id block-table)
      (assert present)
      (setf (gethash id block-table)
            (textblock-include block block-table)))))

(defstruct textblockdef
  "Information about where blocks are defined and config options."
  (title nil :type (or null string))
  (block (make-textblock) :type textblock)
  (kind :PROSE :type symbol)
  (line-number 0 :type integer)
  (file nil :type (or null string))
  (operation :DEFINE :type symbol)
  (modifiers nil :type (or null list))
  (language "text" :type string))

(defun commandp (expr)
  (and (consp expr) (symbolp (car expr))))

(defun textblockdef-is-file (def)
  (find :FILE (textblockdef-modifiers def)))

(defun textblockdef-weavable (def)
  (not (find :NO-WEAVE (textblockdef-modifiers def))))

(defun textblockdef-id (def)
  (format nil "b~a:~a"
          (string-to-slug (textblockdef-title def))
          (textblockdef-line-number def)))

(defun textblockdef-title-slug (def)
  (textblock-slug (textblockdef-title def)))

(defun textblockdef-create-table (defs)
  "maps definition ids to definitions"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (def defs)
      (setf (gethash (textblockdef-id def) table) def))
    table))

(defun textblockdef-create-title-to-initial-table (defs)
  "maps titles to their first definition id"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (def defs)
      (when (not (gethash (textblockdef-title-slug def) table))
        (setf (gethash (textblockdef-title-slug def) table)
              (textblockdef-id def))))
    table))

(defun textblockdef-create-use-table (defs)
  "maps titles to the definition ids that use them"
  (let ((table (make-hash-table :test #'equal)))
    (dolist (def defs)
      (let ((id (textblockdef-id def)))
        (dolist (dependency (textblock-referenced-titles (textblockdef-block def)))
          (push id
                (gethash (textblock-slug dependency) table)))))
    (maphash (lambda (key val)
               (setf (gethash key table) (reverse val))) table)
    table))

(defun textblockdefs-apply (defs)
  "construct a table of blocks by evaluating combination operations"
  (let ((block-table (make-hash-table :test #'equal)))
    (loop for def in defs do
          (let ((block (textblockdef-block def))
                 (title (textblockdef-title def))
                 (slug (textblock-slug (textblockdef-title def)))
                 (line  (textblockdef-line-number def)))
            (multiple-value-bind (current present) (gethash slug block-table)
              (setf (gethash slug block-table)
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



