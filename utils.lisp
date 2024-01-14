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

(defun mapcar-indexed-helper (f list i result)
  (if (null list)
      result
      (mapcar-indexed-helper
        f
        (cdr list)
        (+ i 1)
        (cons (funcall f (car list) i) result))))

(defun mapcar-indexed (f list)
  (nreverse (mapcar-indexed-helper f list 0 nil)))

(defun mapnil-indexed (f list)
  (do ((i 0 (+ i 1))
       (part list (cdr part)))
      ((null part) nil)
    (funcall f (car part) i)))

(defun find-map (f sequence)
  (let ((result nil))
    (find-if (lambda (x)
               (setf result (funcall f x))
               result) sequence)
    result))

(defun write-separated-list (list separator &optional (stream t))
  (when (not (null list))
    (write-string (car list) stream)
    (loop for x in (cdr list) do
          (write-char separator stream)
          (write-string x stream))))

(defun join-strings (list &key (separator #\space))
  (with-output-to-string (stream)
    (write-separated-list list separator stream)))

(defun file-output-date-safe (file-path &optional (default 0))
  (handler-case (or (file-write-date file-path) default)
    (error (c) default)))

(defun split-whitespace (string)
  (ppcre:split '(:GREEDY-REPETITION 1 nil :WHITESPACE-CHAR-CLASS)
               string))

(defun string-nullp (string)
  (or (null string)
      (= (length (string-trim " " string)) 0)))

(defun string-is-whitespace (string)
  (ppcre:scan '(:SEQUENCE
                :START-ANCHOR
                (:GREEDY-REPETITION 0 nil :WHITESPACE-CHAR-CLASS)
                :END-ANCHOR)
              string))

(defun strip-line (line)
    (if (not line) line
        (uiop:stripln line)))

(define-condition user-error (simple-error) ())

(defun group-into-table (objs &key (test #'eql) (key #'identity))
  "return a hash map of of lists where all objects in a list are equal."
  (let ((groups (make-hash-table :test test)))
    (map nil (lambda (x)
               (push x (gethash (funcall key x) groups nil))) objs)
    groups))

(defun disambiguate-ids-into (objects id-table &key (key nil) (combine))
  "Given list of object ids that are possibly not unique, we can identify duplicates and refine them until they are unique.
   This is done by finding equivalent groups and calling `combine` to create a new key.
   One way to use this is to attach a suffix containing a character not in the original id set."

                                        ; group all objects that are equivalent
  (let ((groups (group-into-table objects :test #'equal :key key)))
    (maphash (lambda (key list)
               (mapnil-indexed (lambda (object i)
                                 (setf (gethash object id-table)
                                       (if (= i 0) key
                                           (funcall combine key i))))
                               (reverse list))) groups)))

(defun edge-from (edge) (car edge))
(defun edge-to (edge) (cdr edge))

(defun make-indegree-table (edges &key (test #'eq))
  "count how many times each block is referenced."
  (let ((indegree (make-hash-table :test test)))
    ; this will get most of the vertices:
    (dolist (e edges)
      (setf (gethash (edge-to e) indegree) 0))
    ; this will get the rest
    (dolist (e edges)
      (incf (gethash (edge-from e) indegree 0)))
    indegree))

(defun top-sort (edges &key (test #'eq))
  "given a list of edges: (from . to)
   find a topological ordering of the vertices implied by the edges."
  (let ((from (make-indegree-table edges :test test))
        (to (make-hash-table :test test))
        (will-visit nil)
        (counter 0)
        (result))

    (dolist (e edges)
      (push (edge-from e) (gethash (edge-to e) to)))

    ; prepare starting vertices
    (maphash (lambda (v count)
               (when (= count 0)
                 (push v will-visit))) from)

    (loop while will-visit do
      (let ((remove (pop will-visit)))
        (incf counter)
        (push remove result)
        (loop for v in (gethash remove to) do
          (when (= (decf (gethash v from)) 0)
            (push v will-visit)))))

    (if (= (hash-table-count from) counter)
        ; every vertex should be visited once
        result
        (error 'user-error :format-control "top-sort sort failed. cycle detected."))))

(defun filename-replace-extension (filename replacement)
  (concatenate 'string (uiop:split-name-type filename) replacement))
