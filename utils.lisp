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

(defparameter *slug-pattern*
  (ppcre:create-scanner '(:INVERTED-CHAR-CLASS
                          #\_ #\- #\.
                          (:RANGE #\A #\Z)
                          (:RANGE #\a #\z)
                          (:RANGE #\0 #\9))))

(defun string-to-slug (string)
  (string-downcase (ppcre:regex-replace-all *slug-pattern* string "-")))


(define-condition user-error (simple-error) ())

(defun singlep (l)
  (= 1 (length l)))

(defmacro comment (&rest body)
  (declare (ignore body))
  nil)

;; The original idea for this was that I wanted
;; a way to link using a minimal unique prefix.
;;
;; If my section heading is something long,
;; I don't want to have to type out the entire
;; Click [here](@Some long section heading that is easy to typo) to read more!
;;
;; So I was going to use a trie to lookup the Chapter/Section ID given
;; a title prefix. If it ends up being unique, we're good to go. If it's
;; not unique, we can pick one and leave a warning.
;;
;; I haven't yet gotten close to finishing that implementation. So it
;; doesn't make much sense to have this data structure right now.
;; But oh well. Whatever. It works for an MVP of the chapter/section linking stuff.
(defun trie-assoc (key alist)
  (assoc key alist :test #'equal))

(defun trie-put (alist key val)
  (if (singlep key)
      (acons (car key) (acons :terminal val '()) alist)
      (let ((child (trie-assoc (car key) alist)))
        (if child
            (progn
              (rplacd child (trie-put (cdr child) (cdr key) val))
              alist)
            (acons (car key) (trie-put '()  (cdr key) val) alist)))))

(defun trie-get (alist key)
  (if (singlep key)
      (trie-assoc (car key) alist)
      (trie-get (cdr (trie-assoc (car key) alist)) (cdr key))))

(comment
 (trie-put '() '(1 2) 3)
 ; => ((1 (2 (:TERMINAL . 3))))
 (let ((trie (trie-put
              (trie-put
               (trie-put '() '(2) 3)
               '(1) 2)
              '(1 2) 3)))
   trie)
 ; => ((1 (2 (:TERMINAL . 3)) (:TERMINAL . 2)) (2 (:TERMINAL . 3)))
 )
