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

(defun chapter-anchor (index &optional file)
  (format nil "~a#~a" (or file "") (chapter-id index)))

(defun section-anchor (section-index chapter-index &optional file)
  (format nil "~a#~a" (or file "") (section-id section-index chapter-index)))
 
(defun textblockdef-create-toc (defs)
  (let ((chapter nil)
        (all-chapters nil))

    (dolist (def defs)
      (loop for line across (textblock-lines (textblockdef-block def)) do
            (loop for expr in line do
                  (when (commandp expr)
                    (case (first expr)
                      (:C
                       (when chapter 
                         (push (nreverse chapter) all-chapters)
                         (setf chapter nil))
                       (push :C chapter)
                       (push (second expr) chapter))
                      (:S
                       (push (list :S (second expr)) chapter)))))))
    (push (nreverse chapter) all-chapters)
    all-chapters
    (nreverse all-chapters)))

 
(defun create-global-toc (file-def-pairs)
  (mapcar (lambda (pair)
            (cons :FILE 
                  (cons (file-namestring (car pair))
                        (textblockdef-create-toc (cdr pair)))))
          file-def-pairs))

(defun create-global-toc-linkmap (toc)
  "Creates a map of chapter/section names to the href when woven into a book file.html#s0:1.
The current implementation has the downside that subsequent chapter/section names
will overwrite antecedent chapter/section entries in the map. Users will just have
to be aware of this and make their chapter/section names unique."
  (do ((linkmap (make-hash-table :test 'equal))
       (file (car toc) (car (cdr toc)))
       (toc toc (cdr toc)))
      ((null file) linkmap)
    (do ((chapters (cddr file) (cdr chapters))
         (chapter (caddr file) (car (cdr chapters)))
         (chapter-counter 0 (incf chapter-counter)))
        ((null chapter))
      (let ((link (format nil "~a#c~a" (lit-page-filename (cadr file)) chapter-counter)))
        (setf (gethash (cadr (subseq chapter 0 2)) linkmap) link)
        (do ((sections (cddr chapter) (cdr sections))
             (section (caddr chapter) (car (cdr sections)))
             (section-counter 0 (incf section-counter)))
            ((null section))
          (let ((link (format nil "~a#s~a:~a"
                              (lit-page-filename (cadr file))
                              chapter-counter
                              section-counter)))
            (setf (gethash (cadr section) linkmap) link)))))))

(comment
 (let* ((file-defs (parse-lit-files '("dev/dev.lit" "dev/scratch.lit")))
        (linkmap (create-global-toc-linkmap (create-global-toc file-defs)))
        (res nil))
   (maphash
    (lambda (k v)
      (setf res (cons (list k v) res)))
    linkmap)
   res)

 ; => (("Scratch" "scratch.html#s0:0") ("My scratch lit file" "scratch.html#c0")
 ;     ("Section 2" "dev.html#c1") ("Foobazs" "dev.html#s0:1")
 ;     ("Foobar" "dev.html#s0:0") ("My test lit file" "dev.html#c0"))
 )

(defun weave-toc-section (name file chapter-counter section-counter)
  (format t "<li><a href=\"~a\">~a</a></li>"
          (section-anchor
            section-counter
            chapter-counter
            file)
          name))

(defun weave-toc-chapter (name sections file chapter-counter)
  (when name
    (format t "<li><a href=\"~a\">~a</a>"
            (chapter-anchor chapter-counter file)
            name))

  (format t "<ol>")
  (mapnil-indexed (lambda (section i)
                    (weave-toc-section
                      (second section)
                      file
                      chapter-counter
                      i))
                  sections)
  (format t "</ol>")
  (when name
    (format t "</li>")))

(defun toc-count-chapters (toc)
  (reduce (lambda (total file)
            (+ total (length (cddr file))))
          toc
          :initial-value 0))

(defun weave-toc (toc current-file)
  (let ((show-chapters (> (toc-count-chapters toc) 1))
        (chapter-counter -1))
    (when show-chapters
      (write-string "<ol>"))

    (dolist (file-command toc)
          (dolist (chapter (cddr file-command))
            (incf chapter-counter)
            (weave-toc-chapter (if show-chapters (second chapter) nil)
                               (cddr chapter)
                               (if (equal current-file
                                          (second file-command))
                                   nil
                                   (concatenate 'string
                                                (uiop:split-name-type (second file-command))
                                                ".html"))
                               chapter-counter)))
    (when show-chapters
      (write-string "</ol>"))))

