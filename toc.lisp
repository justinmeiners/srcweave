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

(defun create-global-toc (file-def-pairs)
  (let ((chapter nil)
        (all-chapters nil))
    (dolist (file-pair file-def-pairs)
      (let ((filename (file-namestring (car file-pair)))
            (defs (cdr file-pair)))
        (dolist (def defs)
          (loop for line across (textblock-lines (textblockdef-block def)) do
            (loop for expr in line do
              (when (commandp expr)
                (case (first expr)
                  (:C
                   (when chapter
                     (push (nreverse chapter) all-chapters)
                     (setf chapter nil))
                   ; chapters are built in reverse
                   (setf chapter (nreverse (list :C (second expr) filename))))
                  (:S
                    (push (list :S (second expr) filename)
                          chapter)))))))))
    (push (nreverse chapter) all-chapters)
    (nreverse all-chapters)))

(defun ref-to-weave-filename (filename)
  (concatenate 'string
               (uiop:split-name-type filename)
               ".html"))

(defun weave-toc-filename (filename toc-filename)
  "don't append the filename at the beginning of the link if the TOC is on the same page as the content."
  (if (equal filename toc-filename)
      nil
      (ref-to-weave-filename filename)))

(defun weave-toc-section (section toc-filename chapter-counter section-counter)
  (destructuring-bind (_ name filename . contents) section
    (format t "<li><a href=\"~a\">~a</a></li>~%"
            (section-anchor
             section-counter
             chapter-counter
             (weave-toc-filename filename toc-filename))
            name)))

(defun weave-toc-chapter (chapter show-chapters toc-filename chapter-counter)
  (destructuring-bind (_ name filename . sections) chapter
    (when show-chapters
      (format t "<li><a href=\"~a\">~a</a>~%"
              (chapter-anchor chapter-counter (weave-toc-filename filename toc-filename))
              name))

    (format t "<ol>~%")
    (mapnil-indexed (lambda (section i)
                      (weave-toc-section
                       section
                       toc-filename
                       chapter-counter
                       i
                       ))
                    sections)
    (format t "</ol>~%")
    (when show-chapters
      (format t "</li>~%"))))

(defun weave-toc (toc toc-filename)
  (let ((show-chapters (> (length toc) 1))
        (chapter-counter -1))
    (when show-chapters
      (format t "<ol>~%"))

    (dolist (chapter toc)
      (incf chapter-counter)
      (weave-toc-chapter chapter
                         show-chapters
                         toc-filename
                         chapter-counter))
    (when show-chapters
      (format t "</ol>"))))
