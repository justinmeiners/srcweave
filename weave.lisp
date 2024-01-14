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

; Weave: Make readable documentation from .lit file.
; We have chosen HTML for it's support for many media types and readability
; on many devices.

; DESIGN
; One-to-one correspondence between .lit and .html files.
; The body of an .html file should look the same regardless of whether the .lit is in another context (like a book).

(defstruct weaver
  (use-table (make-hash-table) :type hash-table)
  (initial-def-table (make-hash-table) :type hash-table)
  (anchors (make-hash-table) :type hash-table)
  (toc nil :type list)
  (title nil :type (or string null))
  (filename nil :type (or string null))
  (code-type-table (make-hash-table :test #'equal) :type hash-table)
  (used-extensions nil :type list)
  (used-math nil :type boolean))

(defun make-weaver-default (file-defs)
  (let ((all-defs (alexandria-2:mappend #'cdr file-defs)))
    (make-weaver
      :use-table (make-block-use-table (remove-if-not
                                        (lambda (def) (eq (textblockdef-kind def) :CODE))
                                        all-defs))
      :initial-def-table (make-title-to-first-def-table all-defs)
      :anchors (make-anchor-table file-defs)
      :toc (make-textheading-toc file-defs)
      :code-type-table (codetable-create))))


(defun html-file-anchor (block-id &optional file)
  (format nil "~a#~a"  (or file "") block-id))

(defun textblockdef-weave-file (def)
  ".lit -> .html"
  (filename-replace-extension (textblockdef-file def) ".html"))

(defun filename-if-different (filename current)
  "don't append the filename at the beginning of the anchor if it is on the same page as the content."
  (if (equal filename current) nil filename))

(defun weave-anchor-for-def (def weaver)
  (html-file-anchor (gethash def (weaver-anchors weaver))
                    (filename-if-different (textblockdef-weave-file def)
                                           (weaver-filename weaver))))

(defun textheading-weave-file (heading)
  ".lit -> .html"
  (filename-replace-extension (textheading-file heading) ".html"))

(defun weave-anchor-for-heading (heading weaver)
  (html-file-anchor (gethash heading (weaver-anchors weaver))
                    (filename-if-different (textheading-weave-file heading)
                                           (weaver-filename weaver))))


(defun format-include (title code-style)
  (if code-style (format nil "@{~a}" title) title))

(defun weave-include-unknown (sym  error code-style weaver)
  (format t "<em class=\"block-link nocode\" title=\"~a\">" error)
  (write-string (format-include (symbol-name sym) code-style))
  (write-string "</em>"))

(defun weave-include-exists (other code-style weaver)
  (format t "<em class=\"block-link nocode\"~a>"
          ; create hover text which tells the user which file the block is in.
          (if (equal (weaver-filename weaver) (textblockdef-weave-file other))
              ""
              (format nil " title=\"~a\"" (textblockdef-file other))))
  (format t "<a href=\"~a\">" (weave-anchor-for-def other weaver))
  (write-string (format-include (textblockdef-title other) code-style))
  (write-string "</a>")
  (write-string "</em>"))


(defun weave-include (sym code-style weaver)
   ; find the first block that uses that title
  (let ((other
          (gethash sym
                   (weaver-initial-def-table weaver))))

    (cond ((not other)
           (weave-include-unknown sym "undefined block" code-style weaver))
          ((not (textblockdef-weavable other))
           (weave-include-unknown sym "block defined, but not weavable." code-style weaver))
          (t (weave-include-exists other code-style weaver)))))

(defparameter *html-replace-list*
  '((#\& . "&amp;") (#\< . "&lt;") (#\> . "&gt;")))

(defun escape-html (string)
  "replace invalid HTML characters with their escaped versions."
  (reduce (lambda (string replace-pair)
            (ppcre:regex-replace-all (car replace-pair) string (cdr replace-pair)))
          *html-replace-list*
          :initial-value string))

(defun weave-code-line (weaver line def)
  (loop for expr in line do
        (cond ((stringp expr) (write-string (escape-html expr)))
              ((commandp expr)
               (case (first expr)
                 (:INCLUDE (weave-include
                            (second expr)
                            t
                            weaver))
                 (otherwise
                   ; this shouldn't happen.
                   ; This relates to "block has not been fully resolved" in the tangle.
                   (error "unknown code command ~S" expr))))
              (t (error "internal error. unknown structure ~S" expr)))))

(defun usage-tooltip (def other)
  "generate title attribute contents"
  (if (equal (textblockdef-file def)
             (textblockdef-file other))
      (textblockdef-title other)
      (format nil "~a. ~a"
              (textblockdef-title other)
              (textblockdef-file other))))

(defun weave-uses (weaver def)
  (let ((uses (gethash (textblockdef-title-sym def) (weaver-use-table weaver))))
    (when (not (null uses))
      (write-string "<p class=\"block-usages\"><small>Used by ")
      (mapnil-indexed (lambda (other i)
                        (if (textblockdef-weavable other)
                            (format t "<a href=\"~a\" title=\"~a\">~a</a> "
                                    (weave-anchor-for-def other weaver)
                                    (usage-tooltip def other)
                                    (+ i 1))
                            (format t "<span title=\"~a\">~a</span> "
                                    (usage-tooltip def other)
                                    (+ i 1)))) uses)
      (write-string "</small></p>"))))

(defun language-to-class (language)
  (if (equal language "text") "" language))

(defun operation-string (op)
  (case op
    (:DEFINE nil)
    (:APPEND "+=")
    (:REDEFINE ":=")
    (otherwise (string op))))

(defun weave-operation (weaver def)
  (alexandria-2:when-let ((symbol (operation-string (textblockdef-operation def))))
  (format t " <a href=\"~a\">~a</a>"
          (weave-anchor-for-def
             (gethash (textblockdef-title-sym def) (weaver-initial-def-table weaver))
             weaver)
          symbol)))

(defun weave-codedef (weaver def)
  ; record extensions
  (when (textblockdef-is-file def)
    (alexandria-2:when-let ((extension (pathname-type (textblockdef-title def))))
                           (push extension (weaver-used-extensions weaver))))

  (write-line "<div class=\"code-block\">")

  ; write header
  (let* ((title (textblockdef-title def)))

    (write-line "<span class=\"block-header\">")
    (format t "<strong class=\"block-title\"><em><a id=\"~a\" href=\"~a\">~a</a></em></strong>"
            (gethash def (weaver-anchors weaver))
            (weave-anchor-for-def def weaver)
            title)
    (weave-operation weaver def)
    (write-line "</span>"))

  ; write body
  (let* ((block (textblockdef-block def))
         (lines (textblock-lines block)))
    (format t "<pre class=\"prettyprint\"><code class=\"~a\">"
            (language-to-class (textblockdef-language def)))

    (alexandria-2:if-let ((left (position-if-not #'null lines))
                         (right (position-if-not #'null lines :from-end t)))
                        ; trim blank lines from start and end
                        (loop for i from left to right do
                              (weave-code-line weaver (aref lines i) def)
                              (write-line ""))
                        (warn "weaving empty block ~s" (textblockdef-title def)))

    (write-line "</code></pre>"))
    (when (eq :DEFINE (textblockdef-operation def))
      (weave-uses weaver def))
  (write-line "</div>"))

(defun weave-prose-line (weaver line def)
  (loop for expr in line do
        (cond ((stringp expr) (write-string expr))
              ((commandp expr)
               (case (first expr)
                 (:INCLUDE (weave-include
                            (second expr)
                            nil
                            weaver))
                 (:TITLE
                  (setf (weaver-title weaver)
                        (second expr)))
                 (:HEADING
                  ; Also can act as a title
                  (when (null (weaver-title weaver))
                    (setf (weaver-title weaver)
                          (textheading-title (second expr))))

                  (format t "<h~a id=\"~a\">~a</h~a>~%"
                          (textheading-level (second expr))
                          (gethash (second expr) (weaver-anchors weaver))
                          (textheading-title (second expr))
                          (textheading-level (second expr))))
                 (:CODE_TYPE
                  (let* ((args (split-whitespace (second expr)))
                         (language (first args))
                         (extension (subseq (second args) 1)))
                    (setf (gethash extension (weaver-code-type-table weaver)) language)
                    (push extension (weaver-used-extensions weaver))))
                 (:MATHBLOCK
                  ; Put <code> tags in block so it displays tex nicely without JS.
                  (setf (weaver-used-math weaver) t)
                  (write-string "<div class=\"math-block\"><code>")
                  (when (not (equal (second expr) "displaymath"))
                      (format t "\\begin{~a}" (second expr)))
                  (write-separated-list (third expr) #\newline *standard-output*)
                  (when (not (equal (second expr) "displaymath"))
                      (format t "\\end{~a}" (second expr)))
                  (write-string "</code></div>"))
                 (:MATH
                  ; Use backticks to prevent markdown from formatting tex,
                  ; for example treating _ as emphasis.
                  (setf (weaver-used-math weaver) t)
                  (format t "<span class=\"math\">`~a`</span>"
                          (second expr)))
                 (:TOC
                  (weave-toc weaver))

                 ; These commands are from Zach's Literate.
                 ; We treat them as warnings instead of errors to make migration easier.
                 ; It's possible they may be useful for us in the future.
                 ((:COMMENT_TYPE :ADD_CSS :OVERWRITE_CSS :COLORSCHEME :ERROR_FORMAT)
                  (warn "deprecated Literate prose command ~s. ignored." (first expr)))
                 (otherwise (error 'user-error
                                   :format-control "unknown prose command ~S"
                                   :format-arguments (first expr)))))
              (t (error "unknown structure ~s" expr)))))

(defun weave-prosedef (weaver def)
   (let ((block (textblockdef-block def)))
     (setf (weaver-filename weaver) (textblockdef-weave-file def))
     (loop for line across (textblock-lines block) do
           (weave-prose-line weaver line def)
           (write-line ""))))

(defun weave-blocks (weaver source-defs)
  (dolist (def source-defs)
    (when (textblockdef-weavable def)
      (if (eq (textblockdef-kind def) :CODE)
          (weave-codedef weaver def)
          (weave-prosedef weaver def)))))

(defparameter *attribution* "Generated by srcweave. https://github.com/justinmeiners/srcweave")

(defun weave-html (weaver stream source-defs)
  (format stream "<!-- ~a -->~%" *attribution*)
  (finish-output stream)
  ; Run markdown on the entire document so named links work.

  (if (stringp *markdown-command*)
      (let ((md (uiop:launch-program
                  (split-whitespace *markdown-command*)
                  :input :stream
                  :output stream
                  :error-output *error-output*)))
        (let ((*standard-output* (uiop:process-info-input md)))
          (weave-blocks weaver source-defs))
        (uiop:close-streams md)
        (uiop:wait-process md))
      (let ((*standard-output* stream))
        (weave-blocks weaver source-defs))))

(defparameter *env-key-types* "LIT_TYPES")
(defparameter *env-key-title* "LIT_TITLE")
(defparameter *env-key-math* "LIT_MATH")

(defun weave-path (weaver source-defs output-path)
  (with-open-file (output-stream output-path
                                 :direction :output
                                 :if-exists :supersede
                                 :if-does-not-exist :create)

    (if (not (stringp *format-command*))
        (weave-html weaver output-stream source-defs)
        ; we first write to data in RAM to get formatting info for environment variables.
        (let ((text (with-output-to-string (s)
                      (weave-html weaver s source-defs))))
           ; set environmental variables based on weave results.
          (setf (uiop:getenv *env-key-types*)
                (extensions-to-type-string (weaver-code-type-table weaver)
                                           (weaver-used-extensions weaver)))
          (setf (uiop:getenv *env-key-title*) (or (weaver-title weaver) ""))
          (setf (uiop:getenv *env-key-math*) (if (weaver-used-math weaver) "1" ""))

           ; run format on weave results.
          (uiop:run-program (split-whitespace *format-command*)
                            :output output-stream
                            :error-output t
                            :input (make-string-input-stream text))))))

(defun weave-build-pathname (file-path base)
  (uiop:merge-pathnames*
    (make-pathname
      :name (pathname-name (uiop:ensure-pathname file-path :want-file t))
      :type (if (stringp *markdown-command*) "html" "md"))
    base))

(defun weave (file-defs output-dir)
  (let* ((output-dir (uiop:ensure-directory-pathname output-dir))
         (weaver (make-weaver-default file-defs)))
    (map nil (lambda (path-defs-pair)
               (let* ((input-path (car path-defs-pair))
                      (output-path (weave-build-pathname input-path output-dir))
                      (defs (cdr path-defs-pair)))

                 (progn
                   (format t "writing doc: ~a~%" output-path)
                   (ensure-directories-exist output-path)
                   (weave-path
                     weaver
                     defs
                     output-path))))
         file-defs)))

(defun make-title-to-first-def-table (defs)
  "maps titles to their first textblockdef"
  (let ((table (make-hash-table)))
    (dolist (def defs)
      (when (not (gethash (textblockdef-title-sym def) table))
        (setf (gethash (textblockdef-title-sym def) table)
              def)))
    table))

(defun make-block-use-table (defs)
  "maps titles to the definition ids that use them"
  (let ((table (make-hash-table)))
    (dolist (def defs)
      (dolist (dependency (textblock-references (textblockdef-block def)))
        (push def
              (gethash dependency table))))
    (maphash (lambda (key val)
               (setf (gethash key table) (reverse val))) table)
    table))

(defparameter *fragment-invalid-pattern*
  (ppcre:create-scanner '(:INVERTED-CHAR-CLASS
                          #\- #\.
                          (:RANGE #\a #\z)
                          (:RANGE #\0 #\9))))

(defun string-to-fragment (string)
  (ppcre:regex-replace-all '(:GREEDY-REPETITION 1 nil #\-)
   (ppcre:regex-replace-all *fragment-invalid-pattern*
                           (string-downcase (if (is-file-title string) (subseq string 1) string))
                           "-")
   "-"))


(defun textblockdef-fragment (def)
  (format nil ":~a" (string-to-fragment (textblockdef-title def))))

(defun textheading-fragment (def)
  (format nil "~a" (string-to-fragment (textheading-title def))))

(defun disambiguate-by-adding-dot-suffix (str i)
  (format nil "~a_~a" str (+ i 1)))

(defun make-anchor-table (file-defs)
  "Find HTML anchors for a list of defs that will appear on the same page."
  (let ((table (make-hash-table)))
    (dolist (file-pair file-defs)
       ; only anchors which appear on the same page need to be disambiguated
      (disambiguate-ids-into (cdr file-pair)
                             table
                             :key #'textblockdef-fragment
                             :combine #'disambiguate-by-adding-dot-suffix)

      (disambiguate-ids-into (all-headings-from-defs (cdr file-pair))
                             table
                             :key #'textheading-fragment
                             :combine #'disambiguate-by-adding-dot-suffix))
      table))


(defun weave-toc-section (section weaver)
  (format t "<li><a href=\"~a\">~a</a></li>~%"
          (weave-anchor-for-heading section weaver)
          (textheading-title section)))

(defun weave-toc-chapter (chapter sections show-chapters weaver)
    (when show-chapters
      (format t "<li><a href=\"~a\">~a</a>~%"
              (weave-anchor-for-heading chapter weaver)
              (textheading-title chapter)))

    (format t "<ol>~%")
    (dolist (section sections)
      (weave-toc-section (car section) weaver))
    (format t "</ol>~%")

    (when show-chapters
      (format t "</li>~%")))

(defun weave-toc (weaver)
  (let ((show-chapters (> (length (weaver-toc weaver)) 1)))
    (when show-chapters
      (format t "<ol>~%"))

    (dolist (chapter (weaver-toc weaver))
      (weave-toc-chapter (car chapter)
                         (cdr chapter)
                         show-chapters
                         weaver))
    (when show-chapters
      (format t "</ol>"))))
