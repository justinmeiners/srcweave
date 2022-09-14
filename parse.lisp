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

; PARSE: Capture lines, determine commands, organize them into blocks.

(defun parse-block-operator (x)
  (cond ((equal x "+=") :APPEND)
        ((equal x ":=") :REDEFINE)
        (t (error 'user-error
                  :format-control "unknown operation ~s"
                  :format-arguments x))))

(defun parse-modifier (x)
  (cond ((equal x "noWeave") :NO-WEAVE)
        ((equal x "hidden") :NO-WEAVE)
        (t (error 'user-error
                  :format-control "unknown modifier ~s"
                  :format-arguments x))))

(defparameter *ref-pattern*
  (ppcre:create-scanner '(:SEQUENCE
                          (:NEGATIVE-LOOKBEHIND #\@)
                          "@{"
                          (:REGISTER (:GREEDY-REPETITION 1 NIL (:INVERTED-CHAR-CLASS #\})))
                          #\})))

;; To link to an section or chapter
(defparameter *anchor-pattern* "\\[.*?\\]\\(@(.*?)\\)")

(defun parse-refs (line)
  (let ((parts (ppcre:split *ref-pattern* line :with-registers-p t)))
    (mapcar-indexed (lambda (string i)
                      (if (evenp i)
                          (ppcre:regex-replace-all "@@({[^}]+})" string "@\\1")
                          (list :INCLUDE string)))
                    parts)))

(defparameter *command-pattern*
  (ppcre:create-scanner
    '(:SEQUENCE
      :START-ANCHOR
      #\@
      (:REGISTER (:GREEDY-REPETITION 1 nil
                  (:CHAR-CLASS #\_ #\- #\.
                   (:RANGE #\A #\Z)
                   (:RANGE #\a #\z)
                   (:RANGE #\0 #\9))))
      (:GREEDY-REPETITION 0 nil 
       :WHITESPACE-CHAR-CLASS))))

(defparameter *heading-pattern*
  (ppcre:create-scanner
    '(:SEQUENCE
      :START-ANCHOR
      (:REGISTER (:GREEDY-REPETITION 1 nil #\#))
      (:GREEDY-REPETITION 1 nil :WHITESPACE-CHAR-CLASS))))


(defparameter *math-inline-pattern*
  (ppcre:create-scanner
    '(:SEQUENCE #\\
      (:REGISTER (:ALTERNATION "begin" "end"))
      #\{ (:REGISTER "math") #\})))

(defun parse-math-text (line)
  (let ((counter 0)
        (start 0)
        (math-type nil)
        (expr nil))
    (ppcre:do-scans
      (match-start match-end reg-starts reg-ends *math-inline-pattern* line)
      (let ((command (subseq line
                             (aref reg-starts 0)
                             (aref reg-ends 0)))
            (type (subseq line
                          (aref reg-starts 1)
                          (aref reg-ends 1))))

        (if (equal command "begin")
            (progn
              (when (= counter 0)
                (push (subseq line start match-start) expr)
                (setf math-type type)
                (setf start match-end))
              (incf counter))
           (progn
              (decf counter)
              (when (= counter 0)
                (when (not (equal type math-type))
                  (error 'user-error
                         :format-control "expected math command ~s"
                         :format-arguments (list type)))
                (push (list :MATH (subseq line start match-start)) expr)
                (setf math-type nil)
                (setf start match-end)))
            )))
    (when (not (= counter 0))
      (error 'user-error
             :format-control "unbalance tags: ~s"
             :format-arguments (list line)))
    (push (subseq line start) expr)
    (nreverse expr)))

(defun parse-prose-line (line)
  (or 
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings *heading-pattern* line)
      (if match
          (list (case (length (aref groups 0))
                  (1 (list :C (subseq line (length match))))
                  (2 (list :S (subseq line (length match))))
                  (otherwise line)))
          nil))
    (multiple-value-bind (match groups)
        (ppcre:scan-to-strings *command-pattern* line)
      (if match
          (list (list (intern (string-upcase (aref groups 0)) :KEYWORD)
                      (subseq line (length match))))
          nil))
    (alexandria-2:mappend (lambda (expr)
                            (if (stringp expr)
                                (parse-math-text expr)
                                (list expr)))
                          (parse-refs line))))

(defparameter *block-start-pattern*
  (ppcre:create-scanner '(:SEQUENCE :START-ANCHOR "---")))
 
(defun parse-block-start (line)
  (let*  ((parts 
            (mapcar (lambda (x) (string-trim " " x))
                    (remove-if #'string-nullp 
                               (ppcre:split "(---)|([+]=)|(:=)" line
                                            :with-registers-p t))))

          (suffix (cdr (cdr parts)))
          (divider (position "---" suffix :test #'equal))
          (operators (subseq suffix 0 divider))
          (modifiers (subseq suffix (+ 1 (or divider
                                             (- (length suffix) 1)
                                             )))))
    (values
      (nth 1 parts)
      (mapcar #'parse-block-operator operators)  
      (mapcar #'parse-modifier modifiers))))

(defun read-code-block (line n stream)
  (prog ((def nil))
        (multiple-value-bind (title operator modifiers)
            (parse-block-start line)

          (when (null title)
            (error 'user-error
                   :format-control "block is missing title on line: ~s"
                   :format-arguments (list n)))

          (setf def (make-textblockdef :line-number n
                                       :kind :CODE
                                       :title title
                                       :operation (if (null operator) :DEFINE (first operator))
                                       :modifiers (if (is-filename title)
                                                      (cons :FILE modifiers)
                                                      modifiers) )))

        TEXT
        (setf line (read-line stream nil))
        (incf n)
        (when (null line)
          (error 'user-error
                 :format-control "unexpected end of file in code block: ~s"
                 :format-arguments (list (textblockdef-title def))))

        (when (ppcre:scan *block-start-pattern* line)
          (return (values def line n)))

        (vector-push-extend (parse-refs line)
                            (textblock-lines (textblockdef-block def)))
        (go TEXT)))


(defparameter *math-block-pattern*
  (ppcre:create-scanner
    '(:SEQUENCE
      :START-ANCHOR
      #\\ (:REGISTER (:ALTERNATION "begin" "end")) #\{
      (:REGISTER (:ALTERNATION "equation" "align" "displaymath" "gather" "CD"))
      #\})))

(defun read-prose-block (line n stream)
  (prog ((def (make-textblockdef :line-number n
                                 :kind :PROSE))
         (math-type)
         (math-lines nil))

    TEXT
    (setf line (read-line stream nil))
    (incf n)

    (when (or (null line)
              (ppcre:scan *block-start-pattern* line))
      (return (values def line n)))

    (ppcre:register-groups-bind (command type)
                                (*math-block-pattern* line :sharedp t)
                                (when (not (equal command "begin"))
                                  (error 'user-error
                                         :format-control "unexpected \end block. line: ~s"
                                         :format-arguments (list n)))
                                (setf math-type type)
                                (go MATH))

    (vector-push-extend (parse-prose-line line)
                        (textblock-lines (textblockdef-block def)))
    (go TEXT)

    MATH
    (setf line (read-line stream nil))
    (incf n)
    (when (null line)
      (error 'user-error
             :format-control "unexpected end of file in math block: ~s"
             :format-arguments (list math-type)))

    (ppcre:register-groups-bind (command type)
                                (*math-block-pattern* line :sharedp t)
                                (when (and (equal type math-type)
                                           (equal command "end"))
                                  (vector-push-extend (list (list :MATHBLOCK
                                                                  math-type
                                                                  (nreverse math-lines)))
                                                      (textblock-lines (textblockdef-block def)))
                                  (setf math-lines nil)
                                  (go TEXT)))
    (push line math-lines)
    (go MATH)))

(defun is-filename (name)
  (uiop:string-prefix-p "/" name)) 

(defun read-lit (&optional (stream *standard-input*))
  (prog ((prose t)
         (all-defs nil)
         (line nil)
         (line-number 0))

        LOOP
        (multiple-value-bind (def new-line n)
            ; alternate between prose and code
            (if prose
                (read-prose-block line line-number stream)
                (read-code-block line line-number stream))

          (push def all-defs)

          (setf line new-line)
          (setf line-number n)
          (setf prose (not prose))

          (if (null line)
              (return (nreverse all-defs))
              (go LOOP)))))

(defun set-file-attributes (defs modify-date file)
  (loop for def in defs do
        (setf (textblockdef-file def) file)
        (setf (textblock-modify-date
                (textblockdef-block def)) modify-date)) defs)

(defun parse-lit-file (path)
  (when (not (uiop:file-pathname-p path))
    (error 'user-error
           :format-control "lit input: ~s is not a file path"
           :format-arguments (list path)))

  (set-file-attributes
    (with-open-file (*standard-input* path :direction :input)
      (read-lit))
    (file-write-date path)
    (file-namestring path)))


(defun set-def-indexes (file-def-pairs)
  (let ((counter 0))
    (loop for pair in file-def-pairs do
          (loop for def in (cdr pair) do
                (setf (textblockdef-index def) counter)
                (incf counter))))
  file-def-pairs)


(defun parse-lit-files (paths)
  "returns a list of pairs (path . list-of-block-defs)"
    (set-def-indexes
     (mapcar (lambda (path)
              (cons path (parse-lit-file (uiop:ensure-pathname path)))) paths)))

