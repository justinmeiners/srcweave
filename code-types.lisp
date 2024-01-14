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

; Wha is the goal of detecting code types?

; - identifiy which language is in each block
;     Best indicator is the extension of the blocks that include it.
;     Already this is tricky because a block may be included by more than one.
;     - topological sort block names
;     - assign them langues in that order.
;     - Issue a warning if it is ambiguous.
; - tell the styler which types are used
;     - use env var

(defparameter *default-code-types*
  '(
    ("bash" . "bash")
    ("c" . "c")
    ("cl" . "common-lisp")
    ("cpp" . "cpp")
    ("cs" . "csharp")
    ("css" . "css")
    ("el" . "emacs-lisp")
    ("h" . "c")
    ("hpp" . "cpp")
    ("hs" . "haskell")
    ("htm" . "html")
    ("html" . "html")
    ("impl" . "cpp")
    ("java" . "java")
    ("js" . "javascript")
    ("kt" . "kotlin")
    ("lisp" . "common-lisp")
    ("lua" . "lua")
    ("m" . "objective-c")
    ("plist" . "xml")
    ("py" . "python")
    ("r" . "r")
    ("rb" . "ruby")
    ("rs" . "rust")
    ("scala" . "scala")
    ("scm" . "scheme")
    ("sh" . "shell")
    ("sql" . "sql")
    ("tex" . "tex")
    ("ts" . "typescript")
    ("swift" . "swift")
    ("xml" . "xml") ))

(defun codetable-create ()
  (alexandria-2:alist-hash-table *default-code-types* :test #'equal))

(defun extensions-to-type-string (type-table extensions)
  (string-downcase (join-strings (remove-duplicates (remove-if #'null (mapcar
                                                     (lambda (extension)
                                                       (gethash extension type-table))
                                                     extensions))
                                                    :test #'equal))))
