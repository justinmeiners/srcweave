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

(defstruct textblock
  "stores a vector of lines. Each line is a list of strings or other commands."
  (lines (make-array 16 :fill-pointer 0 :adjustable t) :type array)
  (modify-date 0 :type integer))

(defstruct textblockdef
  "Information about where blocks are defined and config options."
  (title-sym nil :type (or null symbol))
  (block (make-textblock) :type textblock)
  (kind :PROSE :type symbol)
  (line-number 0 :type integer)
  (file nil :type (or null string))
  (operation :DEFINE :type symbol)
  (modifiers nil :type (or null list))
  (language "text" :type string))

(defun textblockdef-title (def)
  (symbol-name (textblockdef-title-sym def)))

(defun textblockdef-is-file (def)
  (find :FILE (textblockdef-modifiers def)))

(defun textblockdef-weavable (def)
  (not (find :NO-WEAVE (textblockdef-modifiers def))))

(defun textblockdef-tanglable (def)
  (not (find :NO-TANGLE (textblockdef-modifiers def))))

(defun textblock-references (block)
  "returns all titles which this block references for inclusion"
  (let ((list nil))
    (loop for line across (textblock-lines block) do
      (loop for expr in line do
        (when (and (commandp expr) (eq (first expr) :INCLUDE))
          (push (second expr) list))))
    list))

(defun is-file-title (name)
  (uiop:string-prefix-p "/" name))

(defun commandp (expr)
  (and (consp expr) (symbolp (car expr))))

(defstruct textheading
  (title-sym nil :type (or null symbol))
  (level 0 :type number)
  (file nil :type (or null string)))

(defun textheading-title (heading)
  (symbol-name (textheading-title-sym heading)))

(defun make-textheading-toc (file-def-pairs)
  (let ((chapter nil)
        (all-chapters nil))
    (dolist (file-pair file-def-pairs)
      (dolist (def (cdr file-pair))
        (loop for line across (textblock-lines (textblockdef-block def)) do
          (loop for expr in line do
            (when (and (commandp expr)
                       (eq (first expr) :HEADING))
              (setf (textheading-file (second expr)) (car file-pair))
              (case (textheading-level (second expr))
                (1 (when chapter
                                        ; chapters are built in reverse
                    (push (nreverse chapter) all-chapters)
                    (setf chapter nil))
                  (setf chapter (list (second expr))))
                (2 (push (list (second expr)) chapter))))))))
    (when chapter
      (push (nreverse chapter) all-chapters))
    (nreverse all-chapters)))

(defun all-headings-from-defs (defs)
  (let ((result nil))
    (dolist (def defs)
      (loop for line across (textblock-lines (textblockdef-block def)) do
        (loop for expr in line do
          (when (and (commandp expr)
                     (eq (first expr) :HEADING))
            (push (second expr) result)
            ))))
    result))
