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


(defpackage :srcweave
  (:use :cl)
  (:export
    :toplevel
    :parse-lit-files
    :read-lit
    :weave
    :tangle
    *markdown-command*
    *styler-command*
    "user-error"))

(in-package :srcweave)

(defparameter *markdown-command* "markdown")
(defparameter *styler-command* nil)
(defparameter *trailing-newline* t)
