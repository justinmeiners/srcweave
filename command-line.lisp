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

(defun parse-empty-arg-as-null (arg)
  (if (and (stringp arg) (= (length arg) 0))
      '()
      arg))

(opts:define-opts
  (:name :help
         :description "Show program help."
         :long "help"
         :short #\h)

  (:name :tangle
         :description "Generate source code from documents, including creating files for corresponding blocks. Tangle is lazy and uses file modification dates to determine when to rebuild."
         :long "tangle"
         :short #\t
         :arg-parser (lambda (text)
                       (uiop:ensure-pathname text :ensure-directory t)))

  (:name :weave
         :description "Generate HTML documentation for documents. One .html is created for each .lit input file."
         :long "weave"
         :short #\w
         :arg-parser (lambda (text)
                       (uiop:ensure-pathname text :ensure-directory t)))

  (:name :md-compiler
         :description "Specify a command for converting markdown to HTML. Default: markdown."
         :arg-parser #'parse-empty-arg-as-null
         :long "markdown-converter")

  (:name :styler
         :description "Specify a command for converting raw HTML into styled output. Recommended: `srcweave-html-styler`."
         :arg-parser #'parse-empty-arg-as-null
         :long "styler")

  (:name :force-output
         :description "Ignore file modification dates and tangle all files. For tangle only."
         :short #\f
         :long "force-output")

  (:name :no-trailing-newline
         :description "Do not add a trailing newline to source files. For tangle only."
         :long "no-trailing-newline"))


(defun start-command ()
  (multiple-value-bind (options free-args)
      (opts:get-opts)

    (when (getf options :help)
      (opts:describe
        :prefix "Literate programming system. Write code to be read by humans, not machines."
        :usage-of "srcweave"
        :args "LITFILE ...")

      (write-lines
       ""
       "Styler:"
       "  A styler program accepts HTML from stdin and outputs to stdout. It will also receive the following env vars:"
       "    - LIT_TYPES: a space separated list of programming languages used in this file."
       "    - LIT_TITLE: the title of this document."
       "    - LIT_MATH: whether typeset math is desired for this file."
       "")

      (write-lines
       "Markdown:"
       "  A markdown converter accepts markdown (with HTML fragments) from stdin and produces HTML to stdout."
       "  Gruber's perl implementation or discount are recommended choices."
       "")

      (write-line "Created by Justin Meiners (2022)")

      (opts:exit 0))

    (when (null free-args)
      (error 'user-error :format-control "must provide at least one literate file"))

    (when (and (not (getf options :weave))
               (not (getf options :tangle)))
      (warn "no tangle or weave command specified."))

    (setf *markdown-command*
          (getf options :md-compiler *markdown-command*))
    (setf *styler-command*
          (getf options :styler *styler-command*))
    (setf *trailing-newline*
          (getf options :trailing-newline *trailing-newline*))

    (let ((file-defs (parse-lit-files free-args))
          (ignore-dates (if (getf options :force-output) t nil))
          (weave-path (getf options :weave))
          (tangle-path (getf options :tangle)))

     (when tangle-path
        (format t "TANGLE~%")
        (tangle (alexandria-2:mappend #'cdr file-defs)
                tangle-path
                :ignore-dates ignore-dates)

       (format t "DONE~%"))
      (when weave-path
        (format t "WEAVE~%")
        (weave file-defs
               weave-path)
        (format t "DONE~%")))))

(defun unknown-option (condition)
  (warn "unknown option: ~a" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defun toplevel ()
  (handler-case
    (handler-bind ((opts:unknown-option #'unknown-option))
      (start-command))
    (opts:missing-arg (condition)
      (format *error-output* "option ~s needs an argument!~%"
              (opts:option condition)))
    (opts:arg-parser-failed (condition)
      (format *error-output* "cannot parse ~s as argument of ~s~%"
              (opts:raw-arg condition)
              (opts:option condition)))
    (opts:missing-required-option (con)
      (format *error-output* "error: ~a~%" con))
    (user-error (c)
      (format *error-output* "error: ~a~%" c)
      (opts:exit 2))))
