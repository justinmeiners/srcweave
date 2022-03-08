; How to load a local ASDF?
; https://stackoverflow.com/a/65303394/425756

(asdf:defsystem "srcweave"
    :version "0.0.1"
    :author "Justin Meiners"
    :license "GPL 2"
    :depends-on (:cl-ppcre :uiop :unix-opts :alexandria)
;    :build-operation "program-op"
    :build-pathname "srcweave"
    :entry-point "srcweave:toplevel"
    :serial t
    :components (
          (:file "package")
          (:file "utils")
          (:file "textblock")
          (:file "parse")
          (:file "toc")
          (:file "code-types")
          (:file "weave")
          (:file "tangle")
          (:file "command-line")))

