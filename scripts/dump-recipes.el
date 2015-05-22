(require 'package)
(package-initialize)

(require 'package-build)

(require 'json)

(defun dump-recipes-json ()
  (if (not noninteractive)
      (error "`dump-recipes-json' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-dir)
     (progn
       (setq package-build-recipes-dir recipes-dir)
       (princ (json-encode (package-build-recipe-alist)))))))
