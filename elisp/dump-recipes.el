(require 'package)
(package-initialize)

(require 'package-build)

(defun dump-recipes ()
  (if (not noninteractive)
      (error "`dump-recipes' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-dir)
     (progn
       (setq package-build-recipes-dir recipes-dir)
       (print (package-build-recipe-alist))))))
