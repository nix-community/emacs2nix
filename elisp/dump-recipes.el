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

(require 'json)

(defun dump-recipes-json ()
  (if (not noninteractive)
      (error "`dump-recipes-json' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-el)
     (progn
       (princ (json-encode (pb/read-from-file recipes-el)))))))
