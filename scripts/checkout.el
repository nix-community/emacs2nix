(require 'package)
(package-initialize)

(require 'package-build)

(defun checkout ()
  (if (not noninteractive)
      (error "`checkout' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-dir ,package-name ,work-dir)
     (progn
       (setq package-build-verbose nil)
       (setq package-build-recipes-dir recipes-dir)
       (let* ((recipe (cdr (assoc (intern package-name) (package-build-recipe-alist)))))
         (princ (package-build-checkout package-name recipe (file-name-as-directory work-dir))))))))
