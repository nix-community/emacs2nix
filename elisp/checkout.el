(require 'package)
(package-initialize)

(require 'package-build)

(defun checkout ()
  (if (not noninteractive)
      (error "`checkout' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-el ,package-name ,work-dir)
     (progn
       (setq package-build-verbose nil)
       (let* ((recipes (pb/read-from-file recipes-el))
              (recipe (cdr (assoc (intern package-name) recipes))))
         (princ (package-build-checkout package-name recipe (file-name-as-directory work-dir))))))))
