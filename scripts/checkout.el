(require 'package)
(package-initialize)

(require 'package-build)

(defun checkout ()
  (if (not noninteractive)
      (error "`checkout' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipe-file ,package-name ,work-dir)
     (progn
       (setq package-build-verbose nil)
       (let* ((recipe (cdr (pb/read-from-file recipe-file))))
         (princ (package-build-checkout package-name recipe (file-name-as-directory work-dir))))))))
