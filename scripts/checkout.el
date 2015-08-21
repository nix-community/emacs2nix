(require 'package)
(package-initialize)

(require 'package-build)

(defun checkout ()
  (if (not noninteractive)
      (error "`checkout' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipe-file ,package-name ,work-dir)
     (emacs2nix-checkout recipe-file package-name work-dir))))

(defun checkout-stable ()
  (if (not noninteractive)
      (error "`checkout-stable' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipe-file ,package-name ,work-dir)
     (progn
       (setq package-build-stable t)
       (emacs2nix-checkout recipe-file package-name work-dir)))))

(defun emacs2nix-checkout (recipe-file package-name work-dir)
  (setq package-build-verbose nil)
  (let* ((recipe (cdr (package-build--read-from-file recipe-file)))
         (actual-work-dir (file-name-as-directory work-dir)))
    (princ (package-build-checkout package-name recipe actual-work-dir))))
