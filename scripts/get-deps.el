(require 'package)
(package-initialize)

(require 'package-build)

(require 'json)

(defun get-deps ()
  (if (not noninteractive)
      (error "`get-deps' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipe-file ,package-name ,source-dir)
     (progn
       (setq package-build-verbose nil)
       (let* ((recipe (cdr (package-build--read-from-file recipe-file)))
              (file-specs (package-build--config-file-list recipe))
              (files (package-build-expand-file-specs source-dir file-specs))
              (pkg-file (concat package-name "-pkg.el"))
              (pkg-file-source (or (package-build--find-source-file pkg-file files)
                                   pkg-file))
              (file-source (concat package-name ".el"))
              (pkg-source (or (package-build--find-source-file file-source files)
                              file-source))
              (pkg-info (let ((default-directory source-dir))
                          (or (package-build--get-pkg-file-info pkg-file-source)
                              ;; some packages (like magit) provide name-pkg.el.in
                              (package-build--get-pkg-file-info
                               (expand-file-name (concat pkg-file ".in")
                                                 (file-name-directory pkg-source)))
                              (package-build--get-package-info pkg-source))))
              (deps (apply 'append
                           (mapcar (lambda (dep)
                                     (list (intern (format ":%s" (car dep)))
                                           (cadr dep)))
                                   (elt pkg-info 1)))))
         (if deps (princ (json-encode deps)) (princ "{}")))))))
