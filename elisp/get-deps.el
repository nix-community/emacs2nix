(require 'package)
(package-initialize)

(require 'package-build)

(require 'json)

(defun get-deps ()
  (if (not noninteractive)
      (error "`get-deps' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-el ,package-name ,source-dir)
     (let* ((recipes (pb/read-from-file recipes-el))
            (recipe (cdr (assoc (intern package-name) recipes)))
            (file-specs (or (plist-get recipe :files)
                            package-build-default-files-spec))
            (files (package-build-expand-file-specs source-dir file-specs))
            (pkg-file (concat package-name "-pkg.el"))
            (pkg-file-source (or (pb/find-source-file pkg-file files)
                                 pkg-file))
            (file-source (concat package-name ".el"))
            (pkg-source (or (pb/find-source-file file-source files)
                            file-source))
            (pkg-info (let ((default-directory source-dir))
                        (or (pb/get-pkg-file-info pkg-file-source)
                            ;; some packages (like magit) provide name-pkg.el.in
                            (pb/get-pkg-file-info
                             (expand-file-name (concat pkg-file ".in")
                                               (file-name-directory pkg-source)))
                            (pb/get-package-info pkg-source)))))
       (princ (json-encode (elt pkg-info 1)))))))
