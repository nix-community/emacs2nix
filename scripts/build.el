(require 'package)
(package-initialize)

(require 'package-build)
(require 'package-recipe)

(setq package-build-verbose nil)
(setq package-build-working-dir (expand-file-name "working/"))
(setq package-build-archive-dir (expand-file-name "packages/"))
(setq package-build-recipes-dir (expand-file-name "recipes/"))

(defun pkg-info-for-json (info)
  "Convert INFO into a data structure which will serialize to JSON in the desired shape.
Differs from `package-build--pkg-info-for-json' by ignoring `:props'."
  (let ((ver (elt info 0))
        (deps (elt info 1))
        (desc (elt info 2))
        (type (elt info 3)))
    (list :ver ver
          :deps (cl-mapcan (lambda (dep)
                             (list (intern (format ":%s" (car dep)))
                                   (cadr dep)))
                           deps)
          :desc desc
          :type type)))

(defun build-1 (name)
  (let* ((rcp (package-recipe-lookup name))
         (version (package-build--checkout rcp))
         (pkg-info (cdr (package-build-package rcp version))))
    (princ (json-encode (pkg-info-for-json pkg-info)))))

(defun build ()
  (if (not noninteractive)
      (error "`build' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,name) (build-1 name))))

(defun build-stable ()
  (if (not noninteractive)
      (error "`build-stable' is to be used only with -batch"))
  (setq package-build-stable t)
  (build))

(defun files-1 (name)
  (let* ((rcp (package-recipe-lookup name))
         (version (package-build--checkout rcp)))
    (princ (json-encode (package-build--expand-source-file-list rcp)))))

(defun files ()
  (pcase command-line-args-left
    (`(,name) (files-1 name))))
