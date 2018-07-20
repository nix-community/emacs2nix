(require 'package)
(package-initialize)

(require 'package-recipe)
(require 'package-build)

(require 'json)

(defun read-recipe (name)
  "Return a recipe plist for the package named NAME.
If no such recipe file exists or if the contents of the recipe
file is invalid, then raise an error."
  ;; Adapted from `package-recipe-lookup' to return a plist
  ;; rather than an EIEIO object.
  (let ((file (expand-file-name name package-build-recipes-dir)))
    (if (file-exists-p file)
        (let* ((recipe (with-temp-buffer
                         (insert-file-contents file)
                         (read (current-buffer)))))
          (package-recipe--validate recipe name)
          recipe)
      (error "No such recipe: %s" name))))

(defun dump-recipes-json ()
  (if (not noninteractive)
      (error "`dump-recipes-json' is to be used only with -batch"))
  (pcase command-line-args-left
    (`(,recipes-dir)
     (progn
       (setq package-build-recipes-dir recipes-dir)
       (let* ((recipes (mapcar #'read-recipe (package-recipe-recipes))))
       (princ (json-encode recipes)))))))
