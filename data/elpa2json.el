(require 'json)

(defun pb/sym-to-keyword (s)
  "Return a version of symbol S as a :keyword."
  (intern (concat ":" (symbol-name s))))

(defun pb/pkg-info-for-json (info)
  "Convert INFO into a data structure which will serialize to JSON in the desired shape."
  (let* ((ver (elt info 0))
         (deps (elt info 1))
         (desc (elt info 2))
         (type (elt info 3)))
    (list :ver ver
          :deps (apply 'append
                       (mapcar (lambda (dep)
                                 (list (pb/sym-to-keyword (car dep))
                                       (cadr dep)))
                               deps))
          :desc desc
          :dist type)))

(defun packages-for-json (packages)
  "Return the packages list in a form suitable for JSON encoding."
  (apply 'append
         (mapcar (lambda (entry)
                   (list (pb/sym-to-keyword (car entry))
                         (pb/pkg-info-for-json (cdr entry))))
                 packages)))

(defun print-archive-contents-as-json (archive-contents)
  "Print the packages listed in file ARCHIVE-CONTENTS in JSON encoding."
  (let ((packages (cdr (read (find-file-noselect archive-contents)))))
    (princ (json-encode (packages-for-json packages)))))
