(require 'straight)

(eval-and-compile
  (require 'cl-lib))

(defun ublt/straight-recipe (package &rest plist)
  (declare (indent 1))
  (let ((recipe (cl-copy-list (cdr (straight-recipes-retrieve package)))))
    (cl-loop for (key value) on plist by #'cddr
             do (plist-put recipe key value))
    (cons package recipe)))

(defun ublt/straight-override (package &rest plist)
  (declare (indent 1))
  (straight-override-recipe (apply #'ublt/straight-recipe package plist)))

(cl-defun ublt/-fork (name &key branch)
  `(:repo ,(format "github:ubolonton/%s" name)
          :host nil ; XXX: Otherwise straight tries to construct https URL.
          :remote "ubolonton"
          :branch ,branch))

(provide 'ublt-straight-recipes)
