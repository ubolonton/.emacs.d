(require 'straight)

(eval-and-compile
  (require 'cl-lib))

(defun ublt/straight-recipe (name &rest plist)
  (declare (indent 1))
  (let ((recipe (cl-copy-list (cdr (straight-recipes-retrieve name)))))
    (cl-loop for (key value) on plist by #'cddr
             do (plist-put recipe key value))
    (cons name recipe)))

(defun ublt/straight-override (name &rest plist)
  (declare (indent 1))
  (straight-override-recipe (apply #'ublt/straight-recipe name plist)))

(cl-defun ublt/-fork (name &key branch)
  `(:repo ,(format "github:ubolonton/%s" name)
          :host nil ; XXX: Otherwise straight tries to construct https URL.
          :remote "ubolonton"
          :branch ,branch))

(ublt/straight-override 'lv
  :fork (ublt/-fork "hydra" :branch "delayed-hiding"))

(ublt/straight-override 'hydra
  :fork (ublt/-fork "hydra" :branch "delayed-hiding"))

(provide 'ublt-straight-recipes)
