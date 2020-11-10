(require 'straight)

(eval-and-compile
  (require 'cl-lib))

(defun ublt/straight-recipe (package &rest plist)
  "Return the recipe for PACKAGE, with modifications from PLIST.
The base recipe is sourced from `straight-recipes-repositories', and is left
untouched."
  (declare (indent 1))
  (let ((recipe (cl-copy-list (cdr (straight-recipes-retrieve package)))))
    (cl-loop for (key value) on plist by #'cddr
             do (plist-put recipe key value))
    (cons package recipe)))

(defun ublt/straight-override (package &rest plist)
  "Override properties of PACKAGE's recipe with PLIST.
The base recipe is sourced from `straight-recipe-repositories'. The modified
recipe is used, while the base recipe is left untouched. To revert to using the
base recipe, call this function without specifying PLIST.

Example:

    (ublt/straight-override 'hydra
      :fork (ublt/-fork \"hydra\" :branch \"delayed-hiding\"))
    (ublt/straight-override 'lv
      :fork (ublt/-fork \"hydra\" :branch \"delayed-hiding\"))
"
  (declare (indent 1))
  (straight-register-package (apply #'ublt/straight-recipe package plist)))

(defun ublt/straight-add-files (package files)
  "Add more FILES to the recipe for PACKAGE."
  (declare (indent 1))
  (ublt/straight-override package
    :files (append (plist-get (cdr (ublt/straight-recipe package)) :files)
                   files)))

(cl-defun ublt/-fork (name &key branch)
  `(:repo ,(format "github:ubolonton/%s" name)
          :host nil ; XXX: Otherwise straight tries to construct https URL.
          :remote "ubolonton"
          :branch ,branch))

(ublt/straight-add-files 'tsc
  '("core/DYN-VERSION" "core/tsc-dyn.*"))

(ublt/straight-add-files 'tree-sitter-langs
  '("langs/bin"))

(straight-register-package
 '(wat-mode :type git :host github :repo "devonsparks/wat-mode"))

(provide 'ublt-straight-recipes)
