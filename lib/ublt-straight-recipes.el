(require 'straight)

(eval-and-compile
  (require 'subr-x)
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

(cl-defun ublt/-fork (name &key branch)
  `(:repo ,(format "github:ubolonton/%s" name)
          :host nil ; XXX: Otherwise straight tries to construct https URL.
          :remote "ubolonton"
          :branch ,branch))

(defun ublt/straight-modify-recipe (package modify-plist-fn)
  (declare (indent 1))
  (let* ((old-props (cdr (ublt/straight-recipe package)))
         (new-props (funcall modify-plist-fn old-props)))
    (apply #'ublt/straight-override package new-props)))

(defun ublt/plist-append (plist key values)
  (plist-put plist key (append (plist-get plist key) values)))

(defun ublt/straight-add-files (package files)
  "Add more FILES to the recipe for PACKAGE."
  (declare (indent 1))
  (ublt/straight-modify-recipe package
    (lambda (props)
      (ublt/plist-append props :files files))))

(defmacro ublt/straight-with-modifications (package &rest forms)
  (declare (indent 1))
  `(ublt/straight-modify-recipe ,package
     (lambda (props)
       (thread-first
         props
         ,@forms))))

(ublt/straight-with-modifications 'tree-sitter
  (plist-put :local-repo "~/Programming/projects/elisp-tree-sitter"))

(ublt/straight-with-modifications 'tsc
  ;; To use locally-built dynamic module.
  (ublt/plist-append :files '("core/DYN-VERSION" "core/tsc-dyn.*"))
  (plist-put :local-repo "~/Programming/projects/elisp-tree-sitter"))

(ublt/straight-with-modifications 'tree-sitter-langs
  ;; To use locally-built grammar binaries.
  (ublt/plist-append :files '("bin"))
  (plist-put :local-repo "~/Programming/projects/tree-sitter-langs"))

(straight-register-package
 '(wat-mode :type git :host github :repo "devonsparks/wat-mode"))

(provide 'ublt-straight-recipes)
