;;; eproject-ido-imenu.el --- navigate project's symbols
;;
;; Copyright (C) 2011 Nguyễn Tuấn Anh <ubolonton@gmail.com>
;;
;; Author: Nguyễn Tuấn Anh <ubolonton@gmail.com>
;; Maintainer: Nguyễn Tuấn Anh <ubolonton@gmail.com>
;; Created: 7 Aug 2011
;; Version: 0.1
;; Keywords: navigation, projects, convenience
;;
;; This file is not a part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.
;;
;;; Commentary:
;;
;; This provides a simple integration of ido, imenu and eproject. It
;; is similar to emacs-starter-kit's `ido-menu', but uses (jrockway's)
;; eproject to search all opened buffers of the current project.
;;
;; For best result, set ido to use flex matching, and customize
;; `ido-decorations' so that candidates are shown vertically.
;;
;; Configuration:
;;
;; (require 'eproject-ido-imenu)
;; (ido-mode t)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-max-prospects 10)
;; (setq ido-decorations (quote ("\n=> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;;
;; And possibly more sensible bindings:
;;
;; (eval-after-load "ido"
;;   '(add-hook 'ido-setup-hook
;;              (lambda ()
;;                (define-key ido-completion-map (kbd "<tab>")  'ido-complete)
;;                (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
;;                (define-key ido-completion-map (kbd "<up>")   'ido-prev-match))))
;;
;; Usage: M-x eproject-ido-imenu
;;
;; FIX:
;; - It seems to do something weird with marks.
;;
;; TODO:
;; - Parse all relevant files, not just opened one (use the cached
;;   index to improve performance).
;;
;; - Show namespace/module not buffer name, e.g. "clojure.core/map",
;;   highlight in using appropriate color.
;;
;; - Follow-mode à la `helm'.
;;
;; - Highlight based on symbol types (seems impossible since imenu does
;;   not give that info).
;;
;; - Argument lists for functions (see above)
;;
;; - Find a better way to deal with imenu's nested levels
;;
;; - Inline drop-down à la autocomplete (might as well use it instead
;;   of ido).

;;; Code:

(require 'ido)
(require 'imenu)
(require 'eproject-extras)

(eval-when-compile
  (require 'cl))

(defgroup ubolonton nil ""
  :group 'personal)

(defface eproject-ido-imenu-file-path
  `((t (:inherit font-lock-builtin-face)))
  "Face used to display the file paths associated with symbols in
  `eproject-ido-imenu's prompt. For some languages/project types
  they map to namespaces/modules.")

;; This and the next function were broken up from emacs-starter-kit
;; `ido-menu'
(defun eproject--process-imenu-index (imenu-index &optional prefix)
  "Convert an imenu-index-alist to a format usable by
`eproject--ido-symbols', attaching PREFIX to symbol names."
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (imenu-index &optional prefix)
                       (when (listp imenu-index)
                         (dolist (symbol imenu-index)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol (concat prefix (propertize (car symbol)
                                                                             'face 'eproject-ido-imenu-file-path)
                                                          " ")))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             ;; We also want buffer, not just position
                             (unless (markerp position)
                               (setq position (set-marker (make-marker) position)))

                             (unless (or (null position) (null name) (equal symbol imenu--rescan-item))
                               (add-to-list 'symbol-names (concat prefix name))
                               (add-to-list 'name-and-pos (cons (concat prefix name) position))))))))
      (addsymbols imenu-index prefix))
    (list symbol-names name-and-pos)))

(defun eproject--ido-symbols (ido-index)
  "Use ido to select symbol and go to their position, taking a
list of 2 lists: 1 containing symbol names, the other contains
symbol-to-position mappings."
  (let ((symbol-names (car ido-index))
        (name-and-pos (cadr ido-index)))
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (marker (cdr (assoc selected-symbol name-and-pos))))
      (switch-to-buffer (marker-buffer marker))
      (goto-char (marker-position marker)))))

(defun eproject--make-imenu-alist (&optional project-root)
  "Build an imenu index containing symbols in opened files of
the (current) project defined by eproject. The index is then
converted to a form that `eproject--ido-symbols' can use."
  (let ((index '()))
    (with-each-buffer-in-project (buffer project-root)
        (condition-case err
            (let ((separators (or (eproject-attribute :symbol-separators) '("/" " "))))
              (add-to-list 'index
                           (eproject--process-imenu-index
                            ;; TODO: Don't simply use buffer name. Use
                            ;; namespace/module or whatever, depending
                            ;; on project's type
                            (imenu--make-index-alist)
                            ;; imenu--index-alist
                            (propertize
                             (concat (replace-regexp-in-string "\/" (car separators) ; Namespace path separator
                                                               ;; TODO: Define `eproject--current-namespace'
                                                               (file-name-sans-extension (car (eproject--shorten-filename (buffer-file-name)))))
                                     (nth 1 separators)) ; Namespace/symbol separator
                             'face 'eproject-ido-imenu-file-path))))
          ;; XXX: Seems like a bad idea to ignore all errors?
          (error nil)))
    ;; XXX: Looks convoluted, I don't know my way around ELisp functions well
    (list (apply #'append (mapcar #'car index))
          (apply #'append (mapcar #'cadr index)))))

;;; XXX: This was removed from emacs-starter-kit since in favor of
;;; "omni-ido", but that thing has problem with imenu nesting menus
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
  Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil (mapcar (lambda (symbol)
                                                     (if (string-match regexp symbol) symbol))
                                                   symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol) (setq symbol-names (cons symbol (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun eproject-ido-imenu ()
  "Use ido to select imenu candidates (symbols) from opened
  buffers of the current project, as defined by eproject. Fall
  back to `ido-imenu' if the buffer is not associated with any
  project."
  (interactive)
  (condition-case err
      (progn
        (eproject--ido-symbols
         (eproject--make-imenu-alist))
        (recenter)) ; TODO: Is recentering always good?
    ;; XXX: Using `ido-menu' requires emacs-starter-kit. Find
    ;; another way to fall back instead
    (error
     (call-interactively 'ido-imenu))))

;;; TODO: Honor .gitignore
(define-project-type warp (generic)
  (or (look-for "warpconfig.py") (look-for "warpconfig.py.sample"))
  :symbol-separators ("." ".")
  :relevant-files (".*\.py$" ".*\.mak$" ".*\.css$" ".*\html$")
  :irrelevant-files (".*__init__\.py"))
(define-project-type clojure (generic)
  (look-for "project.clj")
  :symbol-separators ("." "/")
  :relevant-files (".*\.clj$" ".*\.cljs$")
  ;; :irrelevant-files (".*\.jar$" ".*\.js$" "^resources" "^target")
  :irrelevant-files ("out" "target" "resources")
  )
(define-project-type yii (generic)
  (or (look-for "yiic") (look-for "yiic.bat")(look-for "yiic.php"))
  :symbol-separators ("." "/")
  :relevant-files (".*\.php$")
  :irrelevant-files ("vendors" "extensions" "tests" "scripts"))
(define-project-type erlang-rebar (generic)
  (look-for "rebar.config")
  :symbol-separators ("." " ")
  :relevant-files (".*\.[h|e]rl$")
  :irrelevant-files (".*\.beam$" "rel" "test" "ebin" "priv"))
;;; Not exactly type
(define-project-type require.js (generic)
  (look-for "require-jquery.js")
  :symbol-separators ("." ".")
  :irrelevant-files ("libs" "require-jquery\.js"))

;;; XXX: ???
(defun eproject-find--combine-regexps (regexp-list)
  (format "\(%s\)"
          (reduce (lambda (a b) (concat a "\|" b))
                  (mapcar (lambda (f) (format "\(%s\)" f)) regexp-list))))
(defun eproject--combine-regexps (regexp-list)
  "Combine regexps like `regexp-opt', but without quoting anything.
Argument REGEXP-LIST is a list of regexps to combine."
  (format "\\(?:%s\\)"
          (reduce (lambda (a b) (concat a "\\|" b))
                  (mapcar (lambda (f) (format "\\(?:%s\\)" f)) regexp-list))))

(provide 'eproject-ido-imenu)
