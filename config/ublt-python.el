(require 'ublt-util)

;; The length of this section proves python support in Emacs is weak,
;; since all these are just for basic stuffs. Also Pymacs
;; initialization is very slow.
;; Try to install stuffs from official pages instead of from
;; apt (use easy_install)

;; pymacs, ropemode, ropemacs

(require 'python-mode)
(setq-default py-shell-switch-buffers-on-execute nil)

(require 'ipython)
(setq-default py-python-command "ipython"
              py-python-command-args (list "--colors" "Linux"))
(setq ipython-completion-command-string
      "print(';'.join(__IP.Completer.all_completions('%s')))\n")

(require 'pymacs)
;; Bug in `python-mode'. They use defalias which is intended for
;; functions, not variables
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
(add-to-list 'interpreter-mode-alist'("python" . python-mode))

(let ((pymacs-timeout-at-start 300))
  (pymacs-load "ropemacs" "rope-"))
(setq ropemacs-enable-autoimport t
      ropemacs-guess-project t)
(ac-ropemacs-setup)


(defun ublt/comint-preoutput-clear-^A^B (string)
  "Clears the ^A^B strings that somehow get into ipython input
prompt returned to comint."
  (replace-regexp-in-string "[]" "" string))
(defun ublt/use-py-imenu-support ()
  ;; py-imenu-create-index-function =>
  ;; py-imenu-create-index-new some where between 5 & 6
  (setq imenu-create-index-function #'py-imenu-create-index-function))
(defadvice ropemacs-mode (around py-only activate)
  (when (and buffer-file-name
             (string-match "\\.py$" buffer-file-name))
    ad-do-it))
(defun ublt/set-python-tab ()
  (setq tab-width 4))
(defadvice py-shell (around set-path activate)
  (let ((env (getenv "PYTHONPATH"))
        (project-root (condition-case nil (eproject-root)
                        (error default-directory))))
    (when project-root
      (setenv "PYTHONPATH" (format "%s:%s" project-root env)))
    ad-do-it
    (setenv "PYTHONPATH" env)))


(remove-hook 'python-mode-hook 'ropemacs-mode)
(add-hook 'python-mode-hook 'ublt/set-python-tab)
;; (add-hook 'python-mode-hook (ublt/on-fn 'ropemacs-mode))
(add-hook 'python-mode-hook 'esk-prog-mode-hook t)
(add-hook 'python-mode-hook 'enable-paredit-mode t)
;; python.el use `semantic' to provide `imenu' support, we need to override
(add-hook 'python-mode-hook 'ublt/use-py-imenu-support t)
(add-hook 'comint-preoutput-filter-functions
          'ublt/comint-preoutput-clear-^A^B)


;;; Syntax checking
(eval-after-load "flymake"
  '(progn
     (defun flymake-pyflakes-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "pyflakes" (list local-file))))
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pyflakes-init))

     ;; From `starter-kit-ruby.el'
     (defun ublt/flymake-python-maybe-enable ()
       (when (and buffer-file-name
                  ;; flymake and mumamo are at odds, so look at buffer
                  ;; name instead of `major-mode' when deciding whether
                  ;; to turn this on
                  (string-match "\\.py$" buffer-file-name)
                  (file-writable-p
                   (file-name-directory buffer-file-name))
                  (file-writable-p buffer-file-name)
                  (if (fboundp 'tramp-list-remote-buffers)
                      (not (subsetp
                            (list (current-buffer))
                            (tramp-list-remote-buffers)))
                    t))
         (flymake-mode +1)))

     (remove-hook 'python-mode-hook 'flymake-mode)
     (add-hook 'python-mode-hook 'ublt/flymake-python-maybe-enable)))



;; ============================================================
;; `http://taesoo.org/Opensource/Pylookup'
;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
(setq pylookup-dir "~/.emacs.d/lib/pylookup")
(add-to-list 'load-path pylookup-dir)
;; load pylookup when compile time
(eval-when-compile (require 'pylookup))

;; ;; set executable file and db file
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

;; ;; to speedup, just load it on demand
(autoload 'pylookup-lookup "pylookup"
  "Lookup SEARCH-TERM in the Python HTML indexes." t)
(autoload 'pylookup-update "pylookup"
  "Run pylookup-update and create the database at `pylookup-db-file'." t)

(defvar ac-source-rope
  '((candidates . (lambda () (prefix-list-elements (rope-completions) ac-target))))
  "Source for Rope")
(defun set-up-rope-ac ()
  (interactive)
  (setq ac-sources (add-to-list 'ac-sources 'ac-source-yasnippet)))
(add-hook 'python-mode-hook 'set-up-rope-ac)


;;; XXX HACK: This is a bug since 23.1 which seemed to have been fixed
;;; in 24
(when (ublt/legacy?)
  (eval-after-load "gud"
    '(defun pdb (command-line)
       "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
       (interactive
        (list (gud-query-cmdline 'pdb)))

       (gud-common-init command-line nil 'gud-pdb-marker-filter)
       (set (make-local-variable 'gud-minor-mode) 'pdb)

       (gud-def gud-break  "break %d%f:%l"  "\C-b" "Set breakpoint at current line.")
       (gud-def gud-remove "clear %d%f:%l"  "\C-d" "Remove breakpoint at current line")
       (gud-def gud-step   "step"         "\C-s" "Step one source line with display.")
       (gud-def gud-next   "next"         "\C-n" "Step one line (skip functions).")
       (gud-def gud-cont   "continue"     "\C-r" "Continue with display.")
       (gud-def gud-finish "return"       "\C-f" "Finish executing current function.")
       (gud-def gud-up     "up"           "<" "Up one stack frame.")
       (gud-def gud-down   "down"         ">" "Down one stack frame.")
       (gud-def gud-print  "p %e"         "\C-p" "Evaluate Python expression at point.")
       ;; Is this right?
       (gud-def gud-statement "! %e"      "\C-e" "Execute Python statement at point.")

       ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
       (setq comint-prompt-regexp "^(Pdb) *")
       (setq paragraph-start comint-prompt-regexp)
       (run-hooks 'pdb-mode-hook))))

(add-to-list 'Info-directory-list "~/.emacs.d/lib/python")
(require 'info-look)
(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]]+"
 :doc-spec
 '(("(python)Index" nil "")))

;;; TODO: Remove pymacs/ropemacs/ropemode?
;; (ublt/set-up 'jedi
;;   (add-hook 'python-mode-hook 'jedi:setup))

(provide 'ublt-python)
