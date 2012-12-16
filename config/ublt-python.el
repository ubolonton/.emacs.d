(require 'ublt-util)

;; The length of this section proves python support in Emacs is weak,
;; since all these are just for basic stuffs. Also Pymacs
;; initialization is very slow.
;; Try to install stuffs from official pages instead of from
;; apt (use easy_install)

;; pymacs, ropemode, ropemacs

(condition-case err
    (ublt/in '(gnu/linux darwin)
      (setq-default ;; py-shell-name          "ipython"
       ;; py-python-command      py-shell-name
       ;; py-jpython-command     py-shell-name
       ;; py-jython-command      py-shell-name
       ;; py-default-interpreter py-shell-name
       ;; python-command         py-shell-name
       py-shell-switch-buffers-on-execute nil)
      (require 'python-mode)

      (require 'ipython)
      (setq-default py-python-command "ipython"
                    py-python-command-args (list "-colors" "Linux"))
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

      (defun flymake-pyflakes-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-inplace))
               (local-file (file-relative-name
                            temp-file
                            (file-name-directory buffer-file-name))))
          (list "pyflakes" (list local-file))))
      ;; (add-hook 'find-file-hook 'flymake-find-file-hook)
      ;; From `starter-kit-ruby.el'
      (defun ublt/flymake-python-enable ()
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
          (flymake-mode t)))
      (defun ublt/comint-preoutput-clear-^A^B (string)
        "Clears the ^A^B strings that somehow get into ipython input
prompt returned to comint."
        (replace-regexp-in-string "[]" "" string))
      (defun ublt/use-py-imenu-support ()
        ;; py-imenu-create-index-function =>
        ;; py-imenu-create-index-new some where between 5 & 6
        (setq imenu-create-index-function #'py-imenu-create-index-function))
      (defun ublt/turn-on-ropemacs-mode ()
        (when (and buffer-file-name
                   (string-match "\\.py$" buffer-file-name))
          (ropemacs-mode +1)))
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

      (require 'flymake)
      (add-to-list 'flymake-allowed-file-name-masks
                   '("\\.py\\'" flymake-pyflakes-init))
      (remove-hook 'python-mode-hook 'ropemacs-mode)
      (add-hook 'python-mode-hook 'ublt/set-python-tab)
      (add-hook 'python-mode-hook 'ublt/turn-on-ropemacs-mode)
      (add-hook 'python-mode-hook 'ublt/flymake-python-enable)
      (add-hook 'python-mode-hook 'esk-prog-mode-hook t)
      (add-hook 'python-mode-hook 'enable-paredit-mode t)
      ;; python.el use `semantic' to provide `imenu' support, we need to override
      (add-hook 'python-mode-hook 'ublt/use-py-imenu-support t)
      (add-hook 'comint-preoutput-filter-functions
                'ublt/comint-preoutput-clear-^A^B)

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

      )
  (error (message "No python: %s" err)))



(ublt/in '()
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defun prefix-list-elements (list prefix)
    (let (value)
      (nreverse
       (dolist (element list value)
         (setq value (cons (format "%s%s" prefix element) value))))))
  (defvar ac-source-rope
    '((candidates
       . (lambda ()
           (prefix-list-elements (rope-completions) ac-target))))
    "Source for Rope")
  (defun ac-python-find ()
    "Python `ac-find-function'."
    (require 'thingatpt)
    (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
      (if (null symbol)
          (if (string= "." (buffer-substring (- (point) 1) (point)))
              (point)
            nil)
        symbol)))
  (defun ac-python-candidate ()
    "Python `ac-candidates-function'"
    (let (candidates)
      (dolist (source ac-sources)
        (if (symbolp source)
            (setq source (symbol-value source)))
        (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
               (requires (cdr-safe (assq 'requires source)))
               cand)
          (if (or (null requires)
                  (>= (length ac-target) requires))
              (setq cand
                    (delq nil
                          (mapcar (lambda (candidate)
                                    (propertize candidate 'source source))
                                  (funcall (cdr (assq 'candidates source)))))))
          (if (and (> ac-limit 1)
                   (> (length cand) ac-limit))
              (setcdr (nthcdr (1- ac-limit) cand) nil))
          (setq candidates (append candidates cand))))
      (delete-dups candidates)))
  (add-hook 'python-mode-hook
            (lambda ()
              (auto-complete-mode 1)
              (set (make-local-variable 'ac-sources)
                   (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
              (set (make-local-variable 'ac-find-function) 'ac-python-find)
              (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
              (set (make-local-variable 'ac-auto-start) nil)))
  ;;Ryan's python specific tab completion
  (defun ryan-python-tab ()
                                        ; Try the following:
                                        ; 1) Do a yasnippet expansion
                                        ; 2) Do a Rope code completion
                                        ; 3) Do an indent
    (interactive)
    (if (eql (ac-start) 0)
        (indent-for-tab-command)))
  (defadvice ac-start (before advice-turn-on-auto-start activate)
    (set (make-local-variable 'ac-auto-start) t))
  (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
    (set (make-local-variable 'ac-auto-start) nil))
  (define-key py-mode-map "\t" 'ryan-python-tab)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; End Auto Completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

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

(provide 'ublt-python)
