(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

(when (ublt/legacy?)
 (load-file "~/.emacs.d/emacs23/package.el"))

;; ;;; XXX: elnode depends on this
;; (add-to-list 'load-path "~/.emacs.d/lib/apel")

;; ;;; HACK XXX: FLIM breaks this (no mailcap-parse-mailcaps)
;; (when (eql system-type 'gnu/linux)
;;   (if (ublt/legacy?)
;;       (load-file "/usr/share/emacs/23.3/lisp/gnus/mailcap.elc")
;;     (load-file "/usr/local/share/emacs/24.0.93/lisp/gnus/mailcap.elc")))

;;; Emacs is not a text editor, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ;; TODO: Maybe, use this after emacs24 is released
                  ;; (development versions of packages)
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
(package-initialize)

;;; Required packages
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar ublt/packages
  '(smex auto-complete ac-slime ido-ubiquitous yasnippet helm
         org textmate paredit undo-tree whole-line-or-region
         bookmark+ ace-jump-mode htmlize twittering-mode keyfreq
         scpaste
         ;; Uhm, f*ck shell
         exec-path-from-shell
         info+ pabbrev
         ;; Vim emulation
         evil surround
         ;; Appearance
         color-theme rainbow-mode pp-c-l diminish ;; whitespace
         highlight highlight-symbol hl-line+ idle-highlight-mode volatile-highlights
         ;; Don't actually use these themes, just to learn some ideas
         color-theme-solarized zenburn
         ;; Dired
         dired-details dired-details+
         dired+
         ;; Code folding
         fold-dwim fold-dwim-org hideshowvis
         ;; Languages
         flymake
         haskell-mode quack
         markdown-mode less-css-mode yaml-mode
         clojure-mode clojurescript-mode durendal swank-clojure
         elisp-slime-nav
         js2-mode flymake-jshint
         php-mode flymake-php
         ;; Starter kit
         starter-kit starter-kit-bindings starter-kit-eshell
         starter-kit-lisp starter-kit-js starter-kit-ruby))
(dolist (p ublt/packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;; XXX: Some starter-kit packages are broken
(defalias 'run-coding-hook 'esk-prog-mode-hook)
(defalias 'esk-run-coding-hook 'esk-prog-mode-hook)

;;; NOTE: As my stuffs may depend on packages loaded after
;;; starter-kit, it does not make sense to let starter-kit load my
;;; stuffs. Thus my config is in ~/.emacs.d/init.el, not
;;; ~/.emacs.d/ubolonton/init.el. And don't ever choose "elpa" as your
;;; user name =))

;;; Clipboard integration for old version
(when (ublt/legacy?)
  (eval-after-load "ublt-dvorak"
    '(progn
       (ublt/define-keys
        global-map
        "M-w" 'clipboard-kill-ring-save
        "C-w" 'clipboard-kill-region
        "C-y" 'clipboard-yank)
       (setq x-select-enable-clipboard t))))



;;; TODO: Use this (from Emacs prelude)
;; (defun prelude-add-subfolders-to-load-path (parent-dir)
;;   "Adds all first level `parent-dir' subdirs to the
;; Emacs load path."
;;   (dolist (f (directory-files parent-dir))
;;     (let ((name (concat parent-dir f)))
;;       (when (and (file-directory-p name)
;;                  (not (equal f ".."))
;;                  (not (equal f ".")))
;;         (add-to-list 'load-path name)))))

;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

(ublt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "CLOJURESCRIPT_HOME"))

;;; General usability
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuffs
(ublt/add-path "emms/lisp/")
(ublt/add-path "org2blog/")
(ublt/set-up 'ublt-communication)
(when window-system (ublt/set-up 'ublt-entertainment))
(ublt/set-up 'ublt-organization)

;;; Might grow into a project on its own, adding more project
;;; management stuffs
(ublt/add-path "eproject")
(setq eproject-completing-read-function 'eproject--ido-completing-read
      eproject-todo-expressions '("TODO" "XXX" "FIX" "FIXME" "HACK" "NTA"))
(require 'eproject-ido-imenu)

;;; Misc customization
;;; TODO: add case toggling

(ublt/in '(gnu/linux)
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(ublt/add-path "emacs-skype")
;;; XXX: Disable for now, since Skype is f**king unstable
;; (require 'skype)
;; (skype--init)
(setq skype--my-user-handle "ubolonton")

(unless (ublt/legacy?)
  ;; Uhm but why do we need it turned on on 24?
  (setq stack-trace-on-error t))

;; nxhtml seems to byte-compile a lot while running, and uses some
;; obsolete stuffs. This is to prevent warning pop-ups (especially in
;; mako files)
(setq byte-compile-warnings '(not obsolete free-vars))
(eval-after-load "js"
  '(defvar javascript-mode-syntax-table js-mode-syntax-table))

;; `http://www.emacswiki.org/emacs/DeskTop#toc6'
;; (desktop-save-mode +1)
(defadvice desktop-create-buffer (around ignore-errors activate)
  (condition-case err
      ad-do-it
    (error (message "desktop-create-buffer: %s" err))))
(defun ublt/session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (y-or-n-p "Restore desktop? ")
      (desktop-read))
  (desktop-save-mode +1)
  (add-to-list 'desktop-modes-not-to-save 'highlight-parentheses-mode))
;; Ask user whether to restore desktop at start-up
(add-hook 'after-init-hook 'ublt/session-restore t)

;; Use IBus for input method `http://www.emacswiki.org/emacs/IBusMode'
;; Gần được nhưng hầu hết các font fixed-width không hiện được một số chữ
(ublt/in '(gnu/linux)
  (ublt/add-path "ibus-el-0.2.1")
  (require 'ibus)
  (add-hook 'after-init-hook 'ibus-mode-on)
  ;; Use C-SPC for Set Mark command
  (ibus-define-common-key ?\C-\s nil)
  ;; Use C-/ for Undo command
  (ibus-define-common-key ?\C-/ nil))

;; Command statistics
;; FIXME: Prune #'s from table to make it work
;; (require 'command-frequency)
;; (setq command-frequency-table-file "~/.emacs.d/cmd_frequencies")
;; (command-frequency-table-load)
;; (command-frequency-mode 1)
;; (command-frequency-autosave-mode 1)
;; (ublt/set-up 'keyfreq
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; Some modes do not need those
(defun turn-off-auto-fill-mode ()
  (auto-fill-mode -1))
(defun turn-off-flyspell-mode ()
  (flyspell-mode -1))
(add-hook 'html-mode-hook 'turn-off-auto-fill-mode)
;; (add-hook 'html-mode-hook 'turn-off-flyspell-mode)

;; These should be disabled for new users, not me.
(ublt/enable '(narrow-to-region set-goal-column upcase-region downcase-region))

;; Save positions in visited files
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.places")

;; Save history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.savehist")
(savehist-mode t)

;; TextMate minor mode
(require 'textmate)
;; (textmate-mode)

;; TODO: Use this
;; ECB, CEDET
;; (ublt/add-path "ecb/")
;; (require 'ecb)
;; (setq ecb-windows-width 40)

;; pabbrev
;; (require 'pabbrev)
;; (put 'python-mode 'pabbrev-global-mode-excluded-modes t)
;; (global-pabbrev-mode +1)

;;; TODO: Hack on this project
;; epresent
(ublt/add-path "epresent")
(require 'epresent)

;; TODO: This makes python setup unhappy. Figure out why
;; (ublt/add-path "bookmark-plus")
;; (require 'bookmark+)

;; (require 'key-chord)
;; (key-chord-mode +1)
;; (key-chord-define-global "dd" 'kill-whole-line)

(ublt/set-up 'volatile-highlights
  (volatile-highlights-mode +1))

;;; Evil -------------------------------------------------------------
(require 'ublt-evil)

;;; Paredit ----------------------------------------------------------
(require 'paredit)
;; (defun ublt/enable-paredit-mode ()
;;   "Enable paredit-mode without checking paren balance."
;;   (let ((current-prefix-arg t))
;;     (paredit-mode +1)))
;; XXX: Seems unclean
(defadvice paredit-mode (around force activate)
  (if (eq major-mode 'python-mode)
      (let ((current-prefix-arg t))
        ad-do-it)
    ad-do-it))
(defun ublt/paredit-space-for-open? (endp delimiter)
  "Don't insert space for ( [ \" in these modes."
  (not (and (member major-mode '(comint-mode python-mode javascript-mode js-mode js2-mode))
            (member delimiter '(?\( ?\[ ?\")))))
(eval-after-load "paredit"
  '(add-to-list 'paredit-space-for-delimiter-predicates
                'ublt/paredit-space-for-open?))
;;; Since I use paredit in many modes, it's better to use its
;;; comment-dwim only in lisp modes
(defadvice comment-dwim (around lisp-specific activate)
  (if (member major-mode '(lisp-mode emacs-lisp-mode clojure-mode scheme-mode))
      (call-interactively 'paredit-comment-dwim)
    (message "normal")
    ad-do-it))

;;; Dired ------------------------------------------------------------
(require ublt-dired)

;;; ido --------------------------------------------------------------
(require 'ublt-ido)

;;; Git --------------------------------------------------------------
(ublt/add-path "magit")
(ublt/add-path "git-emacs")
(require 'ublt-git)

;;; Quicksilver/Spotlight for Emacs ----------------------------------
(require 'ublt-helm)

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/data/auto-complete/dict")
(global-auto-complete-mode +1)
;; (add-hook 'eshell-mode-hook 'ac-eshell-mode-setup)
(setq-default ac-auto-start nil
              ac-sources '(ac-source-yasnippet
                           ac-source-dictionary
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-words-in-all-buffer
                           ac-source-abbrev))
(setq ac-delay 0.5
      ac-auto-show-menu 1
      ac-quick-help-delay 0.8)

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode text-mode haml-mode
                sass-mode yaml-mode csv-mode espresso-mode haskell-mode
                html-mode nxml-mode sh-mode smarty-mode clojure-mode
                lisp-mode textile-mode markdown-mode tuareg-mode
                nxhtml-mode))
  (add-to-list 'ac-modes mode))


;;; Yasnippet --------------------------------------------------------

(ublt/set-up 'yasnippet
  (setq yas/root-directory  "~/.emacs.d/data/yasnippet/snippets"
        yas/prompt-functions '(yas/dropdown-prompt ;; yas/ido-prompt yas/no-prompt
                                                   )
        yas/trigger-key nil)
  (add-to-list 'hippie-expand-try-functions-list 'yas/hippie-try-expand)
  (yas/load-directory yas/root-directory)
  (yas/global-mode +1))

;;; Languages support ------------------------------------------------

;; Indentation in some modes
(setq css-indent-offset 2)
(setq espresso-indent-level 2)
(setq js-indent-level 2)
(add-hook 'php-mode-hook (lambda () (setq c-basic-offset 4)))

;; Might be useful for f*cks like PHP, JSP, ASP.NET, mako, rhtml, django
(ublt/add-path "nxhtml/")
(add-hook 'nxhtml-mode-hook (lambda () (rng-validate-mode -1)))
(load "autostart.el")
(defun turn-off-hl-line-mode ()
  (hl-line-mode nil))
(add-to-list 'auto-mode-alist '("\\.mako?$" . mako-nxhtml-mumamo-mode))
(add-hook 'mako-nxhtml-mumamo-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'nxhtml-mode-hook 'turn-off-auto-fill-mode)
;; (add-hook 'nxhtml-mode-hook 'turn-off-flyspell-mode)


;; Javascript (it seems js-mode in Emacs is newer than espresso)
;; v8: scons via apt-get (not pip or easy_install; among other things,
;; build tool & package manager is what clojure gets absolutely right,
;; whereas python sucks ass).
;; Actually screw that, use virtualenv and easy_install scons, as
;; described here https://github.com/koansys/jshint-v8. But remember
;; to do
;;
;; export SCONS_LIB_DIR=/path/to/virtual-scons-egg/scons-sth
;;
;; scons console=readline snapshot=on library=shared d8
;;
;; Well it's still complains about missing libv8.so. Just install
;; "node" then.
;;
;; MozRepl integration
;;
;; (defalias 'javascript-mode 'espresso-mode)
;; (setq js-mode-hook '())
;; (setq flymake-jslint-command "jslint")
(when (ublt/legacy?)
  (add-hook 'js-mode-hook 'esk-prog-mode-hook))
(eval-after-load "js2-mode"
  '(progn
     (add-hook 'js2-mode-hook 'esk-prog-mode-hook)
     (add-hook 'js2-mode-hook 'esk-paredit-nonlisp)
     (add-hook 'js2-mode-hook 'moz-minor-mode)
     (defalias 'javascript-mode 'js2-mode)
     ;; XXX: Copied from starter-kit-js
     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords
      'js2-mode `(("\\(function *\\)("
                   (0 (progn (compose-region (match-beginning 1)
                                             (match-end 1) "\u0192")
                             nil)))))
     (setcdr (assoc "\\.js\\'" auto-mode-alist)
             'js2-mode)
     ))
(add-hook 'js-mode-hook 'moz-minor-mode)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; Factor
(condition-case err
    (progn
(ublt/in '(darwin)
  (load-file "/Applications/factor/misc/fuel/fu.el"))
(ublt/in '(gnu/linux)
        (load-file "~/Programming/factor/misc/fuel/fu.el")))
  (error (message "No Factor")))

;; Haskell
(ublt/set-up 'haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode)))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Scheme
(require 'quack)
(setq quack-fontify-style nil)

;; Erlang
(ublt/in '(gnu/linux)
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/erlang")
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (require 'erlang-start nil t)
  (defun ublt/erlang-compile-and-display ()
    (interactive)
    (call-interactively 'erlang-compile)
    (call-interactively 'erlang-compile-display))
  (add-hook 'erlang-mode-hook 'esk-prog-mode-hook)
  )

(ublt/set-up 'less-css-mode
  (add-hook 'less-css-mode-hook 'esk-prog-mode-hook)
  (add-hook 'less-css-mode-hook 'enable-paredit-mode))
(ublt/set-up 'css-mode
  (add-hook 'css-mode-hook 'esk-prog-mode-hook)
  (add-hook 'css-mode-hook 'enable-paredit-mode))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; SLIME, Common Lisp, Clojure --------------------------------------

(ublt/add-path "slime")
(ublt/add-path "slime/contrib")
(ublt/add-path "swank-clojure-extra")
(require 'ublt-lisp)


;;; Flymake ----------------------------------------------------------

(require 'flymake)
;; (defun flymake-php-init ()
;;   (let* ((temp (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;;          (local (file-relative-name temp (file-name-directory buffer-file-name))))
;;     (list "php" (list "-f" local "-l"))))
;; (add-to-list 'flymake-err-line-patterns
;;   '("\\(Parse\\|Fatal\\) error: +\\(.*?\\) in \\(.*?\\) on line \\([0-9]+\\)$" 3 4 nil 2))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.php$" flymake-php-init))
;; (add-hook 'php-mode-hook 'enable-flymake)

(defun ublt/flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(defface ublt/flymake-message-face
  `((t (:inherit font-lock-keyword-face)))
  "Face for flymake message echoed in the minibuffer.")
(defun ublt/flymake-err-echo ()
  "Echo flymake error message in the minibuffer (not saving to *Messages*)."
  (ublt/status-message "%s"
             (propertize (mapconcat 'identity
                                    (ublt/flymake-err-at (point)) "\n")
                         'face 'ublt/flymake-message-face)))

(defadvice flymake-goto-next-error (after display-message activate)
  (ublt/flymake-err-echo))
(defadvice flymake-goto-prev-error (after display-message activate)
  (ublt/flymake-err-echo))

(eval-after-load "js"
  '(ublt/set-up 'flymake-jshint
     (setq jshint-configuration-path "~/.jshint.json")
     (defun ublt/flymake-js-enable ()
       (when (and buffer-file-name
                  (string-match "\\.js$" buffer-file-name))
         (flymake-mode +1)))
     (remove-hook 'js-mode-hook 'flymake-mode)
     (add-hook 'js-mode-hook 'ublt/flymake-js-enable)))

(eval-after-load "php-mode"
  '(ublt/set-up 'flymake-php
     (add-hook 'php-mode-hook 'flymake-php-load)))

(defun enable-flymake () (flymake-mode 1))
(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook #'enable-flymake))

;;; Python -----------------------------------------------------------
(require 'ublt-python)

;; Who the hell set it to t?
(setq debug-on-error nil)

;;; Misc stuff I use -------------------------------------------------

;; (defun ublt/pretty-org ()
;;   (font-lock-add-keywords
;;    nil `(("\\(#\\\+begin_src\\>\\)"
;;           (0 (progn (compose-region (match-beginning 1) (match-end 1)
;;                                     ">>>>")
;;                     nil))))))

;; Kill processes
(ublt/in '(darwin gnu/linux)
  (require 'vkill)
  ;; Notifications
  (ublt/set-up 'todochiku))

;; TODO: move to corresponding mode sections
;; .rjs file is ruby file
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(setq-default ruby-indent-level 4)

(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; FIXME: Make it support mp3 not only ogg
(require 'lyric-mode)

;; Devilspie's config
(add-to-list 'auto-mode-alist '("\\.ds$" . lisp-mode))

;; Conkeror as default browser
(ublt/in '(gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror"))

(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

(condition-case nil
    (load-file "~/.emacs.d/config/ublt-personal.el")
  (error nil))

;;; `http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/'

;; (eval-when-compile
;;   (require 'cl))

;; (defun get-buffers-matching-mode (mode)
;;   "Returns a list of buffers where their major-mode is equal to MODE"
;;   (let ((buffer-mode-matches '()))
;;    (dolist (buf (buffer-list))
;;      (with-current-buffer buf
;;        (if (eq mode major-mode)
;;            (add-to-list 'buffer-mode-matches buf))))
;;    buffer-mode-matches))

;; (defun multi-occur-in-this-mode ()
;;   "Show all lines matching REGEXP in buffers with this major mode."
;;   (interactive)
;;   (multi-occur
;;    (get-buffers-matching-mode major-mode)
;;    (car (occur-read-primary-args)))


(setq custom-file "~/.emacs.d/custom.el")
(condition-case err
(load custom-file)
  (error (message "Error loading custom file")))

;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (server-start)
  (error (message "Could not start server")))
