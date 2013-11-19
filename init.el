;;; For browsing Emacs's C source. This must be set early.
(setq source-directory "~/Programming/Tools/emacs")

(add-to-list 'load-path "~/.emacs.d/config")
(require 'ublt-util)

(when (ublt/legacy?)
  (load-file "~/.emacs.d/emacs23/package.el"))

;;; Emacs is not a text editor, and here we load its package manager!
(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")
                  ("melpa" . "http://melpa.milkbox.net/packages/")
                  ))
  (add-to-list 'package-archives source t))
;;; See the end of this file for explanation of this
(defvar ublt/package-archives package-archives)
(package-initialize)

;;; Required packages
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar ublt/packages
  '(smex auto-complete yasnippet
         org textmate undo-tree whole-line-or-region
         ace-jump-mode htmlize twittering-mode keyfreq
         adaptive-wrap
         ido-ubiquitous                 ; List-narrowing UI
         helm helm-cmd-t helm-swoop
         number-font-lock-mode          ; Color numbers in code
         powerline                      ; Util helping configure mode-line
         ;; TODO: Use & combine with eproject
         projectile                     ; Project management
         emms                           ; Music
         paredit                        ; Structural editing with ()[]{}
         scpaste                        ; Publish highlighted code fragments
         exec-path-from-shell           ; Uhm, f*ck shell
         anzu                           ; Match count for search
         info+
         pabbrev                        ; TODO: Find better alternative
         ;; git
         magit magit-svn
         ;; Vim emulation
         evil surround
         ;; Appearance
         color-theme rainbow-mode page-break-lines  ;; whitespace
         diminish                       ; Mode names => symbols
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
         clojure-mode clojurescript-mode durendal cider
         elisp-slime-nav
         js2-mode flymake-jshint
         php-mode php-boris flymake-php
         rvm flymake-ruby
         web-mode
         emmet-mode                          ; html/css editing
         ;; TODO: Remove starter kit dependency
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

;; (ublt/set-up 'auto-async-byte-compile
;;   (setq auto-async-byte-compile-display-function 'bury-buffer)
;;   (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode))

(ublt/set-up 'exec-path-from-shell
  (exec-path-from-shell-initialize))

;;; General usability
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuffs
(ublt/add-path "org2blog/")
(ublt/add-path "o-blog")
(ublt/add-path "org-html-slideshow")
(ublt/set-up 'ublt-communication)
(when window-system (ublt/set-up 'ublt-entertainment))
(ublt/set-up 'ublt-organization)

;;; Might grow into a project on its own, adding more project
;;; management stuffs
(ublt/add-path "eproject")
(setq eproject-completing-read-function 'eproject--ido-completing-read
      eproject-todo-expressions '("TODO" "XXX" "FIX" "FIXME" "HACK" "NTA"))
(require 'eproject-ido-imenu)

(ublt/set-up 'projectile
  (projectile-global-mode +1))

;;; Misc customization
;;; TODO: add case toggling

;;; This stops the damned auto-pinging
(setq ffap-machine-p-known 'reject)

;;; Old buffer clean up
(ublt/set-up 'midnight
  (setq clean-buffer-list-delay-general 7             ; days
        clean-buffer-list-delay-special (* 3 24 3600) ; 3 days
        )
  (add-to-list 'desktop-locals-to-save 'buffer-display-time))

(setq tramp-default-method "ssh")

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
;; (eval-after-load "desktop"
;;   '(setq desktop-restore-eager 100
;;          desktop-lazy-idle-delay 3))

;;; XXX: DIsable for now, ibus changed dramatically (in a bad way?)
;;and `ibus-mode' could not keep up.
;; ;; Use IBus for input method `http://www.emacswiki.org/emacs/IBusMode'
;; ;; Gần được nhưng hầu hết các font fixed-width không hiện được một số chữ
;; (ublt/in '(gnu/linux)
;;   (ublt/add-path "ibus-el-0.3.2")
;;   (require 'ibus)
;;   (add-hook 'after-init-hook 'ibus-mode-on)
;;   ;; Use C-SPC for Set Mark command
;;   (ibus-define-common-key ?\C-\s nil)
;;   ;; Use C-/ for Undo command
;;   (ibus-define-common-key ?\C-/ nil))

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

;; (add-hook 'html-mode-hook (ublt/off-fn 'flyspell-mode))

;; These should be disabled for new users, not me.
(ublt/enable '(narrow-to-region set-goal-column upcase-region downcase-region))

;; Save positions in visited files
(setq-default save-place t)
(require 'saveplace)
(setq save-place-file "~/.emacs.d/.saveplace")

;; Save history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/.savehist")
(savehist-mode t)

(setq bookmark-default-file "~/.emacs.d/.bookmarks")

(setq recentf-save-file "~/.emacs.d/.recentf" )

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

;; XXX: This makes python setup unhappy. (Both use C-x p)
;; (require 'bookmark+)

;; (require 'key-chord)
;; (key-chord-mode +1)
;; (key-chord-define-global "dd" 'kill-whole-line)

(require 'ublt-evil)
(require 'ublt-dired)
(require 'ublt-ido)
;; (ublt/add-path "helm")
(require 'ublt-helm)

(require 'ublt-git)

(ublt/add-path "python")
(require 'ublt-python)

(require 'ublt-js)

(require 'ublt-flymake)


;;; Languages support ------------------------------------------------

;; Indentation in some modes
(setq css-indent-offset 2)
(add-hook 'php-mode-hook (lambda () (setq c-basic-offset 4)))

(defun turn-off-hl-line-mode ()
  (hl-line-mode nil))
;; ;; Might be useful for f*cks like PHP, JSP, ASP.NET, mako, rhtml, django
;; (ublt/add-path "nxhtml/")
;; (add-hook 'nxhtml-mode-hook (lambda () (rng-validate-mode -1)))
;; (load "autostart.el")
;; (add-to-list 'auto-mode-alist '("\\.mako?$" . mako-nxhtml-mumamo-mode))
;; (add-hook 'mako-nxhtml-mumamo-mode-hook 'esk-turn-on-hl-line-mode)
;; (add-hook 'nxhtml-mode-hook (ublt/off-fn 'auto-fill-mode))
;; (add-hook 'nxhtml-mode-hook (ublt/off-fn 'flyspell-mode))


;; Factor
(condition-case err
    (progn
      (ublt/in '(darwin)
        (load-file "/Applications/factor/misc/fuel/fu.el"))
      (ublt/in '(gnu/linux)
        (load-file "~/Programming/factor/misc/fuel/fu.el")))
  (error (message "No Factor")))

;; Scheme
(require 'quack)
(setq quack-fontify-style nil)

(require 'ublt-erlang)

(require 'ublt-haskell)

(require 'ublt-sql)

(require 'ublt-web)

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;;; Lisp, Clojure --------------------------------------

(require 'ublt-lisp)


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
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

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


;; Some packages mess up `package-archives', so restore its value
;; here. TODO: Find a better solution
(setq package-archives ublt/package-archives)
