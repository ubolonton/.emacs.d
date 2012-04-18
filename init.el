;;; This is the first thing to get loaded.

;;; nxhtml is having troubles with emacs 24, so I have to use both 24
;;; & 23 now
(defun ublt/legacy? ()
  (< emacs-major-version 24))

(when (ublt/legacy?)
 (load-file "~/.emacs.d/emacs23/package.el"))

;; ;;; XXX: elnode depends on this
;; (add-to-list 'load-path "~/.emacs.d/lib/apel")

;;; HACK XXX: FLIM breaks this (no mailcap-parse-mailcaps)
(when (eql system-type 'gnu/linux)
  (if (ublt/legacy?)
      (load-file "/usr/share/emacs/23.2/lisp/gnus/mailcap.elc")
    (load-file "/usr/local/share/emacs/24.0.94/lisp/gnus/mailcap.elc")))

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
         bookmark+ evil ace-jump-mode htmlize twittering-mode keyfreq
         ;; Appearance
         color-theme hl-line+ rainbow-mode pp-c-l idle-highlight-mode diminish volatile-highlights
         ;; Don't actually use these themes, just to learn some ideas
         color-theme-solarized color-theme-zenburn
         ;; Dired
         dired-details dired-details+
         ;; Code folding
         fold-dwim fold-dwim-org hideshowvis
         ;; Languages
         markdown-mode php-mode haskell-mode
         clojure-mode clojurescript-mode durendal swank-clojure
         elisp-slime-nav
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
  (ublt/define-keys
   global-map
   "M-w" 'clipboard-kill-ring-save
   "C-w" 'clipboard-kill-region
   "C-y" 'clipboard-yank))


(defun ublt/add-path (path)
  "Add to load-path a path relative to ~/.emacs.d/lib/"
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" path)))

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

(add-to-list 'load-path "~/.emacs.d/config")
;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

(require 'ublt-util)

;;; General usability
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuffs
(ublt/add-path "emms/lisp/")
(ublt/add-path "org2blog/")
(require 'ublt-communication)
(require 'ublt-entertainment)
(require 'ublt-organization)

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
(ublt/set-up 'keyfreq
(keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; Some modes do not need those
(defun turn-off-auto-fill-mode ()
  (auto-fill-mode -1))
(defun turn-off-flyspell-mode ()
  (flyspell-mode -1))
(add-hook 'html-mode-hook 'turn-off-auto-fill-mode)
;; (add-hook 'html-mode-hook 'turn-off-flyspell-mode)

;; These should be disabled for new users, not me.
(defun ublt/enable (funcs)
  (dolist (f funcs)
          (put f 'disabled nil)))
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
;; (ublt/add-path "evil")
;; (defface ublt/evil-insert-tag
;;   `((t (:inherit font-lock-variable-name-face)))
;;   "Evil insert mode indicator face")
;; (defface ublt/evil-normal-tag
;;   `((t (:inherit font-lock-warning-face)))
;;   "Evil normal mode indicator face")
;; (defface ublt/evil-emacs-tag
;;   `((t (:inherit font-lock-builtin-face)))
;;   "Evil emacs mode indicator face")
;; (defface ublt/evil-visual-tag
;;   `((t (:inherit font-lock-preprocessor-face)))
;;   "Evil visual mode indicator face")
;; (setq evil-mode-line-format 'before
;;       evil-normal-state-tag (propertize "« ☢ »" 'face 'ublt/evil-normal-tag)
;;       evil-insert-state-tag (propertize "( | )" 'face 'ublt/evil-insert-tag)
;;       evil-emacs-state-tag  (propertize "( E )" 'face 'ublt/evil-emacs-tag)
;;       evil-visual-state-tag (propertize "( ∞ )" 'face 'ublt/evil-visual-tag)
;;       evil-normal-state-cursor '(box "#F86155")
;;       evil-insert-state-cursor '(bar "yellow")
;;       evil-emacs-state-cursor  '(bar "yellow")
;;       evil-visual-state-cursor '(bar "yellow")
;;       )

;; (require 'evil)
;; (dolist (mode '(sql-interactive-mode magit-log-edit-mode))
;;   (add-to-list 'evil-emacs-state-modes mode))
;; (evil-mode +1)

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
  (not (and (member major-mode '(comint-mode python-mode javascript-mode js-mode))
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

(require 'dired+)
;; `http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/'
;; linux;; multiple files
;; "nohup xdg-open" current-prefix-arg ;; linux can open multiple files, but one at a time
;; "see" current-prefix-arg ;; linux;; can open at most 1 file (being opened)
;; "open" current-prefix-arg ;; mac os x
(defun ublt/dired-open-native ()
  (interactive)
  (save-window-excursion
    (dired-do-async-shell-command
     (case system-type
       ('darwin "open")
       ;; XXX: Why doesn't 'gnome-open' work?
       ('gnu/linux "~/.emacs.d/config/open.sh"))
     current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))

;; Highlight current line
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode)

;; Hide details
(ublt/set-up 'dired-details+
  (setq dired-details-hide-link-targets nil)
  ;; Hide unimportant files
  (setq-default dired-omit-mode t
                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."))

;; Directories first by default. "s d" to change locally
(require 'dired-sort-map)
(setq dired-listing-switches "--group-directories-first -al")

;; Offer the other window's path as default when copying
(setq dired-dwim-target t)

;; Make find-name-dired ignore case
(setq find-name-arg "-iname")

;;; Source - `http://sites.google.com/site/steveyegge2/my-dot-emacs-file'
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

;;; ido --------------------------------------------------------------

;; (setq ido-decorations '( "(" ")" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(setq ido-decorations (quote ("\n=> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
      ido-use-virtual-buffers t
      ido-max-directory-size 100000)
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(ido-everywhere t)
;; ido on steroid `http://www.emacswiki.org/emacs/InteractivelyDoThing'
(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list) ido-cur-list)
                                        ;ido-cur-list ; Emacs 24
          )  ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read
                 prompt
                 allcomp
                 nil require-match initial-input hist def))
        ad-do-it))))

;;; Git --------------------------------------------------------------
;; TODO: Submit this to emacs-starter-kit. Older versions of magit
;; seem to ignore this option. Version 1 uses it as a list. I don't
;; understand why emacs-starter-kit set it as a string.
(ublt/add-path "magit")
(require 'magit)
(require 'magit-svn)
(setq magit-diff-options '("-w"))
;; magit status buffer should not be a pop-up (in the sense of not
;; volatile or temporary like helm buffer). This is important for
;; small screen such as mine.
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; ;;; Messages all the sync git invocations
;; (defadvice magit-git-insert (around show-sync-cmd activate)
;;   (let ((args (ad-get-arg 0)))
;;     (message (concat "git "
;;                      (mapconcat 'identity (append magit-git-standard-options args)
;;                                 " ")))
;;     ad-do-it))

;;; Growl async git invocations (in Ubuntu libnotify is used, which
;;; sucks)
(defadvice magit-run* (around show-async-cmd activate)
  (let ((cmd-args (ad-get-arg 0))
        (todochiku-timeout 1))
    (todochiku-message
     "Magit"
     (mapconcat 'identity cmd-args " ")
     (todochiku-icon 'social))
    ad-do-it))

;; git-emacs (some complementary features)
(ublt/add-path "git-emacs/")
(require 'git-emacs)

;;; Quicksilver/Spotlight for Emacs ----------------------------------
;;; TODO: Clean up

(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-regexp)
(require 'helm-buffers)
(require 'helm-files)
(setq helm-mp-highlight-delay 0.7
      helm-mp-highlight-threshold 4)
(setq ublt/helm-sources
      '(helm-c-source-ffap-line
        helm-c-source-ffap-guesser
        helm-c-source-buffers-list
      ;; helm-c-source-files-in-current-dir+
        helm-c-source-bookmarks
        helm-c-source-recentf
        helm-c-source-file-cache
        helm-c-source-locate)
             ;; Additions
      ;; helm-c-source-semantic
      ;; helm-c-source-git-project-files
      ;; helm-c-source-emacs-process
      )
(ublt/in '(gnu/linux)
  (setq helm-c-locate-command "locate -i -r '%s'"))

(defun ublt/helm ()
  (interactive)
  (helm-other-buffer ublt/helm-sources "*ublt/helm*"))

;; Find occurences of current symbol
;; TODO: turn on follow-mode by default for this
(require 'thingatpt)
(defun ublt/helm-occur-at-point ()
  (interactive)
  (let ((helm-follow-mode t))
    (helm :sources helm-c-source-occur
              :input (thing-at-point 'symbol))))

;;; TODO: Maybe customize faces is better (per-source selection of
;;; fixed-pitch/variable-pitched font)?
;;; XXX: `helm-M-x' does not define a source
(defun ublt/helm-should-use-variable-pitch? (sources)
  "Determine whether all of SOURCES should use variable-pitch
font (fixed-pitch is still preferable)."
  (every (lambda (x)
             (member x '(;; helm-c-source-ffap-line
                         ;; helm-c-source-ffap-guesser
                         ;; helm-c-source-buffers-list
                         helm-c-source-bookmarks
                         ;; helm-c-source-recentf
                         ;; helm-c-source-file-cache
                         ;; helm-c-source-filelist
                         ;; helm-c-source-files-in-current-dir+
                         ;; helm-c-source-files-in-all-dired
                         ;; helm-c-source-locate
                         helm-c-source-emacs-process
                         helm-c-source-org-headline
                         helm-c-source-emms-streams
                         helm-c-source-emms-files
                         helm-c-source-emms-dired
                         helm-c-source-google-suggest
                         helm-c-source-apt
                         ;; helm-c-source-helm-commands
                         )))
         sources))
(defun ublt/helm-tweak-appearance ()
  "Use variable-pitched font for helm if it's suitable for
all of the sources."
  (with-current-buffer helm-buffer
    (when (ublt/helm-should-use-variable-pitch? helm-sources)
      (variable-pitch-mode +1))
    (setq line-spacing 0.5)
    (text-scale-increase 2)))
(add-hook 'helm-after-initialize-hook 'ublt/helm-tweak-appearance)
;;; XXX: Big hack!
;;; TODO: Move to ublt-appearance?
(defadvice helm-initialize-overlays (after tweak-appearance activate)
  (condition-case nil
      (with-current-buffer helm-action-buffer
        (variable-pitch-mode +1)
        (setq line-spacing 0.5)
        (text-scale-increase 2))
    (error nil)))

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
(setq flymake-jslint-command "jslint")
(add-hook 'js-mode-hook 'moz-minor-mode)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

;; Factor
(ublt/in '(darwin)
  (load-file "/Applications/factor/misc/fuel/fu.el"))
(ublt/in '(gnu/linux)
  (load-file "~/Programming/factor/misc/fuel/fu.el"))

;; Haskell
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'auto-mode-alist '("\\.hs$" . haskell-mode))
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
  (require 'erlang-start)
  (defun ublt/erlang-compile-and-display ()
    (interactive)
    (call-interactively 'erlang-compile)
    (call-interactively 'erlang-compile-display))
  (add-hook 'erlang-mode-hook 'esk-prog-mode-hook)
  )

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

;;; SLIME, Common Lisp, Clojure --------------------------------------

(ublt/add-path "slime/")
(ublt/add-path "slime/contrib/")

;; SLIME customization
(eval-after-load "slime"
  '(progn
     ;; Extra features (contrib)
     (slime-setup
      '(slime-repl ;; slime-fuzzy
        ;; slime-highlight-edits
        slime-scratch
        slime-editing-commands
                   ))
     (setq slime-net-coding-system 'utf-8-unix
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           ;; common-lisp-hyperspec-root
           ;; "file:///Users/ubolonton/Programming/Tools/HyperSpec/"
           )
     ;; Use parentheses editting mode paredit
     (add-hook 'slime-mode-hook 'enable-paredit-mode t)
     (add-hook 'slime-repl-mode-hook 'enable-paredit-mode t)
     ;; Steel Bank CL
     (add-to-list 'slime-lisp-implementations
                  '(sbcl ("sbcl")))
     ;; (ad-activate 'slime-read-interactive-args)
     ))


;;;; clojure settings ------------------
;;;; UPDATED: use ELPA and lein swank now
;;;; Clojure is added to top of slime-lisp-implementations by ELPA?
;;;; 2010-01-27: Disabled ELPA and relied on manual tweaking
;; - About the :init part: I think SLIME was written for Common Lisp,
;; so many of its features do not work with Clojure (same thing with
;; paredit)
;; UPDATED: No need. `slime-read-interactive-args' takes care of this
;; (add-to-list 'slime-lisp-implementations
;;   `(clojure ,(swank-clojure-cmd) :init swank-clojure-init))
;; - Force clojure-mode buffers to use SLIME
;; UPDATED: No need
;; (add-hook 'clojure-mode-hook 'slime-lisp-mode-hook)
;; - Redirect printing to REPL
;; (add-hook 'clojure-mode-hook
;;   (lambda () (slime-redirect-inferior-output t)))

;; clojure-mode customization
(eval-after-load "clojure-mode"
  '(progn
     (add-hook 'clojure-mode-hook 'enable-paredit-mode t)
     (define-clojure-indent
       (describe 'defun)
       (testing 'defun)
       (given 'defun)
       (using 'defun)
       (with 'defun)
       (it 'defun)
       (do-it 'defun))
     (add-hook 'slime-repl-mode-hook
               'swank-clojure-slime-repl-modify-syntax t)))

;; swank-clojure customization
(eval-after-load "swank-clojure"
  '(progn
     ;; Add a hook to modify repl making it more Clojure-friendly
     ;; (I haven't seen this used anywhere somehow!?!)

     ;; Don't use swank-clojure-project
     ;; (add-hook 'swank-clojure-project-hook
     ;;           (lambda ()
     ;;             (setq default-directory path)
     ;;             (add-to-list
     ;;              'swank-clojure-extra-vm-args "")))
     ))
(ublt/add-path "swank-clojure-extra")
(require 'swank-clojure-extra)

;;; XXX: Make this customizable
(when (> (display-color-cells) 8)
  (font-lock-add-keywords 'clojurescript-mode
                          '(("(\\|)" . 'esk-paren-face))))

;;;; ielm settings ---------------
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; ac-slime
;; (ublt/add-path "ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

(require 'hippie-expand-slime)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

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
  '(progn
     (defun flymake-jshint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "jshint" (list "--jquery" local-file))))
     (setq flymake-allowed-file-name-masks
           (cons '(".+\\.js$"
                   flymake-jshint-init
                   flymake-simple-cleanup
                   flymake-get-real-file-name)
                 flymake-allowed-file-name-masks))
     (setq flymake-err-line-patterns
           (cons '("^Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\): \\(.+\\)$"
                   nil 1 2 3)
                 flymake-err-line-patterns))
     ))

(defun enable-flymake () (flymake-mode 1))
(dolist (hook '(emacs-lisp-mode-hook))
  (add-hook hook #'enable-flymake))

;;; Python -----------------------------------------------------------
;; The length of this section proves python support in Emacs is weak,
;; since all these are just for basic stuffs. Also Pymacs
;; initialization is very slow.
;; Try to install stuffs from official pages instead of from
;; apt (use easy_install)

;; pymacs, ropemode, ropemacs

(ublt/in '(gnu/linux darwin)
  (ublt/add-path "python")
  (setq-default ;; py-shell-name          "ipython"
   ;; py-python-command      py-shell-name
   ;; py-jpython-command     py-shell-name
   ;; py-jython-command      py-shell-name
   ;; py-default-interpreter py-shell-name
   ;; python-command         py-shell-name
   py-shell-switch-buffers-on-execute nil)
  (require 'python-mode)
  (require 'ipython)
  (setq-default py-python-command-args (list "-colors" "Linux"))
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

(defun turn-off-hl-line-mode ()
  (hl-line-mode nil))
(add-to-list 'auto-mode-alist '("\\.mako?$" . mako-nxhtml-mumamo-mode))
(add-hook 'mako-nxhtml-mumamo-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'nxhtml-mode-hook 'turn-off-auto-fill-mode)
;; (add-hook 'nxhtml-mode-hook 'turn-off-flyspell-mode)

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
  (require 'todochiku))

;; TODO: move to corresponding mode sections
;; .rjs file is ruby file
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))

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
(load custom-file)

;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (server-start)
  (error (message "Could not start server")))
