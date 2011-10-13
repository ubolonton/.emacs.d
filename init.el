;; ;;; init.el --- Where all the magic begins
;; ;;
;; ;; Part of the Emacs Starter Kit
;; ;;
;; ;; This is the first thing to get loaded.
;; ;;
;; ;; "Emacs outshines all other editing software in approximately the
;; ;; same way that the noonday sun does the stars. It is not just bigger
;; ;; and brighter; it simply makes everything else vanish."
;; ;; -Neal Stephenson, "In the Beginning was the Command Line"

;; ;; Turn off mouse interface early in startup to avoid momentary display
;; ;; You really don't need these; trust me.
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; ;; Load path etc.

;; (setq dotfiles-dir (file-name-directory
;;                     (or (buffer-file-name) load-file-name)))

;; ;; Load up ELPA, the package manager

;; (add-to-list 'load-path dotfiles-dir)

;; (add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))

;; (setq autoload-file (concat dotfiles-dir "loaddefs.el"))
;; (setq package-user-dir (concat dotfiles-dir "elpa"))
;; (setq custom-file (concat dotfiles-dir "custom.el"))

(require 'package)
(dolist (source '(("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
;; (require 'starter-kit-elpa)

;; ;; These should be loaded on startup rather than autoloaded on demand
;; ;; since they are likely to be used in every session

;; (require 'cl)
;; (require 'saveplace)
;; (require 'ffap)
;; (require 'uniquify)
;; (require 'ansi-color)
;; (require 'recentf)

;; ;; backport some functionality to Emacs 22 if needed
;; (require 'dominating-file)

;; ;; Load up starter kit customizations

;; (require 'starter-kit-defuns)
;; (require 'starter-kit-bindings)
;; (require 'starter-kit-misc)
;; (require 'starter-kit-registers)
;; (require 'starter-kit-eshell)
;; (require 'starter-kit-lisp)
;; (require 'starter-kit-perl)
;; (require 'starter-kit-ruby)
;; (require 'starter-kit-js)

;; (regen-autoloads)
;; (load custom-file 'noerror)

;; You can keep system- or user-specific customizations here
;; (setq system-specific-config (concat dotfiles-dir system-name ".el")
;;       user-specific-config (concat dotfiles-dir user-login-name ".el")
;;       user-specific-dir (concat dotfiles-dir user-login-name))
;; (add-to-list 'load-path user-specific-dir)

;; (if (file-exists-p system-specific-config) (load system-specific-config))
;; (if (file-exists-p user-specific-dir)
;;   (mapc #'load (directory-files user-specific-dir nil ".*\\.el$")))
;; (if (file-exists-p user-specific-config) (load user-specific-config))

;;; init.el ends here














;;; Use forward-page & backward-page to navigate through sections

;;; Consider using following packages:
;;; highlight-completion

;;; Some convenient functions

;; To help separating OS-specific stuffs
(defmacro ublt/in (systems &rest body)
  "Run BODY if `system-type' is in the list of SYSTEMS."
  (declare (indent 1))
  `(when (member system-type ,systems)
     ,@body))

(defun ublt/add-path (path)
  (add-to-list 'load-path (concat "~/.emacs.d/lib/" path)))

(defun ublt/status-message (&rest args)
  "Show a message in the minibuffer without logging. Useful for
transient messages like error messages when hovering over syntax
errors."
  (let ((message-log-max nil))
    (apply #'message args)))

(defvar ublt/set-up-features ())
(defvar ublt/failed-features ())
(defmacro ublt/set-up (feature &rest body)
  "Try to load the feature, running BODY afterward, notifying
user if not found."
  (declare (indent 1))
  `(if (not (require ,feature nil t))
       (progn (message "ublt/customize: `%s' not found" ,feature)
              (add-to-list 'ublt/failed-features ,feature t))
     (add-to-list 'ublt/set-up-features ,feature t)
     ,@body))


;;; Path to stuffs that come from single files
(ublt/add-path "single-file-modes")

;;; General usability
(ublt/add-path "ubolonton")
(require 'ublt-dvorak)
(require 'ublt-appearance)
(require 'ublt-navigation)
(require 'ublt-editing)

;;; Personal stuffs
(ublt/in '(gnu/linux)
  (load-file "/usr/local/share/emacs/24.0.50/lisp/gnus/mailcap.elc")) ; FLIM breaks this
(ublt/add-path "twittering-mode/")
(ublt/add-path "emms/lisp/")
(ublt/add-path "org2blog/")
(require 'ublt-communication)
(require 'ublt-entertainment)
(require 'ublt-organization)

;;; Might grow into a project on its own, adding more project
;;; management stuffs
(ublt/add-path "eproject")
(setq eproject-completing-read-function 'eproject--ido-completing-read
      eproject-todo-expressions '("TODO" "XXX" "FIX" "FIXME" "HACK"))
(require 'eproject-ido-imenu)

;;; Misc customization
;;; TODO: add case toggling

(ublt/in '(gnu/linux)
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(setq stack-trace-on-error t)

;; nxhtml seems to byte-compile a lot while running, and uses some
;; obsolete stuffs. This is to prevent warning pop-ups (especially in
;; mako files)
(setq byte-compile-warnings '(not obsolete))

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
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)

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

;; Additional packages are installed in these directories
(ublt/in '(darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/usr/local/bin" exec-path)
  (push "/opt/local/bin" exec-path))

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

;;; TODO: Set this up
(ublt/add-path "find-file-in-project")
(require 'find-file-in-project)

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
       ('gnu/linux "~/.emacs.d/ubolonton/open.sh"))
     current-prefix-arg
     (dired-get-marked-files t current-prefix-arg))))

;; Highlight current line
(add-hook 'dired-mode-hook 'esk-turn-on-hl-line-mode)

;; Hide details
(ublt/set-up 'dired-details+
  (setq dired-details-hide-link-targets nil)
  ;; Hide unimportant files
  (setq-default dired-omit-mode t))

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
      ido-use-virtual-buffers t)
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

;;; Magit ------------------------------------------------------------
;; TODO: Submit this to emacs-starter-kit. Older versions of magit
;; seem to ignore this option. Version 1 uses it as a list. I don't
;; understand why emacs-starter-kit set it as a string.
(ublt/add-path "magit")
(require 'magit)
(setq magit-diff-options '("-w"))
;; magit status buffer is not a pop-up (in the sense of not volatile
;; or temporary like anything buffer). This is important for small
;; screen such as mine.
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

;;; Quicksilver/Spotlight for Emacs ----------------------------------
;;; TODO: Clean up

(ublt/add-path "anything/")
(ublt/add-path "anything/anything-config/")
(ublt/add-path "anything/anything-config/extensions")
;; (require 'anything)
;; (require 'anything-etags)
;; I don't use `anything-startup' since some of the packages there are
;; not suitable. For example, `anything-match-plugin' is slow.
(require 'anything-config)
(require 'anything-match-plugin)
(setq anything-mp-highlight-delay 0.5
      anything-mp-highlight-threshold 4)

;; http://www.emacswiki.org/emacs/AnythingSources#toc62
(require 'magit)
(defvar anything-c-source-git-project-files
  '((name . "Files from Current GIT Project")
    (init . (lambda ()
              (setq anything-git-top-dir
                    (magit-get-top-dir
                     (if (buffer-file-name)
                         (file-name-directory (buffer-file-name))
                       default-directory)))))
    (candidates
     . (lambda ()
         (if anything-git-top-dir
             (let ((default-directory anything-git-top-dir))
               (mapcar (lambda (file) (concat default-directory file))
                       (magit-shell-lines
                        (magit-format-git-command
                         "ls-files" nil)))))))
    (type . file)))

(eval-after-load "anything"
  '(progn
     (setq anything-sources
           '( ;; Adapt from `anything-for-files-prefered-list'
             anything-c-source-ffap-line
             anything-c-source-ffap-guesser
             anything-c-source-buffers-list
             anything-c-source-files-in-current-dir+
             anything-c-source-bookmarks
             anything-c-source-recentf
             anything-c-source-file-cache
             anything-c-source-locate
             ;; Additions
             ;; anything-c-source-semantic
             ;; anything-c-source-git-project-files
             anything-c-source-emacs-process
             ))))

;; Find occurences of current symbol
;; TODO: turn on follow-mode by default for this
(require 'thingatpt)
(defun ublt/anything-occur-at-point ()
  (interactive)
  (let ((anything-follow-mode t))
    (anything :sources anything-c-source-occur
              :input (thing-at-point 'symbol))))

;;; TODO: Maybe customize faces is better?
;;; XXX: `anything-M-x' does not define a source
(defun ublt/anything-should-use-variable-pitch? (sources)
  "Determine whether all of SOURCES should use variable-pitch
font (fixed-pitch is still preferable)."
  (reduce (lambda (a b) (and a b))
          (mapcar
           (lambda (x)
             (member x '(;; anything-c-source-ffap-line
                         ;; anything-c-source-ffap-guesser
                         ;; anything-c-source-buffers-list
                         anything-c-source-bookmarks
                         ;; anything-c-source-recentf
                         ;; anything-c-source-file-cache
                         ;; anything-c-source-filelist
                         ;; anything-c-source-files-in-current-dir+
                         ;; anything-c-source-files-in-all-dired
                         ;; anything-c-source-locate
                         anything-c-source-emacs-process
                         anything-c-source-org-headline
                         anything-c-source-emms-streams
                         anything-c-source-emms-files
                         anything-c-source-emms-dired
                         anything-c-source-google-suggest
                         anything-c-source-apt
                         ;; anything-c-source-anything-commands
                         )))
           sources)))
(defun ublt/anything-setup-variable-pitch-font ()
  "Use variable-pitched font for anything if it's suitable for
all of the sources."
  (with-current-buffer anything-buffer
    (when (ublt/anything-should-use-variable-pitch? anything-sources)
      (variable-pitch-mode +1))))
(add-hook 'anything-after-initialize-hook 'ublt/anything-setup-variable-pitch-font)
;;; XXX: Big hack!
;;; TODO: Move to ublt-appearance?
(defadvice anything-initialize-overlays (after use-variable-pitch-font activate)
  (condition-case nil
      (with-current-buffer anything-action-buffer
        (variable-pitch-mode +1))
    (error nil)))

;; auto-complete
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode +1)

;;; Yasnippet --------------------------------------------------------

(ublt/set-up 'yasnippet
  (setq yas/root-directory  "~/.emacs.d/data/snippets"
        yas/prompt-functions '(yas/dropdown-prompt yas/ido-prompt yas/no-prompt)
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
(load "autostart.el")

;; Javascript (it seems js-mode in Emacs is newer than espresso)
;; MozRepl integration
;; (defalias 'javascript-mode 'espresso-mode)
(setq js-mode-hook '())
(add-hook 'js-mode-hook 'moz-minor-mode)
(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; Factor
(ublt/in '(darwin)
  (load-file "/Applications/factor/misc/fuel/fu.el"))
(ublt/in '(gnu/linux)
  (load-file "~/Programming/factor/misc/fuel/fu.el"))

;; Haskell
(ublt/add-path "haskell-mode-2.8.0")
(load "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-to-list 'auto-mode-alist '("\\.mak$" . html-mode))
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

;;; SLIME, Common Lisp, Clojure --------------------------------------

;; technomancy's SLIME
(ublt/add-path "slime/")
(ublt/add-path "slime/contrib/")

;; SLIME customization
(eval-after-load "slime"
  '(progn
     ;; Extra features (contrib)
     (slime-setup
      '(slime-repl slime-fuzzy slime-highlight-edits))
     (setq slime-net-coding-system 'utf-8-unix
           slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           common-lisp-hyperspec-root
           "file:///Users/ubolonton/Programming/Tools/HyperSpec/")
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
       (do-it 'defun))))

;; swank-clojure customization
(eval-after-load "swank-clojure"
  '(progn
     ;; Add a hook to modify repl making it more Clojure-friendly
     ;; (I haven't seen this used anywhere somehow!?!)
     (add-hook 'slime-repl-mode-hook
               'swank-clojure-slime-repl-modify-syntax t)
     ;; Don't use swank-clojure-project
     ;; (add-hook 'swank-clojure-project-hook
     ;;           (lambda ()
     ;;             (setq default-directory path)
     ;;             (add-to-list
     ;;              'swank-clojure-extra-vm-args "")))
     ))
(ublt/add-path "swank-clojure-extra")
(require 'swank-clojure-extra)

;;; ClojureScript
(defvar ublt/clojurescript-home "/home/ubolonton/Programming/Tools/clojurescript/")
;; (dolist (path '("src/clj" "src/cljs" "test/cljs"))
;;   (add-to-list 'swank-clojure-classpath (concat ublt/clojurescript-home path)))

;;;; ielm settings ---------------
(add-hook 'ielm-mode-hook 'enable-paredit-mode)

;; ac-slime
(ublt/add-path "ac-slime")
(require 'ac-slime)
(add-hook 'slime-mode-hook '(lambda () (set-up-slime-ac t)))

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
;; (defun enable-flymake () (flymake-mode 1))
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

(defadvice flymake-goto-next-error (after display-message)
  (ublt/flymake-err-echo))
(defadvice flymake-goto-prev-error (after display-message)
  (ublt/flymake-err-echo))
(ad-activate 'flymake-goto-next-error t)
(ad-activate 'flymake-goto-prev-error t)

;;; Python -----------------------------------------------------------
;; The length of this section proves python support in Emacs is weak,
;; since all these are just for basic stuffs. Also Pymacs
;; initialization is very slow.
;; Try to install stuffs from official pages instead of from
;; apt (use easy_install)

(ublt/in '(darwin gnu/linux)
  ;; `http://hide1713.wordpress.com/2009/01/30/setup-perfect-python-environment-in-emacs/'
  (ublt/add-path "python")
  (autoload 'python-mode "python-mode" "Python editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  (add-to-list 'interpreter-mode-alist'("python" . python-mode))
  ;; FIXME: How can we override the built-in mode? I have to load it
  ;; up by hand currently!?!
  ;; (require 'python-mode)
  (load "~/.emacs.d/lib/python/python-mode")
  ;; pymacs
  (require 'pymacs)
  ;; (autoload 'pymacs-apply "pymacs")
  ;; (autoload 'pymacs-call "pymacs")
  ;; (autoload 'pymacs-eval "pymacs" nil t)
  ;; (autoload 'pymacs-exec "pymacs" nil t)
  ;; (autoload 'pymacs-load "pymacs" nil t)
  ;; (eval-after-load "pymacs"
  ;;  '(add-to-list 'pymacs-load-path "/usr/local/lib/python2.6/dist-packages/"))

  ;; FIXME: this does not work during initialization, but works if
  ;; eval'ed afterward, or if we increase timeout. Something's wrong
  ;; here though since it signals error almost immediately upon
  ;; calling, not after 30 seconds.
  (let ((pymacs-timeout-at-start 300))
    (pymacs-load "ropemacs" "rope-"))
  ;; (add-hook 'after-init-hook (lambda ()
  ;;                              (pymacs-load "ropemacs" "rope-"))
  ;;           t)
  (setq ropemacs-enable-autoimport t
        ropemacs-guess-project t)
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
  ;; Auto Syntax Error Hightlight
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
               ;; flymake and mumamo are at odds
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
    (setq imenu-create-index-function #'py-imenu-create-index-function))
  (defun ublt/turn-on-ropemacs-mode ()
    (when (and buffer-file-name
               (string-match "\\.py$" buffer-file-name))
      (ropemacs-mode +1)))
  (defun ublt/set-python-tab ()
    (setq tab-width 4))
  (eval-after-load 'python-mode
    '(progn
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
       (setq py-python-command-args (list "-colors" "Linux")
             py-shell-switch-buffers-on-execute nil)
       (add-hook 'comint-preoutput-filter-functions
                 'ublt/comint-preoutput-clear-^A^B)
       (require 'ipython)
       ;; To use this, import things into ipython
       (require 'anything-ipython)
       ;; Additionally show compeletion in-place
       (when (require 'anything-show-completion nil t)
         (use-anything-show-completion 'anything-ipython-complete
                                       '(length initial-pattern)))
       ;; ============================================================
       ;; `http://taesoo.org/Opensource/Pylookup'
       ;; add pylookup to your loadpath, ex) "~/.lisp/addons/pylookup"
       (setq pylookup-dir "~/.emacs.d/lib/pylookup")
       (add-to-list 'load-path pylookup-dir)
       ;; load pylookup when compile time
       (eval-when-compile (require 'pylookup))

       ;; set executable file and db file
       (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
       (setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))

       ;; to speedup, just load it on demand
       (autoload 'pylookup-lookup "pylookup"
         "Lookup SEARCH-TERM in the Python HTML indexes." t)
       (autoload 'pylookup-update "pylookup"
         "Run pylookup-update and create the database at `pylookup-db-file'." t)
       ))
  (defun ublt/paredit-space-for-open? (endp delimiter)
    (not (and (member major-mode '(python-mode javascript-mode js-mode))
              (member delimiter '(?\( ?\[ ?\")))))
  (eval-after-load "paredit"
    '(add-to-list 'paredit-space-for-delimiter-predicates
                  'ublt/paredit-space-for-open?))
  )

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

;; TODO: integrate with anything
;; Kill processes
(ublt/in '(darwin gnu/linux)
  (require 'vkill)
  ;; Notifications
  (require 'todochiku))

;; TODO: move to corresponding mode sections
;; .rjs file is ruby file
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))

;; FIXME: Make it support mp3 not only ogg
(require 'lyric-mode)

;; git-emacs
(ublt/add-path "git-emacs/")
(require 'git-emacs)

;; Devilspie's config
(add-to-list 'auto-mode-alist '("\\.ds$" . lisp-mode))

;; Conkeror as default browser
(ublt/in '(gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror"))

(setq sql-connection-alist
      '(("bnb" (sql-product 'mysql) (sql-database "bnb")
         (sql-user "bnb") (sql-server "localhost"))
        ("academy" (sql-product 'postgres) (sql-database "academy")
         (sql-user "academy") (sql-server "localhost"))
        ("postgres" (sql-product 'postgres)
         (sql-user "postgres") (sql-server "localhost"))))

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

;; Interops (with Terminal, Conkeror...) -----------------------------
(condition-case err
    (server-start)
  (error (message "Could not start server")))
