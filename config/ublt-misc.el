;;; -*- lexical-binding: t; coding: utf-8 -*-
(require 'ublt-util)

;;; Misc customization
;;; TODO: add case toggling

;;; Project management
(use-package projectile
  :defer 10
  :config (projectile-global-mode +1))

;;; TODO: Better dictionary (one with tech terms)?
(use-package flyspell
  :custom (flyspell-default-dictionary "english"))
(use-package ispell
  :custom (ispell-dictionary "english"))

(setq
;;; This stops the damned auto-pinging
 ffap-machine-p-known 'reject

 tramp-default-method "ssh")

;;; Old buffer clean up
(use-package midnight
  :custom ((clean-buffer-list-delay-general 7)
           (clean-buffer-list-delay-special (* 3 24 3600))
           (midnight-delay (* 12 3600) "Midday instead of midnight"))
  :config (midnight-mode +1))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-locals-to-save 'buffer-display-time))

(ublt/in '(gnu/linux)
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

;; `http://www.emacswiki.org/emacs/DeskTop#toc6'
;; (desktop-save-mode +1)
(define-advice desktop-create-buffer (:around (f file-version buffer-filename &rest args) ublt/ignore-errors)
  (condition-case err
      (apply f file-version buffer-filename args)
    (error (message "desktop-create-buffer: %s %s" err buffer-filename))))
(setq desktop-restore-frames nil
      desktop-restore-eager 30)
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

;; Command statistics
;; FIXME: Prune #'s from table to make it work
;; (require 'command-frequency)
;; (setq command-frequency-table-file "~/.emacs.d/cmd_frequencies")
;; (command-frequency-table-load)
;; (command-frequency-mode 1)
;; (command-frequency-autosave-mode 1)
;; (use-package keyfreq
;;   (keyfreq-mode 1)
;;   (keyfreq-autosave-mode 1))

;; These should be disabled for new users, not me.
(ublt/enable '(narrow-to-region set-goal-column upcase-region downcase-region))

;; Save positions in visited files
(use-package saveplace
  :custom ((save-place-file (ublt/init-rel-path ".saveplace"))
           (save-place-limit 3000))
  :config (save-place-mode +1))

;; Save history
(use-package savehist
  :custom (savehist-file (ublt/init-rel-path ".savehist"))
  :config
  (dolist (var '(log-edit-comment-ring
                 regexp-search-ring
                 search-ring
                 Info-history-list
                 Info-search-history))
    (add-to-list 'savehist-additional-variables var))
  (savehist-mode +1))

(use-package bookmark
  :custom ((bookmark-default-file (ublt/init-rel-path ".bookmarks"))
           (bookmark-version-control t)
           (bookmark-sort-flag nil)))

(use-package recentf
  :custom (recentf-save-file (ublt/init-rel-path ".recentf")))

(setq ring-bell-function 'ignore)

;; TextMate minor mode
(use-package textmate)
;; (textmate-mode)

;; pabbrev
;; (require 'pabbrev)
;; (put 'python-mode 'pabbrev-global-mode-excluded-modes t)
;; (global-pabbrev-mode +1)

;; XXX: This makes python setup unhappy. (Both use C-x p)
;; (require 'bookmark+)

;; (require 'key-chord)
;; (key-chord-mode +1)
;; (key-chord-define-global "dd" 'kill-whole-line)

;; Who the hell set it to t?
(setq debug-on-error nil)


;;; Misc stuff I use -------------------------------------------------

(ublt/in '(darwin)
  (use-package woman
    :config
    (add-to-list 'woman-manpath '("/opt/local/bin" . "/opt/local/man"))
    (dolist (path '("/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/share/man"
                    "/Applications/Xcode.app/Contents/Developer/usr/share/man"
                    "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/share/man"))
      (add-to-list 'woman-manpath path t))))

;; Devilspie's config
(add-to-list 'auto-mode-alist '("\\.ds$" . lisp-mode))

;; Conkeror as default browser
(ublt/in '(gnu/linux)
  (setq browse-url-browser-function 'browse-url-generic
        browse-url-generic-program "conkeror"))

(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (blink-cursor-mode -1))

(defun ublt/turn-off-tool-bar ()
  (if (functionp 'tool-bar-mode) (tool-bar-mode -1)))
(add-hook 'before-make-frame-hook 'ublt/turn-off-tool-bar)

(ublt/turn-off-tool-bar)
(when (functionp 'menu-bar-mode) (menu-bar-mode -1))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq inhibit-startup-screen t
      sentence-end-double-space nil
      shift-select-mode nil
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      create-lockfiles nil
      diff-switches "-u")

(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines nil)
(set-default 'imenu-auto-rescan t)

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(show-paren-mode +1)

;;; Seed the random-number generator.
(random t)

(with-eval-after-load 'hippie-exp
  (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
     (delete f hippie-expand-try-functions-list))

  ;; Add this back in at the end of the list.
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t))

;;; Open files with certain extensions using an external program
;;; (opening a large PDF file can hang Emacs).
(defvar ublt/find-file-externally-extensions
  '("pdf" "xls" "xlsx" "doc" "docx" "odt" "jpg" "png" "dmg" "pkg"))
(define-advice find-file (:around (f filename &rest args) ublt/open-externally-maybe)
  (if (member (downcase (or (file-name-extension filename) ""))
              ublt/find-file-externally-extensions)
      (call-process (pcase system-type
                      ('darwin "open")
                      ('gnu/linux "xdg-open"))
                    nil 0 nil filename)
    (apply f filename args)))

;; https://stackoverflow.com/questions/77328174/how-can-emacs-preview-java-jmod-file-like-jar-file-from-dired
(define-advice archive-find-type (:around (f &rest args) ublt/detect-jmod)
  (let (case-fold-search)
    (cond ((save-excursion
             (save-restriction
               (widen))
             (goto-char (point-min))
             (looking-at "JM\001\000")) 'zip)
          (t (apply f args)))))
(add-to-list 'auto-mode-alist '("\\.jmod\\'" . archive-mode))
(add-to-list 'auto-coding-alist '("\\.jmod\\'" . no-conversion))


;;; `http://www.masteringemacs.org/articles/2011/07/20/searching-buffers-occur-mode/'

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

(provide 'ublt-misc)
