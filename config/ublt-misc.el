(require 'ublt-util)

;;; Misc customization
;;; TODO: add case toggling

;;; Might grow into a project on its own, adding more project
;;; management stuffs
(ublt/set-up 'eproject
  (ublt/add-path "eproject")
  (setq eproject-completing-read-function 'eproject--ido-completing-read
        eproject-todo-expressions '("TODO" "XXX" "FIX" "FIXME" "HACK" "NTA"))
  (ublt/set-up 'eproject-ido-imenu))

(ublt/set-up 'projectile
  (projectile-global-mode +1))

;;; TODO: Better dictionary (one with tech terms)?
(ublt/set-up 'flyspell
  (setq flyspell-default-dictionary "english"))
(ublt/set-up 'ispell
  (setq ispell-dictionary "english"))

(setq
;;; This stops the damned auto-pinging
 ffap-machine-p-known 'reject

 tramp-default-method "ssh")

;;; Old buffer clean up
(ublt/set-up 'midnight
  (setq clean-buffer-list-delay-general 7             ; days
        clean-buffer-list-delay-special (* 3 24 3600) ; 3 days
        )
  (add-to-list 'desktop-locals-to-save 'buffer-display-time))


(ublt/in '(gnu/linux)
  (setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

(ublt/add-path "emacs-skype")
;;; XXX: Disable for now, since Skype is f**king unstable
;; (require 'skype)
;; (skype--init)
(setq skype--my-user-handle "ubolonton")

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
(ublt/set-up 'saveplace
  (setq save-place-file "~/.emacs.d/.saveplace"
        save-place-limit 3000))

;; Save history
(setq savehist-additional-variables
      '(search-ring regexp-search-ring)
      savehist-file "~/.emacs.d/.savehist")
(dolist (var '(log-edit-comment-ring
               regexp-search-ring
               search-ring
               Info-history-list
               Info-search-history))
  (add-to-list 'savehist-additional-variables var))
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

;;; TODO: Enable this when there is a workaround for highlighted
;;; symbols always being displayed in fixed-width font
(remove-hook 'prog-mode-hook 'idle-highlight-mode)


;; Who the hell set it to t?
(setq debug-on-error nil)


;;; Misc stuff I use -------------------------------------------------

;; Kill processes
(ublt/in '(darwin gnu/linux)
  (require 'vkill)
  ;; Notifications
  (ublt/set-up 'todochiku))

;; TODO: move to corresponding mode sections
;; .rjs file is ruby file
(add-to-list 'auto-mode-alist '("\\.rjs$" . ruby-mode))
(setq-default ruby-indent-level 4)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . gfm-mode))

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

(provide 'ublt-misc)
