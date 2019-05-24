;;; -*- lexical-binding: t; coding: utf-8 -*-

(require 'ublt-util)

;;; TODO: Clean up

(use-package helm)

;;; FIX: Check why some helm sources flicker when input changes.

(use-package helm-config
  ;; TODO: Find a way to remove this `:straight' `helm' piece (either by a macro, by calling
  ;; `use-package' on`helm' instead of the individual sub-packages, or by sth else).
  :ensure nil :straight nil
  :custom ((helm-quick-update t)
           ;; helm-maybe-use-default-as-input nil
           (helm-split-window-in-side-p 'below)
           (helm-split-window-default-side 'below)
           (helm-always-two-windows t)
           (helm-move-to-line-cycle-in-source t)
           ;; Better-looking separator for multi-line sources
           (helm-candidate-separator "────────────────────────────────────────────────────────────────────────────────")
           ;; So C-w put the current symbol in helm's prompt
           (helm-yank-symbol-first t))
  :config (helm-mode +1))

(defun ublt/helm-enable-fuzzy (sources-and-classes)
  (dolist (setting sources-and-classes)
    (pcase-let ((`(,s ,class) setting))
      (let ((source (symbol-value s)))
        (set s (helm-make-source (helm-attr 'name source) class
                 :fuzzy-match t))))))

(use-package helm-for-files
  :ensure nil :straight nil
  :custom ((helm-ff-file-name-history-use-recentf t)
           ;; helm-ff-auto-update-initial-value t
           (helm-ff-transformer-show-only-basename nil)
           (helm-file-cache-fuzzy-match t)
           (helm-recentf-fuzzy-match t)
           (helm-ff-search-library-in-sexp t))
  :config (dolist (pattern '("\\.pyc$" "\\.elc$"))
            (add-to-list 'helm-boring-file-regexp-list pattern)))

(use-package helm-buffers
  :ensure nil :straight nil
  :custom (helm-buffers-fuzzy-matching t)
  :config (unless helm-source-buffers-list
            (setq helm-source-buffers-list
                  (helm-make-source "Buffers" 'helm-source-buffers))))

(use-package helm-locate
  :ensure nil :straight nil
  :custom (helm-locate-fuzzy-match nil)
  :config (progn (ublt/in '(gnu/linux)
                   (setq helm-locate-command "locate %s -e -A --regex %s"))
                 (ublt/in '(darwin)
                   (setq helm-locate-command "mdfind %s %s"))))

(use-package helm-bookmark
  :ensure nil :straight nil
  :config (ublt/helm-enable-fuzzy
           '((helm-source-bookmarks helm-source-basic-bookmarks))))

(use-package helm-imenu
  :ensure nil :straight nil
  :custom ((helm-imenu-delimiter " ")
           (helm-imenu-fuzzy-match t)
           ;; This is a misfeature when combined with "use default as
           ;; input", so disable it
           (helm-imenu-execute-action-at-once-if-one nil)))

(use-package helm-net
  :ensure nil :straight nil
  :custom ((helm-google-suggest-use-curl-p (when (executable-find "curl") t))
           (helm-home-url "https://www.google.com")))

(use-package helm-command
  :ensure nil :straight nil
  :custom (helm-M-x-fuzzy-match t))

(use-package helm-org
  :ensure nil :straight nil
  :custom ((helm-org-headings-fontify t)
           (helm-org-format-outline-path t)))

(use-package helm-regexp
  :ensure nil :straight nil)

(use-package helm-elisp
  :ensure nil :straight nil
  :custom (helm-apropos-fuzzy-match t))

(use-package helm-man
  :ensure nil :straight nil
  :config (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages))

(defun ublt/helm-sources ()
  (let ((base '(helm-source-buffers-list
                helm-source-recentf
                helm-source-ido-virtual-buffers
                helm-source-buffer-not-found
                helm-source-files-in-current-dir
                helm-source-bookmarks
                helm-source-file-cache)))
    base))

(defun ublt/helm ()
  (interactive)
  (helm-other-buffer (ublt/helm-sources) "*ublt/helm*"))

;;; TODO: Maybe customize faces is better (per-source selection of
;;; fixed-pitch/variable-pitched font)?
;;; XXX: `helm-M-x' does not define a source
(defun ublt/helm-should-use-variable-pitch? (sources)
  "Determine whether all of SOURCES should use variable-pitch
font (fixed-pitch is still preferable)."
  (-every? (lambda (x)
             (member x '(;; helm-c-source-ffap-line
                         ;; helm-c-source-ffap-guesser
                         ;; helm-c-source-buffers-list
                         helm-source-bookmarks
                         ;; helm-source-recentf
                         ;; helm-source-file-cache
                         ;; helm-source-filelist
                         ;; helm-source-files-in-current-dir+
                         ;; helm-source-files-in-all-dired
                         ;; helm-source-locate
                         helm-source-emacs-process
                         helm-source-org-headline
                         helm-source-google-suggest
                         helm-source-apt
                         ;; helm-source-helm-commands
                         )))
           sources))

(defun ublt/helm-tweak-appearance ()
  "Use `variable-pitch' font for helm if it's suitable for
all of the sources."
  (with-current-buffer helm-buffer
    (when (ublt/helm-should-use-variable-pitch? helm-sources)
      (variable-pitch-mode +1))
    (setq line-spacing 0.3)
    (visual-line-mode -1)
    ;; (text-scale-increase 1)
    ))

(use-package helm
  :hook (helm-after-initialize . ublt/helm-tweak-appearance))

(defvar ublt/helm-exit-other-window-p nil)

(defun ublt/helm-maybe-exit-minibuffer-other-window ()
  (interactive)
  ;; We cannot use `let' because the action is executed after exiting
  ;; the minibuffer, not during
  (setq ublt/helm-exit-other-window-p t)
  (call-interactively 'helm-maybe-exit-minibuffer))

;;; For some reason combining them into a single around advice didn't work
(defadvice helm-execute-selection-action-1
    (before maybe-other-window activate)
  (when ublt/helm-exit-other-window-p
    (when (= (count-windows) 1)
      (split-window-horizontally))
    (other-window 1)))

(defadvice helm-execute-selection-action-1
    (around maybe-other-window-cleanup activate)
  (unwind-protect ad-do-it
    (setq ublt/helm-exit-other-window-p nil)))

;;; TODO: Refine this.
;;; TODO: Sometimes inline display is better.
;;; https://www.reddit.com/r/emacs/comments/7rho4f/now_you_can_use_helm_with_frames_instead_of/
(use-package helm
  :custom ((helm-display-buffer-reuse-frame t)
           (helm-use-undecorated-frame-option t))
  :config

  ;; XXX: There must by some other way.
  (define-advice helm-hide-minibuffer-maybe (:before (&rest _) ublt/fix-input-location)
    "Make sure helm input is always displayed at the top."
    (with-helm-buffer
      (setq-local helm-echo-input-in-header-line t)))

  (when window-system
    (setq helm-display-function #'helm-display-buffer-in-own-frame)

    (add-to-list 'helm-commands-using-frame #'helm-swoop)

    (defun ublt/make-transparent-maybe (&rest _)
      (set-frame-parameter helm-popup-frame 'alpha
                           ;; XXX: The frame is shown during blocking operations. "Hide" it by
                           ;; setting inactive alpha to 0.
                           (if (helm-follow-mode-p) (cons 35 0) (cons 100 0))))

    ;; Make helm popup frame transparent when `helm-follow-mode' is on.
    (add-hook 'helm-minibuffer-set-up-hook #'ublt/make-transparent-maybe)
    (advice-add 'helm-follow-mode :after #'ublt/make-transparent-maybe)

    (define-advice helm-display-buffer-popup-frame (:around (f buffer frame-alist) ublt/tweak-appearance)
      (let ((current-frame (selected-frame)))
        (funcall f buffer
                 (-reduce-from (lambda (alist pair) (cons pair alist))
                               frame-alist
                               '((fullscreen . nil)
                                 (left-fringe . 8)
                                 (right-fringe . 8)
                                 (border-width . 0)
                                 (unsplittable . t)
                                 (undecorated . t)
                                 (cursor-type . bar))))

        (when helm-popup-frame
          (let* ((display-w (x-display-pixel-width))
                 ;; External monitor (Dell).
                 (dell-w 2560)
                 (dell-h 1440)
                 ;; MBP, 15-inch, 2017, default.
                 (laptop-w 1680)
                 (laptop-h 1050)
                 (setup (cond
                         ((> display-w dell-w) 'both)
                         ((= display-w dell-w) 'dell)
                         (t 'laptop)))
                 ;; ;; TODO: Use current frame's size and position.
                 (w (round (* 0.34 (if (eql setup 'laptop)
                                       laptop-w
                                     dell-w))))
                 (h (round (* 0.38 (if (eql setup 'laptop)
                                       laptop-h
                                     dell-h))))
                 (x (pcase setup
                      ('laptop (/ (- laptop-w w) 2))
                      ('dell (/ (- dell-w w) 2))
                      ('both (+ laptop-w
                                (/ (- dell-w w) 2)))))
                 (y (pcase (frame-parameter current-frame 'fullscreen)
                      ('fullboth 0)
                      (_ (let* ((title-h (cdr (alist-get 'title-bar-size (frame-geometry current-frame))))
                                (frame-y (cdr (frame-position current-frame))))
                           (+ frame-y title-h))))))
            (set-frame-size helm-popup-frame
                            w h 'pixelwise)
            (set-frame-position helm-popup-frame
                                x y))
          ;; FIX: Make helm support dynamic sizing instead.
          (setq helm-display-buffer-width (frame-width helm-popup-frame)
                helm-display-buffer-height (frame-height helm-popup-frame)))))))

(use-package helm-projectile
  :custom (projectile-enable-caching t))

(use-package helm-ag
  :custom ((helm-ag-insert-at-point t)
           (helm-ag-command-option "--follow")))

(use-package helm-swoop
  :custom ((helm-swoop-use-line-number-face t)
           (helm-swoop-speed-or-color nil)
           (helm-swoop-use-fuzzy-match t)
           (helm-swoop-pre-input-function (lambda () nil))))

(provide 'ublt-helm)
