(require 'ublt-util)
(require 'ublt-navigation)

(use-package magit
  :custom

  ;; Mainly because I want `magit-status' to work after a `find-library'.
  (vc-follow-symlinks t)

  ;; TODO: Figure out how make `diff-hl' work without 'Git in `vc-handled-backends'.
  ;; This is for performance: https://magit.vc/manual/magit/Performance.html.
  (vc-handled-backends (delq 'Git vc-handled-backends))

  ;; ;; Show original windows when quitting magit.
  ;; magit-restore-window-configuration t

  ;; magit status buffer should not be a pop-up (in the sense of
  ;; not being volatile or temporary like helm buffer). This is
  ;; important for small screens.
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)

  (magit-refresh-status-buffer nil)

  (magit-status-show-hashes-in-headers t)

  ;; NOTE: https://github.com/magit/magit/issues/1743
  ;; 2-way diff for staging is much more useful than 3-way.
  (magit-ediff-dwim-show-on-hunks t)

  ;; Git directories
  (magit-repository-directories `(("~/dotfiles". 0)
                                  (,user-emacs-directory . 0)
                                  ("~/Programming/projects" . 1)
                                  ("~/Programming/lib" . 1)
                                  ("~/Programming/Tools" . 1)
                                  ("~/Programming/projects/emacs-tree-sitter/langs/repos" . 1)
                                  ("~/Programming/pp/parcel-perform" . 1)
                                  ("~/Programming/pp/ml-team" . 1)
                                  (,(ublt/init-rel-path "straight/repos") . 1)))

  (magit-status-margin '(t age magit-log-margin-width t 10))

  ;; `all' would be nice, but it's too slow for large diffs
  (magit-diff-refine-hunk t)
  (magit-diff-paint-whitespace 'status)
  ;; Gravatars.
  (magit-revision-show-gravatars '("^Author:     " .  "^Commit:     "))

  (magit-log-margin '(t age magit-log-margin-width t 10))
  (magit-log-show-refname-after-summary t)

  ;; Other
  (magit-blame-mode-lighter "🔥")
  (magit-blame-time-format "%F")

  (magit-refs-show-commit-count 'branch)

  :config
  ;; XXX: The initialization of this is icky. We use `global-auto-revert-mode' anyway, so disable it here.
  (magit-auto-revert-mode -1)

  ;; TODO: Move this definition somewhere else.
  (defun ublt/disable-line-spacing ()
    (setq line-spacing 0))

  (add-hook 'magit-revision-mode-hook #'ublt/disable-line-spacing)

  (defun ublt/magit-log-last-change (&optional args files)
    "Show log for last change to HEAD (i.e. HEAD@{1}...HEAD)"
    (interactive (magit-log-arguments))
    (magit-log-setup-buffer '("HEAD@{1}...HEAD") args files))

  ;; FIX: Improve `transient' modification APIs to support e.g. searching by description.
  (dolist (addition '(("=m" ("-M" "Show merges only" "--merges"))
                      ("h" ("c" "last change" ublt/magit-log-last-change))))
    (apply #'transient-append-suffix 'magit-log addition))

  ;; Our default value for `fill-column' can be different.
  (add-hook 'git-commit-mode-hook
            (lambda () (setq fill-column 80)))

  (with-eval-after-load 'helm-info
    (ublt/helm-info-reload)))

(use-package git-commit
  :custom (git-commit-summary-max-length 70))

(use-package transient
  ;; `transient' doesn't seem to have a runtime show-all toggle.
  :custom (transient-default-level 7))

;;; We use this only for magit-todos, since we prefer our own coloring in code buffers.
(use-package hl-todo
  :config (add-to-list 'hl-todo-keyword-faces '("FIX"  . "#cc9393")))

(use-package magit-todos
  :disabled
  :config (magit-todos-mode +1))

(use-package magit-libgit2
  :disabled
  :ensure nil :straight nil
  :load-path ("~/Programming/projects/magit-libgit2/elisp"
              "~/Programming/projects/magit-libgit2/target/debug"))

(use-package git-timemachine)

(use-package gitconfig-mode)
(use-package gitattributes-mode)
(use-package gitignore-mode)

(provide 'ublt-git)
