;;; -*- lexical-binding: t; coding: utf-8 -*-
(require 'ublt-util)
(require 'map)

(use-package org
  :hook ((org-mode . hl-line-mode)
         (org-mode . highlight-parentheses-mode))

  :custom (;; Appearance

           ;; Allow setting sizes for inline images.
           (org-image-actual-width nil)

           ;; Show all headlines by default.
           (org-startup-folded 'showall)

           ;; Don't show empty lines in collapsed view.
           (org-cycle-separator-lines 0)

           ;; List additional (on top of 2) indentation.
           ;; I use a variable-pitch font, and same bullet for sub-levels, so use a large value.
           (org-list-indent-offset 2)

           (org-fontify-whole-heading-line t)
           (org-fontify-whole-block-delimiter-line nil)
           (org-fontify-quote-and-verse-blocks t)

           ;; Hide / * _ ~ =
           (org-hide-emphasis-markers t)

           ;; Column-based aligning doesn't work with variable-pitch fonts.
           (org-tags-column 0)

           ;; Fontify code blocks.
           (org-src-fontify-natively t))

  :custom (;; Navigation

           ;; Don't use isearch there, normal isearch is enough
           (org-goto-auto-isearch nil)
           ;; Don't use `outline-path-completion'. `helm-org-in-buffer-headings' is better
           (org-goto-interface 'outline)
           ;; Whole path instead of level-by-level navigation
           (org-outline-path-complete-in-steps nil))


  :custom (;; Other

           (org-indirect-buffer-display 'current-window)
           ;; Intelligent (dwim) bindings
           (org-special-ctrl-a/e t)
           (org-special-ctrl-k t)

           ;; Disallow editing folded content
           (org-catch-invisible-edits 'error)

           (org-read-date-prefer-future 'time)

           ;; Don't interpret _ and ^ as sub/super-scripts
           (org-use-sub-superscripts '{})
           (org-export-with-sub-superscripts '{})

           ;; org-show-entry-below t

           ;; Don't split current heading, create a new one
           (org-M-RET-may-split-line nil)
           ;; Create new heading after the current content
           (org-insert-heading-respect-content t)

           ;; Allow using alphabetical bullets
           (org-list-allow-alphabetical t)

           ;; 2 lines to terminate lists
           (org-empty-line-terminates-plain-lists nil)

           (org-list-use-circular-motion t)

           ;; ;; Different list levels should use different bullets
           ;; (org-list-demote-modify-bullet '(("-" . "+")
           ;;                                  ("+" . "-")
           ;;                                  ("1." . "1)")
           ;;                                  ("1)" . "1.")))

           ;; org-footnote-auto-adjust t

           ;; ;; Don't number headlines
           (org-export-with-section-numbers nil)

           (org-refile-use-outline-path t))

  :config
  ;;; Evaluation of embedded code
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (plantuml . t)
     (shell . t)
     (python . t))))

(use-package org-indent
  :ensure nil :straight nil
  :custom (org-indent-indentation-per-level 3)
  :hook (org-mode . org-indent-mode))

(use-package org-bullets
  :custom (org-bullets-bullet-list '("•"))
  :hook (org-mode . org-bullets-mode))

(use-package ob-plantuml
  :ensure nil :straight nil
  :defer t
  :custom (org-plantuml-jar-path "~/bin/plantuml.jar"))

;; ---------------------------------------------------------------------------
;;; Task management, GTD

;;; TODO: Define tags (upper-case: specific, lower-case: generic)
;;; home | work
;;; computer
;;; read
;;; think
;;; learn
;;; teach
;;; chore
;;; diary
;;; note
;;; Emacs

;;; Status keywords
(use-package org
  :demand t
  :custom ((org-todo-keywords
            '((sequence "TODO(t)" "STARTED(s!)"
                        "WAITING(w@/!)" "PAUSED(p@/!)"
                        "|"
                        "DONE(d!)" "CANCELLED(c@)")))

           ;; TODO: Add more colors
           (org-todo-keyword-faces
            '(("TODO" :foreground "#F86155" :weight normal)
              ("PAUSED" :foreground "Firebrick" :weight normal)
              ("STARTED" :foreground "DarkGoldenRod" :weight normal)
              ("CANCELLED" :foreground "SeaGreen" :weight normal)
              ("DONE" :foreground "LightGreen" :weight normal)))

           (org-use-fast-todo-selection t)

           ;; Allow S-<left> and S-<right> to change state without logging the change
           (org-treat-S-cursor-todo-selection-as-state-change nil)

           ;; Use with `org-toggle-ordered-property'
           (org-enforce-todo-dependencies t)

           ;; org-stuck-projects '("+PROJECT/-DONE" ("DONE") ("*") "")
           ;; org-stuck-projects '("" nil nil "")

           (org-tag-alist '((:startgroup . nil)
                            ("@Home" . ?h) ("@Work" . ?w) ("@Road" . ?o)
                            (:endgroup . nil)
                            ("Emacs" . e)
                            ("Movie" . m)
                            ("Book" . b)))

           (org-log-done nil)
           (org-log-reschedule 'time)
           (org-log-repeat 'time)
           (org-log-redeadline 'time)
           (org-log-note-clock-out nil)
           (org-log-into-drawer t)))

(use-package org-clock
  :ensure nil :straight nil
  :custom ((org-clock-history-length 24)
           (org-clock-in-resume t)

           (org-clock-into-drawer t)
           (org-clock-out-remove-zero-time-clocks t)
           (org-clock-out-when-done t)
           (org-clock-persistent t)

           (org-clock-in-switch-to-state 'ublt/clock-in-to-next))
  :config
  (org-clock-persistence-insinuate)

  (defun ublt/clock-in-to-next (kw)
    (when (equal kw "TODO")
      "STARTED")))


(use-package org-agenda
  :ensure nil :straight nil
  :hook ((org-agenda-mode . hl-line-mode)
         (org-agenda-finalize . ublt/org-agenda-align-tags))
  :custom ((org-agenda-dim-blocked-tasks t)
           (org-agenda-start-on-weekday nil)
           (org-agenda-skip-scheduled-if-deadline-is-shown nil)
           (org-agenda-skip-deadline-if-done nil)
           (org-agenda-skip-scheduled-if-done nil)
           (org-agenda-span 'week)

           (org-agenda-time-grid '((daily today require-timed)
                                   "-----------------------"
                                   (list 0700 0800
                                         1000 1100 1200
                                         1400 1500 1600 1700 1800 1900
                                         2100 2200 2300)))

           ;; org-scheduled-past-days 365

           (org-agenda-log-mode-items '(closed clock state))

           (org-agenda-tags-column 0)
           (org-agenda-window-setup 'current-window)
           (org-agenda-restore-windows-after-quit t)

           (org-agenda-persistent-filter t)
           (org-agenda-persistent-marks t)

           ;; TODO: Maybe more
           (org-agenda-files '("~/org/gtd/tasks.org"
                               ;; "~/org/gtd/tasks.org"
                               ;; "~/org/gtd/journal.org"
                               ))

           ;; ;; TODO: Use another file, as this is meant for notes not actually tasks
           ;; (org-default-notes-file "~/org/gtd/someday.org")

           ;; TODO: Make this context-aware (e.g. notes should go to journal.org or the appropriate
           ;; note file, not tasks.org).
           (org-refile-targets
            '(;; Free-style tasks and backlog.
              (("~/org/gtd/tasks.org") . (:level . 1))
              (("~/org/gtd/someday.org") . (:maxlevel . 1))
              ;; ;; TODO: Create date entry when necessary.
              ;; (("~/org/journal.org") . (:level . 2))
              ;; Projects contain both tasks/backlog and notes, thus are not under gtd/.
              (("~/org/projects/emacs-module-rs.org") . (:regexp . "Tasks"))
              (("~/org/projects/emacs-tree-sitter.org") . (:regexp . "Tasks"))
              (("~/org/projects/configure-emacs.org") . (:regexp . "Tasks"))
              (nil . (:maxlevel . 2))))

           (org-refile-allow-creating-parent-nodes 'confirm)
           (org-refile-target-verify-function 'ublt/verify-refile-target)

           ;; org-columns-default-format "%TODO %50ITEM %TAGS %CATEGORY"

           ;; TODO: More
           (org-agenda-custom-commands '(;; ("P" "Projects summary"
                                         ;;  ((tags "PROJECT"
                                         ;;         ((org-agenda-todo-list-sublevels nil)))))
                                         ;; ("p" "Projects details"
                                         ;;  ((tags "PROJECT")))
                                         ;; ("w" "Work"
                                         ;;  ((agenda "")
                                         ;;   (tags-todo "SCHEDULED=\"\""))
                                         ;;  ((org-agenda-files '("~/org/work/arimo/work-notes.org"))
                                         ;;   (org-agenda-ndays 1)
                                         ;;   (org-agenda-sorting-strategy
                                         ;;    '((agenda todo-state-up time-up priority-down)))
                                         ;;   (org-deadline-warning-days 0)
                                         ;;   (org-agenda-prefix-format
                                         ;;    '((agenda  . "%i %-9:c%?-12t%-12s")
                                         ;;      (tags  . "%i %-9:c%?-12t")))))
                                         ("D" "Daily action list"
                                          (
                                           ;; Tasks with a date (deadline, scheduled)
                                           (agenda "")
                                           ;; Unscheduled tasks
                                           (tags-todo "SCHEDULED=\"\""))
                                          ((org-agenda-files '("~/org/gtd/tasks.org"))
                                           (org-agenda-ndays 2)
                                           (org-agenda-sorting-strategy
                                            '((agenda todo-state-up time-up priority-down)))
                                           (org-deadline-warning-days 0)
                                           (org-agenda-prefix-format
                                            '((agenda  . "%i %-9:c%?-12t%-12s")
                                              (tags  . "%i %-9:c%?-12t")))))
                                         ("c" "Tasks in progress (current)"
                                          ((todo "STARTED")))

                                         ;; TODO: Different views for different types, e.g. books, movies
                                         ;; ("S" "\"Someday\" task list"
                                         ;;  ((todo "TODO" ((org-agenda-files '("~/org/gtd/someday.org"))
                                         ;;                 (org-agenda-sorting-strategy
                                         ;;                  '((todo . (category-down))))
                                         ;;                 (org-agenda-prefix-format
                                         ;;                  '((todo . "%-9:c")))))))
                                         )))
  :config
  (defun ublt/verify-refile-target ()
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (defun ublt/org-agenda-align-tags ()
    (let ((org-agenda-tags-column (- 4 (window-width))))
      (org-agenda-align-tags))))

;;; Task creation (capturing/remembering)
(use-package org-capture
  :ensure nil :straight nil
  :custom
  (org-capture-templates '(("n" "Note"
                            entry (file "~/org/gtd/daily-input.org")
                            (file "~/org/gtd/templates/quick-note.org")
                            :empty-lines-before 1)
                           ("t" "Task"
                            entry (file "~/org/gtd/daily-input.org")
                            (file "~/org/gtd/templates/quick-task.org")
                            :empty-lines-before 1)

                           ;; TODO: Capture link sent or clipboard content sent from other
                           ;; places

                           ;; Handle both internal invocation and external protocol handling
                           ;; org-protocol://capture://l/<url>/<title or " ">[/description]

                           ("L" "Link")
                           ("Lf" "From Firefox"
                            entry (file "~/org/gtd/daily-input.org")
                            (file "~/org/gtd/templates/quick-link.org")
                            :empty-lines-before 1)

                           ;; TODO: Add project-specific task capturing.

                           ("r" "Review"
                            entry (file "~/org/journal.org")
                            "** %t Daily Review :Coach:\n%[~/org/gtd/templates/daily-review.txt]\n" )))

  :config (defun ublt/org-capture-link-format-selection (d)
            (if (and (not (string-equal d "%i"))
                     (> (length d) 0))
                (format "
#+BEGIN_QUOTE
%s
#+END_QUOTE
" d)
              "")))

(use-package org-protocol
  :ensure nil :straight nil
  :hook (org-capture-after-finalize . ublt/org-capture-return-maybe)
  :config
  (defvar ublt/org-capture-info nil)

  ;; Set the flag before capturing
  (define-advice org-protocol-capture (:around (f info &rest args) ublt/set-capture-info)
    "Start temporarily tracking capture info."
    (setq ublt/org-capture-info info)
    (condition-case err
        (apply f info args)
      (error (setq ublt/org-capture-info nil)
             (signal (car err) (cdr err)))))

  ;; Unset it afterward
  (define-advice org-capture-finalize (:around (f &rest args) ublt/unset-capture-info)
    "Unset capture info after finishing capturing."
    (unwind-protect (apply f args)
      (setq ublt/org-capture-info nil)))

  (define-advice org-capture (:around (f &rest args) ublt/handle-abort)
    (condition-case err
        (apply f args)
      (error (let ((org-note-abort t)
                   (err-data (cdr err)))
               (when (string-equal (car err-data) "Capture abort: Quit")
                 (ublt/org-capture-return-maybe))
               (signal (car err) err-data)))))

  (defun ublt/org-capture-return-maybe ()
    (pcase system-type
      ('darwin
       (pcase (plist-get ublt/org-capture-info :template)
         ("Lf"
          ;; Emacs may activate itself later, so we defer this until returning to the command loop.
          (run-with-idle-timer
           0 nil
           (lambda (abort)
             (do-applescript "
tell application \"System Events\" to tell first process whose bundle identifier is \"org.mozilla.Firefox\"
    set frontmost to true
end tell")
             (unless abort
               (do-applescript "
tell application \"System Events\" to tell first process whose bundle identifier is \"org.mozilla.Firefox\"
    tell menu 1 of menu bar item \"File\" of menu bar 1
        click menu item \"Close Tab\"
    end tell
end tell")))
           org-note-abort)))))))


;; ---------------------------------------------------------------------------
;;; Static HTML export

(use-package ox-html
  :ensure nil :straight nil
  :custom (org-html-htmlize-output-type 'css))

(use-package ox-hugo
  :after ox
  :config
  (define-advice org-hugo--gen-front-matter (:around (f data &rest args) ublt/inject-weight)
    "Add weight to front matter in a way that preserve org's tree ordering."
    (setf (map-elt data 'weight) (line-number-at-pos nil :absolute))
    (apply f data args)))

;; ---------------------------------------------------------------------------
;;; Slides for presentation

(use-package ox-reveal
  :custom ((org-reveal-root "reveal.js/")
           (org-reveal-transition "linear")
           (org-reveal-transition-speed "fast")
           (org-reveal-history t)
           (org-reveal-control t)
           (org-reveal-progress t)
           (org-reveal-rolling-links nil)))

;; ---------------------------------------------------------------------------
;;; PDF export

(use-package ox-latex
  :ensure nil :straight nil
  :defer t
  :custom ((org-latex-listings 'minted)
           (org-latex-pdf-process '("xelatex -shell-escape -interaction nonstopmode %f"
                                    "xelatex -shell-escape -interaction nonstopmode %f"))
           (org-latex-toc-command "")
           ;; Don't use `inputenc', `fontenc' (xelatex `fontspec' is
           ;; supposed to handle unicode better)
           (org-latex-default-packages-alist
            '(("" "fixltx2e" nil)
              ("" "graphicx" t)
              ("" "longtable" nil)
              ("" "float" nil)
              ("" "wrapfig" nil)
              ("" "rotating" nil)
              ("normalem" "ulem" t)
              ("" "amsmath" t)
              ("" "textcomp" t)
              ("" "marvosym" t)
              ("" "wasysym" t)
              ("" "amssymb" t)
              ("" "hyperref" t)
              "\\tolerance=1000")))
  :config
  ;; Use minted for code highlighting in exported pdf files
  ;; http://joat-programmer.blogspot.nl/2013/07/org-mode-version-8-and-pdf-export-with.html
  ;; https://tex.stackexchange.com/questions/367332/minted-error-undefined-control-sequence-pyg-with-texmaker
  ;; https://github.com/gpoore/minted/issues/92
  (add-to-list 'org-latex-packages-alist '("cache=false" "minted"))

  (add-to-list 'org-latex-classes
               `("ublt-pp-beamer"
                 ,(ublt/get-string-from-file "~/org/work/pp/ublt-pp-beamer.tex")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

  (add-to-list 'org-latex-classes
               `("ublt-org-article"
                 ,(ublt/get-string-from-file (ublt/init-rel-path "config/ublt-org-article.tex"))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(use-package ox-beamer
  :ensure nil :straight nil
  :after ox-latex)

;; ---------------------------------------------------------------------------
;;; TODO: Move this out into a separate file, allowing it to be byte-compiled.
(use-package org
  :hook (org-mode . ublt/-prettify-org)
  :config
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")

  (defun ublt/-not-in-org-src-block (beg end)
    (notany (lambda (overlay)
              (eq (overlay-get overlay 'face) 'org-block-background))
            (overlays-in beg end)))

  (defun ublt/-prettify-org ()
    (font-lock-add-keywords nil '(("\\(=>\\)"
                                   (0 (ublt/show-as ?⟹ #'ublt/-not-in-org-src-block)))
                                  ("\\(<=\\)"
                                   (0 (ublt/show-as ?⟸ #'ublt/-not-in-org-src-block)))
                                  ("\\(->\\)"
                                   (0 (ublt/show-as ?⟶ #'ublt/-not-in-org-src-block)))
                                  ("\\(<-\\)"
                                   (0 (ublt/show-as ?⟵ #'ublt/-not-in-org-src-block)))
                                  ("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                                    1 'org-checkbox-done-text prepend))
                            'append)
    (setq-local prettify-symbols-alist '(("[ ]" . "☐")
                                         ("[X]" . "☑" )
                                         ("[-]" . "❍" )
                                         ("#+begin_src" . "⌨")
                                         ("#+BEGIN_SRC" . "⌨")
                                         ("#+end_src" . "⌨")
                                         ("#+END_SRC" . "⌨")))
    (prettify-symbols-mode)))


(provide 'ublt-organization)
