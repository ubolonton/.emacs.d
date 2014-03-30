(require 'ublt-util)

(require 'org)

(ublt/set-up 'org-indent
  (add-hook 'org-mode-hook (ublt/on-fn 'org-indent-mode)))

(setq
 ;; Intelligent (dwim) bindings
 org-special-ctrl-a/e t
 org-special-ctrl-k t

 ;; Show all headlines by default
 org-startup-folded t

 org-catch-invisible-edits 'smart

 ;; org-show-entry-below t

 ;; Don't use isearch there, normal isearch is enough
 org-goto-auto-isearch nil
 ;; Don't use `outline-path-completion'. `ido-imenu' is better
 org-goto-interface 'outline

 ;; Don't current heading, create a new one
 org-M-RET-may-split-line nil
 ;; Create new heading after the current content
 org-insert-heading-respect-content t

 ;; Heading visual indentation
 org-indent-indentation-per-level 1
 ;; List additional (on top of 2) indentation
 org-list-indent-offset 1

 ;; Allow using alphabetical bullets
 org-alphabetical-lists t

 ;; 2 lines to terminate lists
 org-empty-line-terminates-plain-lists nil

 org-list-use-circular-motion t

 ;; Different list levels should use different bullets
 org-list-demote-modify-bullet '(("-" . "+")
                                 ("+" . "-")
                                 ("1." . "1)")
                                 ("1)" . "1."))

 ;; org-footnote-auto-adjust t

 ;; Fontify code blocks
 org-src-fontify-natively t

 org-export-htmlize-output-type 'css
 )

(setq
 ;; TODO: Maybe just file-local
 org-todo-keywords
 '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
   (sequence "THINK" "WRITE(w)" "|" "PUBLISHED(p)")
   (sequence "DESIGN" "IMPLEMENT(i)" "TEST" "|" "DONE(d)")
   (sequence "|" "CANCELED(c)"))

 ;; Use with `org-toggle-ordered-property'
 org-enforce-todo-dependencies t

 ;; Log a lot
 org-log-done 'note
 org-log-reschedule 'note
 org-log-repeat 'note
 org-log-redeadline t
 org-log-note-clock-out t)

(ublt/set-up 'org-agenda
  (setq
   org-agenda-dim-blocked-tasks t
   org-agenda-skip-scheduled-if-deadline-is-shown t))

(ublt/set-up 'org-clock
  (setq
   org-clock-persist 'history)
  (org-clock-persistence-insinuate))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (clojure . t)
   (sh . t)))


(add-to-list 'auto-mode-alist '("\\.\\(org\\)$" . org-mode))
(setq org-log-done nil
      org-use-fast-todo-selection t
      org-tags-column -97
      org-completion-use-ido t
      org-agenda-tags-column -132
      org-agenda-start-on-weekday nil
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-span 7)

;; (setq
;;  org-agenda-files '("~/gtd/gtd.org"
;;                     "~/gtd/journal.org"
;;                     "~/gtd/projects.org")
;;  org-default-notes-file "~/gtd/someday.org"
;;  org-columns-default-format "%TODO %50ITEM %TAGS %CATEGORY"
;;  org-stuck-projects '("+PROJECT/-DONE" ("DONE") ("*") "")
;;  org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)"))
;;  org-todo-keyword-faces
;;  '(("TODO" :foreground "Red" :weight normal)
;;    ("DEFERRED" :foreground "DeepSkyBlue" :weight normal)
;;    ("STARTED" :foreground "DarkGoldenRod" :weight normal)
;;    ("CANCELLED" :foreground "Gray15" :weight normal)
;;    ("DONE" :foreground "LightGreen" :weight normal)
;;    )
;;  org-refile-targets '((("~/gtd/projects.org") . (:maxlevel . 2))
;;                       (("~/gtd/gtd.org") . (:maxlevel . 1))
;;                       (("~/gtd/someday.org") . (:maxlevel . 1)))
;;  org-remember-templates
;;  '(("Todo"    ?t "** TODO %^{Brief Description} %^g\n   - Added: %U\n%?"   "~/gtd/gtd.org" "Tasks")
;;    ("Someday" ?s "** TODO %^{Do this someday} %^g\n   - Added: %U\n%?"     "~/gtd/someday.org")
;;    ("Diary"   ?d "** %U %^{Diary} :Diary:%^g\n%i%?"                        "~/gtd/journal.org")
;;    ("Idea"    ?i "** %U %^{Thought} :Thought:%^g\n%i%?"                    "~/gtd/journal.org")
;;    ("Review"  ?r "** %t Daily Review :Coach:\n%[~/gtd/daily-review.txt]\n" "~/gtd/journal.org")
;;    ("Project" ?p "** TODO %^{Project Name}\n  - Added: %U\n%?"             "~/gtd/projects.org" "Projects"))
;;  org-agenda-custom-commands
;;  '(("P" "Projects" ((tags "PROJECT")))
;;    ("D" "Daily action list"
;;     ((agenda "" ((org-agenda-ndays 1)
;;                  (org-agenda-sorting-strategy
;;                   '((agenda time-up priority-down tag-up)))
;;                  (org-deadline-warning-days 0)))))
;;    ("S" "'Someday' task list"
;;     ((todo "TODO" ((org-agenda-files '("~/gtd/someday.org"))
;;                    (org-agenda-prefix-format '((todo . "  ")))))))))

;; (setq org-babel-scheme-cmd "mzscheme")
;; (add-hook 'org-agenda-mode-hook (ublt/on-fn 'hl-line-mode))
;; (add-hook 'org-agenda-mode-hook 'esk-local-column-number-mode)
(add-hook 'org-mode-hook (ublt/on-fn 'hl-line-mode))
;; (add-hook 'org-mode-hook 'esk-local-column-number-mode)

;; (require 'remember)
;; (require 'org-remember)
;; (org-remember-insinuate)

;; Use external browser
(add-to-list 'org-file-apps '("\\.x?html?\\'" browse-url file))

;; ;;; Wordpress blogging
;; (ublt/set-up 'org2blog-autoloads
;;   (setq org2blog/wp-blog-alist '(("ubolonton"
;;                                   :url "http://ubolonton.wordpress.com/xmlrpc.php"
;;                                   :username "ubolonton"
;;                                   :default-title "From Emacs"
;;                                   :default-categories ("org2blog" "emacs")
;;                                   :tags-as-categories nil))
;;         org2blog/wp-confirm-post nil))

(ublt/set-up 'org-html-slideshow
  (defun ublt/org-html-slideshow-decorate ()
    (goto-char (point-max))
    (insert "
#+BEGIN_HTML
<script type='text/javascript' src='js/org-html-slideshow.js'></script>
#+END_HTML

# Local Variables:
# org-export-html-style-include-default: nil
# org-export-html-style-include-scripts: nil
# End:
"))

  (defun ublt/org-html-slideshow-publish ()
    (interactive)
    (let ((org-export-preprocess-hook (cons 'ublt/org-html-slideshow-decorate org-export-preprocess-hook))
          (org-export-html-style-extra "
<link rel='stylesheet' type='text/css' href='css/common.css' />
<link rel='stylesheet' type='text/css' href='css/screen.css' media='screen' />
<link rel='stylesheet' type='text/css' href='css/projection.css' media='projection' />
<link rel='stylesheet' type='text/css' href='css/presenter.css' media='presenter' />
"))
      (call-interactively 'org-export-as-html))))

(ublt/set-up 'org-publish
  (ublt/set-up 'o-blog
    (dolist (project
             '(("blog"
                :base-directory "~/Programming/projects/blog/src/"
                :publishing-directory "~/Programming/projects/blog/public")
               ("o-blog"
                :base-directory "~/.emacs.d/lib/o-blog/example/"
                :publishing-directory "~/.emacs.d/lib/o-blog/example/out")))
      (add-to-list 'org-publish-project-alist project))

    ;; FIX
    (defun ublt/org-publish-blog-sync (file)
      (let ((file1 (format "%s.tmp.org" file)))
        (copy-file file file1 1)
        (org-publish-blog-sync file1)
        (let ((tmp (get-file-buffer file1)))
          (when tmp
            (with-current-buffer tmp
              (set-buffer-modified-p nil)
              (kill-buffer))))
        (delete-file file1)))

    (add-hook 'o-blog-before-publish-hook 'org-export-handle-include-files)

    ;; Use this instead of `org-publish-blog'
    (defun ublt/org-publish-o-blog (&optional file)
      "Publish FILE as a blog synchronously, processing #+INCLUDE
statements."
      (interactive
       (list (or
              (when (eq major-mode 'org-mode) (buffer-file-name))
              (read-file-name "Publish blog from: " nil nil t))))
      (ublt/org-publish-blog-sync file))))

(ublt/set-up 'ox-reveal
  (setq org-reveal-root "reveal.js/"

        org-reveal-transition "linear"
        org-reveal-transition-speed "fast"

        org-reveal-history t
        org-reveal-control t
        org-reveal-progress t
        org-reveal-rolling-links nil))

(ublt/set-up 'org-compat)
(ublt/set-up 'ob-exp)
(ublt/set-up 'ox)
(ublt/set-up 'ob-sql)
(ublt/set-up 'ob-scheme)

(provide 'ublt-organization)
