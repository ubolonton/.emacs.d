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
(setq
 org-agenda-files '("~/gtd/gtd.org"
                    "~/gtd/journal.org"
                    "~/gtd/projects.org")
 org-default-notes-file "~/gtd/someday.org"
 org-columns-default-format "%TODO %50ITEM %TAGS %CATEGORY"
 org-stuck-projects '("+PROJECT/-DONE" ("DONE") ("*") "")
 org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "|" "DONE(d!/!)"))
 org-todo-keyword-faces
 '(("TODO" :foreground "Red" :weight normal)
   ("DEFERRED" :foreground "DeepSkyBlue" :weight normal)
   ("STARTED" :foreground "DarkGoldenRod" :weight normal)
   ("CANCELLED" :foreground "Gray15" :weight normal)
   ("DONE" :foreground "LightGreen" :weight normal)
   )
 org-refile-targets '((("~/gtd/projects.org") . (:maxlevel . 2))
                      (("~/gtd/gtd.org") . (:maxlevel . 1))
                      (("~/gtd/someday.org") . (:maxlevel . 1)))
 org-remember-templates
 '(("Todo"    ?t "** TODO %^{Brief Description} %^g\n   - Added: %U\n%?"   "~/gtd/gtd.org" "Tasks")
   ("Someday" ?s "** TODO %^{Do this someday} %^g\n   - Added: %U\n%?"     "~/gtd/someday.org")
   ("Diary"   ?d "** %U %^{Diary} :Diary:%^g\n%i%?"                        "~/gtd/journal.org")
   ("Idea"    ?i "** %U %^{Thought} :Thought:%^g\n%i%?"                    "~/gtd/journal.org")
   ("Review"  ?r "** %t Daily Review :Coach:\n%[~/gtd/daily-review.txt]\n" "~/gtd/journal.org")
   ("Project" ?p "** TODO %^{Project Name}\n  - Added: %U\n%?"             "~/gtd/projects.org" "Projects"))
 org-agenda-custom-commands
 '(("P" "Projects" ((tags "PROJECT")))
   ("D" "Daily action list"
    ((agenda "" ((org-agenda-ndays 1)
                 (org-agenda-sorting-strategy
                  '((agenda time-up priority-down tag-up)))
                 (org-deadline-warning-days 0)))))
   ("S" "'Someday' task list"
    ((todo "TODO" ((org-agenda-files '("~/gtd/someday.org"))
                   (org-agenda-prefix-format '((todo . "  ")))))))))
(setq org-src-fontify-natively t)
(setq org-babel-scheme-cmd "mzscheme")
(add-hook 'org-agenda-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'org-agenda-mode-hook 'local-column-number-mode)
(add-hook 'org-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'org-mode-hook 'local-column-number-mode)
(require 'org-install)
(require 'remember)
(org-remember-insinuate)
(require 'ob-scheme)

;; Use external browser
(add-to-list 'org-file-apps '("\\.x?html?\\'" browse-url file))

;;; Wordpress blogging
(ublt/set-up 'org2blog-autoloads
  (setq org2blog/wp-blog-alist '(("ubolonton"
                                  :url "http://ubolonton.wordpress.com/xmlrpc.php"
                                  :username "ubolonton"
                                  :default-title "From Emacs"
                                  :default-categories ("org2blog" "emacs")
                                  :tags-as-categories nil))
        org2blog/wp-confirm-post nil))


(provide 'ublt-organization)
