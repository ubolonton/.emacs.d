(require 'ublt-util)

(require 'dired+)

;; `http://blog.nguyenvq.com/2009/12/01/file-management-emacs-dired-to-replace-finder-in-mac-os-x-and-other-os/'
;; linux;; multiple files
;; "nohup xdg-open" current-prefix-arg ;; linux can open multiple files, but one at a time
;; "see" current-prefix-arg ;; linux;; can open at most 1 file (being opened)
;; "open" current-prefix-arg ;; mac os x
(defun ublt/dired-open-native ()
  (interactive)
  (dolist (file (dired-get-marked-files t current-prefix-arg))
    (call-process (case system-type
                    ('darwin "open")
                    ('gnu/linux "gnome-open"))
                  nil 0 nil file)))

;; Highlight current line
(add-hook 'dired-mode-hook (ublt/on-fn 'hl-line-mode))

;; Hide details
(ublt/set-up 'dired-details+
  (setq dired-details-hide-link-targets nil)
  ;; Hide unimportant files
  (setq-default dired-omit-mode t
                dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\."))

;; Directories first by default. "s d" to change locally
(ublt/set-up 'dired-sort-map
  (setq dired-listing-switches "--group-directories-first -alh"))

(setq
 ;; Offer the other window's path as default when copying
 dired-dwim-target t

 ;; Make find-name-dired ignore case
 find-name-arg "-iname")

(provide 'ublt-dired)
