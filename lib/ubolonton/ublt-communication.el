
;;; Twitter

;;; Basic settings
(require 'twittering-mode)
(twittering-icon-mode +1)
(defface ublt-twitter-meta-face
   '((t (:inherit font-lock-comment-face)))
   "Twitter face for important text")
(setq twittering-status-format
      "%i %s,  %FACE[ublt-twitter-meta-face]{%@  from %f%L%r%R}\n\t%t"
      twittering-url-show-status nil
      twittering-timer-interval 600
      twittering-use-master-password t
      twittering-use-icon-storage t
      twittering-display-remaining t
      twittering-new-tweets-count 300
      )
(add-hook 'twittering-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'twittering-edit-mode-hook '(lambda () (auto-fill-mode -1)))

;;; Notifications
;; `http://www.emacswiki.org/emacs/TwitteringMode'
(require 'todochiku)
(defun ublt/notify-tweets ()
  (let ((n twittering-new-tweets-count)
        (todochiku-timeout 2))
    (if (> n 10)
        (todochiku-message
         (twittering-timeline-spec-to-string twittering-new-tweets-spec)
         (format "You have %d new tweet%s"
                 n (if (> n 1) "s" ""))
         (todochiku-icon 'social))
      (dolist (el twittering-new-tweets-statuses)
        (todochiku-message
         (twittering-timeline-spec-to-string twittering-new-tweets-spec)
         (concat (cdr (assoc 'user-screen-name el))
                 " said: "
                 (cdr (assoc 'text el)))
         (todochiku-icon 'social))))))
(add-hook 'twittering-new-tweets-hook 'ublt/notify-tweets)

;;; ERC

(ublt/set-up 'erc
  (setq erc-autojoin-channels-alist
        '(("freenode.net" "#emacs" "#conkeror" "#clojure")
          ("cogini.org" "#cogini"))
        erc-nick "ubolonton"
        erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
        erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477")
        erc-log-channels-directory "~/erc_logs/"
        erc-save-buffer-on-part t
        erc-save-queries-on-quit t
        erc-log-write-after-send t
        erc-log-write-after-insert t)
  (defun ublt/erc ()
    (interactive)
    (erc :server "irc.cogini.org" :port 6667 :nick "ubolonton")
    (call-interactively 'erc)))

;;; Emails (wanderlust)
;;; `http://emacs-fu.blogspot.com/2009/06/e-mail-with-wanderlust.html'
;;; `http://www.mail-archive.com/emacs-orgmode@gnu.org/msg20250/wlconfiguration.org'

(setq user-mail-address "ubolonton@gmail.com"
      wl-from "Nguyễn Tuấn Anh <ubolonton@gmail.com>")

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "ubolonton@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls
      wl-smtp-posting-port 587
      wl-smtp-authenticate-type "plain"
      wl-smtp-posting-user "ubolonton"
      wl-smtp-posting-server "smtp.gmail.com"
      wl-insert-message-id nil          ; Message ID by server, no warning
      wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox"
      wl-default-spec "%"
      wl-draft-folder "%[Gmail]/Drafts" ; Gmail IMAP
      wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t
      wl-stay-folder-window t           ; Keep folder pane
      )

(setq mime-edit-split-message nil)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; Hide many fields from message buffers
(setq wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list '("^\\(To\\|Cc\\):"
                                      "^Subject:"
                                      "^\\(From\\|Reply-To\\):"
                                      "^Organization:"
                                      "^Message-Id:"
                                      "^\\(Posted\\|Date\\):"
                                      )
      wl-message-sort-field-list '("^From"
                                   "^Organization:"
                                   "^X-Attribution:"
                                   "^Subject"
                                   "^Date"
                                   "^To"
                                   "^Cc"))

(provide 'ublt-communication)
