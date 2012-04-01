
;;; EMMS -------------------------------------------------------------
;;; Needs mpg312, ogg123, mplayer, vlc; mp3info, id3v2, ogginfo
;;; w3m for emms-all

;; Info location
(add-to-list 'Info-directory-list "~/.emacs.d/lib/emms/doc")

(defun ublt/emms-config ()
  "Based on `emms-all', without lastfm, which needs w3m, which
does not seem to work with latest Emacs versions. So emms users
use Emacs 21?!?!"
  ;; include
  (emms-standard)
  ;; define
  (eval-and-compile
    (require 'emms-mode-line)
    (require 'emms-mark)
    (require 'emms-tag-editor)
    (require 'emms-streams)
    (require 'emms-lyrics)
    (require 'emms-playing-time)
    (require 'emms-player-mpd)
    (require 'emms-player-xine)
    (require 'emms-playlist-sort)
    (require 'emms-browser)
    (require 'emms-mode-line-icon)
    (require 'emms-cue)
    (require 'emms-bookmarks)
    (require 'emms-last-played))
  ;; setup
  (emms-mode-line 1)
  (emms-mode-line-blank)
  (emms-lyrics 1)
  (emms-playing-time 1)
  (add-to-list 'emms-info-functions 'emms-info-cueinfo)
  (add-hook 'emms-player-started-hook 'emms-last-played-update-current))

(require 'emms-setup)
(ublt/emms-config)
(emms-default-players)
(require 'emms-info-id3v2)
(require 'emms-info-libtag)
(require 'emms-player-mpg321-remote)
;;; TODO: Find out optimal order
(setq emms-player-list '(
                         emms-player-mplayer
                         emms-player-vlc
                         emms-player-mpg321
                         emms-player-ogg123
                         emms-player-mpg321-remote
                         emms-player-mplayer-playlist
                         ))

(defgroup ubolonton nil ""
  :group 'personal)

;;; TODO: Windows?
(defface ublt/emms-mode-line-face
   '((t (:font "DejaVu Sans Condensed")))
   "EMMS mode-line string face")
(setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find
      later-do-interval 0.001
      emms-directory "~/.emacs.d/data/emms"
      emms-stream-bookmarks-file "~/.emacs.d/data/emms/streams"
      emms-source-file-default-directory "~/Music/"
      emms-lyrics-dir "~/Music/Lyrics/"
      emms-show-format (propertize "  %s" 'face 'variable-pitch)
      emms-info-functions '(emms-info-id3v2)
      emms-cache-file "~/.emacs.emms-cache"
      emms-playlist-buffer-name "*EMMS Playlist*")

;; Hmm, explicit defined face must be used, anonymous face does not
;; work
(defface ublt/emms-mode-line-face
   '((t (:font "DejaVu Sans Condensed 8")))
   "EMMS mode-line string face")
(defadvice emms-mode-line-playlist-current (after fontify activate)
  (setq ad-return-value (propertize ad-return-value
                                    'face 'ublt/emms-mode-line-face)))

(add-hook 'emms-player-started-hook 'emms-show)
;; TODO: Remove this?
;; Notifications
(ublt/set-up 'todochiku
  (defun ublt/emms-notify ()
    (let ((todochiku-timeout 2))
      (todochiku-message "EMMS" (emms-track-description
                                 (emms-playlist-current-selected-track))
                         (todochiku-icon 'music))))
  (defadvice emms-start (after notify activate)
    (run-at-time "0 sec" nil 'ublt/emms-notify)))

;; Don't confirm saving tag info
(defadvice emms-tag-editor-submit (around no-question activate)
  (flet ((y-or-n-p (m) t))
    ad-do-it))

;;; Randomize playlist songs' order
(defadvice emms-play-playlist (after shuffle activate)
  (emms-shuffle) (emms-next) (emms-shuffle))

(defun ublt/start-or-pause ()
  (if emms-player-playing-p
      (emms-pause)
    (call-interactively 'emms-play-playlist)))

(provide 'ublt-entertainment)
