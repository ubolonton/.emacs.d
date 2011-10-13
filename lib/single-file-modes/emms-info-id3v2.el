;;; emms-info-id3v2.el --- Info-method for EMMS using id3v2

;; Copyright (C) 2010 tlh <thunkout@gmail.com>

;; File:      emms-info-id3v2.el
;; Author:    tlh <thunkout@gmail.com>
;; Created:   2010-08-11
;; Version:   1.0
;; Keywords:

;; This file is NOT part of EMMS

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;;    id3v2 info-method for EMMS using the "id3v2" program
;;

;;; Installation:
;;
;;  - You need to have `id3v2' program installed. Available here:
;;
;;    http://id3v2.sourceforge.net/
;;
;;  - Put `emms-info-id3v2.el' somewhere on your emacs load path
;;
;;  - Add these lines to your .emacs file:
;;
;;    (require 'emms-info-id3v2)
;;    (add-to-list 'emms-info-functions 'emms-info-id3v2)
;;

;;; Code:

(require 'emms-info)

(defgroup emms-info-id3v2 nil
  "Info-method for EMMS using id3v2."
  :group 'emms
  :version "1.0")

(defcustom emms-info-id3v2-frames
  '(("TPE1" . info-artist)
    ("TCOM" . info-composer)
    ("TPE2" . info-performer)
    ("TIT2" . info-title)
    ("TALB" . info-album)
    ("TRCK" . info-tracknumber)
    ("TYER" . info-year)
    ("COMM" . info-note)
    ("TCON" . info-genre)
    ("TLEN" . info-playing-time))
  "An alist mapping id3v2 frames to emms info fields.
Customize this if you wish to map different id3v2 frames to emms
info fields."
  :type 'alist
  :group 'emms-info-id3v2)

(defcustom emms-info-id3v2-program "id3v2"
  "Program (should be \"id3v2\") to use for id3v2 info."
  :type 'string
  :group 'emms-info-id3v2)

(defun emms-info-id3v2 (track)
  "Set TRACK with id3v2 tags using `emms-track-set'."
  (when (and (eq 'file (emms-track-type track))
             (string-match "\\.[Mm][Pp]3\\'" (emms-track-name track)))
    (with-temp-buffer
      (when (zerop (call-process emms-info-id3v2-program nil t nil "-l"
                                 (emms-track-name track)))
        (goto-char (point-min))
        (while (re-search-forward "^\\([0-9A-Z]\\{4\\}\\) .+?: \\(.+\\)$" nil t)
          (let ((field (cdr (assoc (match-string 1) emms-info-id3v2-frames))))
            (when field
              (emms-track-set track field (match-string 2)))))))))

(provide 'emms-info-id3v2)

;;; emms-info-id3v2.el ends here
