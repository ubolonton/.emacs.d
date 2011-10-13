;;; dired-sort-map.el --- in Dired: press s then s, x, t or n to sort by Size, eXtension, Time or Name

;; Copyright (C) 2002 -> Free Software Foundation, Inc.

;; Inspired by Francis J. Wright's dired-sort-menu.el
;; Author: Patrick Anderson, Nguyen Tuan Anh
;; Version: 2.1a

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;;; Install:
;; Copy this file to a directory in your load path.
;; Execute: M-x eval-buffer :so you don't have to restart.
;; Add the line: (require 'dired-sort-map) : to your .emacs

;;; Todo:
;; (add-hook
;;  'dired-load-hook
;;  '(lambda ()
;;     (progn
;; FIX: reverse/dir-group are reset when change sort type


;;; Code:
(require 'dired)
(defvar dired-sort-map (make-sparse-keymap))

(define-key dired-mode-map "s" dired-sort-map)

(define-key dired-sort-map "s" (lambda () "sort by Size" (interactive) (dired-sort-other (concat dired-listing-switches " -S"))))
(define-key dired-sort-map "x" (lambda () "sort by eXtension" (interactive) (dired-sort-other (concat dired-listing-switches " -X"))))
(define-key dired-sort-map "t" (lambda () "sort by Time" (interactive) (dired-sort-other (concat dired-listing-switches " -t"))))
(define-key dired-sort-map "n" (lambda () "sort by Name" (interactive) (dired-sort-other dired-listing-switches)))
(define-key dired-sort-map "r" 'dired-sm-toggle-reverse)
(define-key dired-sort-map "d" 'dired-sm-toggle-dir-group)
;; )))

(defun dired-sm--toggle (switch)
  (dired-sort-other (setq dired-actual-switches
                          (if (string-match switch dired-actual-switches)
                              (replace-match " " t t dired-actual-switches)
                            (concat dired-actual-switches " " switch)))))

(defun dired-sm-toggle-reverse ()
  (interactive)
  (dired-sm--toggle "-r"))

(defun dired-sm-toggle-dir-group ()
  (interactive)
  (dired-sm--toggle "--group-directories-first"))

(provide 'dired-sort-map)
;;; dired-sort-map.el ends here
