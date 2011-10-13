;;; kbd-fns.el --- keyboard input and keymap utility functions

;; Copyright (C) 1991-1999 Noah S. Friedman

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: friedman@splode.com

;; $Id: kbd-fns.el,v 1.2 1999/10/10 18:57:45 friedman Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:
;;; Code:

(require 'list-fns)

;;;###autoload
(defun keymap-define-keys (keymap bindings)
  "Bind keys to commands.
First arg is a keymap in which to make the bindings.
If not specified, default to the global map.
Bindings should be an alist of conses in which each car is a key sequence
and each cdr is the command to bind to that key sequence."
  (or (keymapp keymap)
      (setq keymap (current-global-map)))
  (while bindings
    (define-key keymap (car (car bindings)) (cdr (car bindings)))
    (setq bindings (cdr bindings))))

(put 'keymap-define-keys 'lisp-indent-function 1)

;;;###autoload
(defun keymap-undefine-keys (keymap &rest keys)
  (or (keymapp keymap)
      (setq keymap (current-global-map)))
  (and (listp (car keys))
       (not (consp (cdr keys)))
       (setq keys (car keys)))
  (let ((binding nil))
    (while keys
      (setq binding (lookup-key keymap (car keys)))
      ;; Do not attempt to unbind the key if it's not currently bound or
      ;; has an invalid prefix; define-key will signal an error.
      (cond ((or (null binding)
                 (numberp binding)))
            (t
             (define-key keymap (car keys) nil)))
      (setq keys (cdr keys)))))

;;;###autoload
(defun keymap-unbind-commands (&rest commands)
  (while commands
    (keymap-undefine-keys nil (where-is-internal (car commands)))
    (setq commands (cdr commands))))

;;;###autoload
(defun make-keyboard-translate-table (&optional size)
  "Create a table suitable as a keyboard translation table, and return it.
The initial contents is a direct mapping of index number to corresponding
character in whatever character set is being used.

Optional argument SIZE determines the size of the table.
By default the size is 128, which is large enough to handle all 7-bit ASCII
characters."
  (or size (setq size 128))
  (let ((tbl (make-string size 0))
        (i 1))
    (while (< i size)
      (aset tbl i i)
      (setq i (1+ i)))
    tbl))

;;;###autoload
(defun make-local-keyboard-translate-table (&optional buffer)
  "Make a buffer-local keyboard-translate-table.
If there is already a buffer-local keyboard-translate-table, just return it.
If there is no buffer-local table but a global one exists, copy it and
 make the copy buffer-local.
If no keyboard translation table exists, create a local initialized table.

The optional argument BUFFER specifies the buffer in which to make the
local table.  If none is specified, the current buffer is used.

This function returns the buffer-local keyboard-translate-table."
  (or buffer (setq buffer (current-buffer)))
  (save-excursion
    (set-buffer buffer)
    (cond
     ((and (assq 'keyboard-translate-table (buffer-local-variables))
           keyboard-translate-table))
     (keyboard-translate-table
      (make-local-variable 'keyboard-translate-table)
      (setq keyboard-translate-table
            (copy-sequence (default-value 'keyboard-translate-table))))
     (t
      (make-local-variable 'keyboard-translate-table)
      (setq keyboard-translate-table (make-keyboard-translate-table))))
    keyboard-translate-table))


;;;###autoload
(defun iso-acc-add-keys (lang pseudo-accent char iso-char &optional reset)
  "Add new modifier keys to ISO accent maps.
For the accent map named LANG \(a string\), and with prefix
PSEUDO-ACCENT \(a character\), map the character CHAR to the character
ISO-CHAR.

Alternatively, CHAR may be a list of pairs, corresponding to a char and
iso-char mapping to be made with the pseudo-accent prefix.  In this case,
the arg ISO-CHAR is ignored.

Optional 5th arg RESET non-nil means reinitialize the iso-acc package.
Without doing this, new key additions may not take effect right away.

For example, the expression \(iso-acc-add-keys \"latin-1\" ?^ ?2 ?\\262\)
causes the sequence \"^2\" typed in a buffer to generate the character
\"\262\" \(a superscript `2' in the iso-8859-1 or latin-1 character set\)."
  (let ((map (cdr (assq pseudo-accent (assoc lang iso-languages)))))
    (cond ((listp char)
           (let (pair)
             (while char
               (setq pair (car char))
               (setq char (cdr char))
               (set-alist-slot map (car pair) (cdr pair)))))
          (t
           (set-alist-slot map char iso-char))))
  (and reset
       (iso-accents-customize (or iso-language lang))))

;;;###autoload
(defun iso-acc-install-extra-keys ()
  "Install my extensions to the iso accents tables:
  latin-1:
    ^1 => \271     /1 => \274     /C => \251     /c => \242     /S => \247
    ^2 => \262     /2 => \275     /R => \256     /p => \243     /P => \266
    ^3 => \263     /3 => \276     /m => \265     /y => \245"
  (iso-acc-add-keys "latin-1" ?^
                    '((?1 . ?\271) (?2 . ?\262) (?3 . ?\263))
                    nil)
  (iso-acc-add-keys "latin-1" ?/
                    '((?1 . ?\274) (?2 . ?\275) (?3 . ?\276)
                      (?c . ?\242) (?p . ?\243) (?y . ?\245)
                      (?S . ?\247) (?P . ?\266)
                      (?C . ?\251) (?R . ?\256)
                      (?m . ?\265))
                    nil t))

(provide 'kbd-fns)

;;; kbd-fns.el ends here.
