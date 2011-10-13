;;; .emacs --- my Emacs Init File

;; Copyright (C) 1999-2010 Fabrice Niessen
;; Time-stamp: <2010-07-09 Fri 11:40 sva on mundaneum>

;; Author: Fabrice Niessen <(concat "fni" at-symbol "mygooglest.com")>
;; Keywords: emacs, dotfile, config

;; $Revision: 4145 $
;; $Date: 2010-07-08 15:13:00 +0200 (Thu, 08 Jul 2010) $

;;
;;    ___ _ __ ___   __ _  ___ ___
;;   / _ \ '_ ` _ \ / _` |/ __/ __|
;;  |  __/ | | | | | (_| | (__\__ \
;; (_)___|_| |_| |_|\__,_|\___|___/
;;

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this file; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;; Commentary:

;;  Citations
;;
;;     "Show me your ~/.emacs and I will tell you who you are."
;;                                                         [Bogdan Maryniuk]
;;
;;     "Emacs is like a laser guided missile. It only has to be slightly
;;      mis-configured to ruin your whole day."
;;                                                            [Sean McGrath]
;;
;;     "While any text editor can save your files, only Emacs can save your
;;      soul."
;;                                                          [Per Abrahamsen]
;;
;;  For Windows users
;;
;;    - HOME variable
;;
;;      Don't put a trailing backslash at the end of your `HOME'
;;      variable (if you have any). Otherwise, `~/emacs.d/server'
;;      gets (for example) translated to `Z://emacs.d/server' (double
;;      slash)!
;;
;;    - PATH variable
;;
;;      Put `C:\cygwin\bin' in the first place, in order to select the correct
;;      `find' command (for searches).
;;
;;  Help
;;
;;      For help on The Emacs Editor, see:
;;          (info "(emacs)") <== C-x C-e here!
;;
;;  News
;;
;;      Emacs 23 has a daemon mode for starting emacs in background (without
;;      opening any frames) and connect to it via emacsclient.

;; BUGS
;;
;; - `C-x SPC' = gud-break, unknown function
;; - menu-bar-mode disappears sometimes


;;; Emacs Tour
;;
;; http://www.gnu.org/software/emacs/tour/


;; XXX Go and see http://stackoverflow.com/questions/298065/which-are-the-gnu-emacs-modes-extensions-you-cant-live-without


;;; Code:

;; This file is only provided as an example. Customize it to your own taste!

;;* Prerequisites

(message "* --[ Loading my Emacs init file ]--")

;; uptimes
(setq emacs-load-start-time (current-time))

;; Identify what parts of your `.emacs' take so long. You can do
;; this e.g. by starting emacs with "emacs -q", set up your
;; load-path, and then evaluate
;;
;; (benchmark-run
;;   (require 'package))
;;
;; The first number appearing in the echo area will be the time needed to run
;; that command.
;;
;; Use autoloads, which delay the loading of the complete package until one of
;; the interactive functions is used.
;;
;; If you want to set options which need to be evaluated after a package is
;; loaded, you can use `eval-after-load'.

;; turn on Common Lisp support
(require 'cl)  ; provides useful things like `loop' and `setf'


;; ;; keep my secrets secret (and not in my `.emacs' file)
;; (let ((personal-settings "~/.hide/.emacs-secrets.gpg"))
;;   (when (file-exists-p personal-settings)
;;     (load personal-settings)))


;; allow quick include/exclude of setup parts
(defvar section-environment t)  ; required
(defvar section-loading-libraries t)  ; required
(defvar section-debugging t)
(defvar section-screen t)
(defvar section-basic t)
(defvar section-minibuffer t)
(defvar section-help t)
(defvar section-mark t)
(defvar section-killing t)
(defvar section-yanking t)
(defvar section-rectangles t)
(defvar section-cua-bindings t)
(defvar section-registers t)
(defvar section-display t)
(defvar section-search t)
(defvar section-fixit t)
(defvar section-keyboard-macros t)
(defvar section-files t)
(defvar section-buffers t)
(defvar section-windows t)
(defvar section-frames t)
(defvar section-international t)
(defvar section-major-modes t)
(defvar section-indentation t)
(defvar section-text t)
(defvar section-programs t)
(defvar section-building t)
(defvar section-maintaining t)
(defvar section-abbrevs t)
(defvar section-dired t)
(defvar section-calendar-diary t)
(defvar section-document-view t)
(defvar section-gnus t)
(defvar section-shell t)
(defvar section-emacs-server t)
(defvar section-printing t)
(defvar section-sorting t)
(defvar section-narrowing t)
(defvar section-saving-emacs-sessions t)
(defvar section-hyperlinking t)
(defvar section-amusements t)
(defvar section-pgg t)
(defvar section-customization t)
(defvar section-ms-dos t)
(defvar section-emacs-display t)


;;** Environment

(when section-environment (message "0 Environment...")

;; OS type --- are we running Microsoft Windows?
(defvar running-ms-windows
  (eq system-type 'windows-nt))

(defvar running-ms-windows
  (string-match "windows" (prin1-to-string system-type)))

(defvar running-gnu-linux
  (string-match "linux" (prin1-to-string system-type)))

;; Emacs type --- are we running XEmacs (or GNU Emacs)?
(defvar running-xemacs
  (string-match "XEmacs" emacs-version))

;; OS type --- are we running GNU Linux?
(defmacro GNULinux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
	(cons 'progn body)))

(defmacro Windows (&rest body)
  (list 'if (string-match "windows" (prin1-to-string system-type))
	(cons 'progn body)))

(defmacro XLaunch (&rest body)
  (list 'if (eq window-system 'x)(cons 'progn body)))

;; Emacs type --- are we running GNU Emacs?
(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
	(cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
	(cons 'progn body)))

(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
	(cons 'progn body)))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
	(cons 'progn body)))

;; Emacs version
(GNUEmacs
 (list emacs-version emacs-major-version emacs-minor-version
       system-type system-name system-configuration
       window-system
       (when (boundp 'aquamacs-version) aquamacs-version)))

(XEmacs
    ;; don't offer migration of the init file
    (setq load-home-init-file t))

(when running-gnu-linux
  (modify-all-frames-parameters
   '((height . 32))))

(message "0 Environment... Done"))


;;** Loading Libraries of Lisp Code for Emacs

(when section-loading-libraries (message "0 Loading Libraries...")

;; make loaded files give a message
(GNUEmacs
    (defadvice load (before debug-log activate)
      (message "Loading %s..." (locate-library (ad-get-arg 0)))))

;; load-path enhancement
(defun fni/add-to-load-path (this-directory &optional with-subdirs recursive)
  "Add THIS-DIRECTORY at the beginning of the load-path, if it exists.
Add all its subdirectories not starting with a '.' if the
optional argument WITH-SUBDIRS is not nil.
Do it recursively if the third argument is not nil."
  (when (and this-directory
             (file-directory-p this-directory))
    (let* ((this-directory (expand-file-name this-directory))
           (files (directory-files this-directory t "^[^\\.]")))

      ;; completely canonicalize the directory name (*may not* begin with `~')
      (while (not (string= this-directory (expand-file-name this-directory)))
        (setq this-directory (expand-file-name this-directory)))

      (message "Adding `%s' to load-path..." this-directory)
      (add-to-list 'load-path this-directory)

      (when with-subdirs
        (while files
          (setq dir-or-file (car files))
          (when (file-directory-p dir-or-file)
            (if recursive
                (fni/add-to-load-path dir-or-file 'with-subdirs 'recursive)
              (fni/add-to-load-path dir-or-file)))
          (setq files (cdr files)))))))

;; Use `M-x list-load-path-shadows RET' to display a list of external Emacs
;; Lisp files that shadow Emacs builtins (listing potential load path
;; problems).


;;*** Features

;; ;; REPLACES ORIGINAL in `C source code' (dumped)
;; ;; redefine require to leave a trace of packages being loaded
;; (if (not (fboundp 'orig-require))
;;     (fset 'orig-require (symbol-function 'require))
;;   (message "The code to redefine `require' should not be loaded twice"))
;;
;; (defvar my-require-depth 0)
;;
;; (defun require (feature &optional file)
;;   "Leave a trace of packages being loaded."
;;   (cond ((member feature features)
;;          (message "%sRequiring `%s' (already loaded)"
;;                   (concat (make-string (* 2 my-require-depth) ?-) "> ")
;;                   feature)
;;          (sit-for 0))
;;         (t
;;          (message "%sRequiring `%s'"
;;                   (concat (make-string (* 2 my-require-depth) ?-) "> ")
;;                   feature)
;;          (sit-for 0)
;;          (let ((my-require-depth (+ 1 my-require-depth)))
;;            (cond (file
;;                   (orig-require feature file))
;;                  (t
;;                   (orig-require feature))))
;;          (message "%sRequiring `%s'...done"
;;                   (concat (make-string (* 2 my-require-depth) ?-) "> ")
;;                   feature)
;;          (sit-for 0))))


(defvar missing-packages-list nil
  "List of packages that `try-require' can't find.")

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
      (progn
        (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
     (progn
       (message "Checking for library `%s'... Missing" feature)
       (add-to-list 'missing-packages-list feature 'append))
     nil)))


;;*** Library Search

;; `load-path' is a list of directories where Emacs Lisp libraries (`.el' and
;; `.elc' files) are installed.

;; `exec-path' is different: it is a list of directories where executable
;; programs are installed.
;;
;; Shouldn't be `exec-path' and `PATH' achieve the same goal under Emacs?
;;
;; No. `exec-path' is used by Emacs to search for programs it runs directly.
;; But `M-x grep' does not run `grep.exe' directly; it runs the shell passing
;; it a command that invokes `grep'. So it's the shell that needs to find
;; `grep.exe', and it uses PATH, of course, not `exec-path'.
;;
;; So the right thing to do when you install a new program, in order for Emacs
;; to find it, is *both* to update `exec-path' *and* update `PATH'. This is
;; because some Emacs features invoke programs directly, while others do that
;; through the shell or some other intermediary programs.

;; The most important directories are the last!

;; TODO Specify variables using `defcustom'

;; 1.
(defvar distro-site-lisp-directory
  (concat (or (getenv "SHARE")
              "/usr/share") "/emacs/site-lisp/")
  "Name of directory where additional Emacs goodies Lisp files (from the
distro optional packages) reside.")

(fni/add-to-load-path distro-site-lisp-directory
                     'with-subdirs)

;; If you put stuff you have installed from tar balls, etc. within the same
;; directory hierarchy as the distro packaged Emacs, you can get problems when
;; upgrading the distro version as many package systems will assume once all
;; the packaged stuff is removed, directories are empty. If they are not, the
;; package management scripts can fail or possibly get into a "confused"
;; state.

;; 2.
(defvar local-site-lisp-directory
  (concat (or (getenv "LOCAL_SHARE")
              "~/Downloads") "/emacs/site-lisp/")
  "Name of directory where additional Emacs goodies Lisp files (from the
Internet) reside.")

(fni/add-to-load-path local-site-lisp-directory
                     'with-subdirs 'recursive)

;; `local-site-lisp-directory' is there so that you have an easy way of
;; installing your own (possibly not distro packaged) Emacs add-ons which are
;; specific to the version of Emacs your running. This keeps your local
;; add-ons apart from distro supplied ones. If your have a `/usr/local'
;; partition, it also means you can do a complete re-install of Emacs (or even
;; your Linux distro) without impacting on stuff you have added by hand.

;; 3.
(defvar my-site-lisp-directory "~/emacs/site-lisp/"
  "Name of directory where my personal additional Emacs Lisp files reside.")

(fni/add-to-load-path my-site-lisp-directory
                     'with-subdirs)

;; 4.
;; automatically compile `.el' files as they're loaded
(setq load-source-file-function 'load-with-code-conversion)  ; for XEmacs
(when (try-require 'byte-code-cache-XXX)

    (require 'bytecomp)

    ;; directory in which we store cached byte-compiled files
    (setq bcc-cache-directory
          ;; FIXME Concat env var (so that it can be stored locally on C:)
          (cond
           (running-gnu-linux "~/.emacs.d/byte-cache-linux")
           (running-ms-windows "~/.emacs.d/byte-cache-ms-windows")))

    (fni/add-to-load-path bcc-cache-directory))


;; load elisp libraries while Emacs is idle
(if (try-require 'idle-require-XXX)
    (progn
      ;; idle time (in seconds) after which autoload functions will be loaded
      (setq idle-require-idle-delay 5)

      ;; time in seconds between automatically loaded functions
      (setq idle-require-load-break 3)

      ;; load unloaded autoload functions when Emacs becomes idle
      (idle-require-mode 1)

      (defun try-idle-require (feature)
        (when (locate-library (symbol-name feature))
          (idle-require feature))))

  (defun try-idle-require (feature)
    (when (locate-library (symbol-name feature))
      (require feature))))


(defun my-make-directory-yes-or-no (dir)
  "Ask user to create the DIR, if it does not already exist."
  (if dir
      (if (not (file-directory-p dir))
          (if (yes-or-no-p (concat "The directory `" dir
                                   "' does not exist currently. Create it? "))
              (make-directory dir t)
            (error
             (concat "Cannot continue without directory `" dir "'"))))
    (error "my-make-directory-yes-or-no: missing operand")))

(defun my-file-executable-p (file)
  "Make sure the file FILE exists and is executable."
  (if file
      (if (file-executable-p file)
          file
        (message "WARNING: Can't find executable `%s'" file)
        ;; sleep 1 s so that you can read the warning
        (sit-for 1))
    (error "my-file-executable-p: missing operand")))

(message "0 Loading Libraries... Done"))


;;** Debugging

(when section-debugging (message "0 Debugging...")

;; ;; 2008-05-23 set it to nil to temporarily avoid the broken vc functions
;; ;; get the backtrace when uncaught errors occur
;; (setq debug-on-error t)  ; will be unset at the end

(XEmacs
    (setq stack-trace-on-error t))

;; ;; hit `C-g' while it's frozen to get an ELisp backtrace
;; (setq debug-on-quit t)

(message "0 Debugging... Done"))


;;* Important General Concepts

;;** 1 The Organization of the (info "(emacs)Screen")

(when section-screen (message "1 The Organization of the Screen...")

;;*** 1.2 The (info "(emacs)Echo Area")

;; don't truncate the message log buffer when it becomes large
(setq message-log-max t)

;; The `*Messages*' buffer is called the ` *Message-Log*' on XEmacs (note the
;; initial space). To display the message log, execute the command
;; `M-x show-message-log'

(message "1 The Organization of the Screen... Done"))


;;* Fundamental Editing Commands

;;** 7 (info "(emacs)Basic") Editing Commands

(when section-basic (message "7 Basic Editing Commands...")

;;*** 7.1 (info "(emacs)Inserting Text")

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)


;;*** 7.2 (info "(emacs)Moving Point") Location

;; don't add newlines to end of buffer when scrolling
(setq next-line-add-newlines nil)

;; XEmacs default for moving point to a given line number
(GNUEmacs
    (global-set-key (kbd "M-g") 'goto-line))

(global-set-key (kbd "M-G") 'what-line)

;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing `M-up' (or `M-down')
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col))))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

;; ;; XXX `M-up' and `M-down' are bound multiple times (to different things)!
;; (global-set-key (kbd "<M-up>") 'move-line-up)
;; (global-set-key (kbd "<M-down>") 'move-line-down)


;;*** 7.4 (info "(emacs)Basic Undo")ing Changes

;; undo some previous changes
(global-set-key (kbd "<f11>") 'undo)

;; redo the most recent undo
(when (try-require 'redo)
    (global-set-key (kbd "<S-f11>") 'redo))


;;*** 7.8 (info "(emacs)Continuation Lines")

(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)))


;;*** 7.11 (info "(emacs)Repeating") a Command

;; repeat last command passed to `shell-command'
(defun repeat-shell-command ()
  "Repeat most recently executed shell command."
  (interactive)
  (save-buffer)
  (or shell-command-history (error "Nothing to repeat."))
  (shell-command (car shell-command-history)))

(global-set-key (kbd "C-c j") 'repeat-shell-command)

(message "7 Basic Editing Commands... Done"))


;;** 8 The (info "(emacs)Minibuffer")

(when section-minibuffer (message "8 The Minibuffer...")

;;*** 8.1 (info "(emacs)Minibuffer File") Names

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;; dim the ignored part of the file name
(GNUEmacs
    (file-name-shadow-mode 1))


;;*** 8.2 (info "(emacs)Minibuffer Edit")ing

;; minibuffer window expands vertically as necessary to hold the text that you
;; put in the minibuffer
(setq resize-mini-windows t)

;; From Babel.el: "If the output is short enough to display in the echo area
;; (which is determined by the variables `resize-mini-windows' and
;; `max-mini-window-height'), it is shown in echo area."


;;*** 8.3 (info "(emacs)Completion")

;; ;; allow to type space chars in minibuffer input
;; ;; (for `timeclock-in', for example)
;; (define-key minibuffer-local-completion-map " " nil)
;; (define-key minibuffer-local-must-match-map " " nil)

;; minibuffer completion incremental feedback
(GNUEmacs
    (icomplete-mode))

;; ignore case when reading a buffer name
(setq read-buffer-completion-ignore-case t)

;; do not consider case significant in completion (GNU Emacs default)
(setq completion-ignore-case t)

;;> I frequently find myself doing `C-M :' for some quick elisp calls, and
;;> find it tedious to type expressions.  Is there some way to allow
;;> completion when writing a sexp in the minibuffer?  I'd like this to work
;;> similar to the way `M-x' helps you complete some command.
;;;;;;(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

(message "8 The Minibuffer... Done"))


;;** 10 (info "(emacs)Help")

(when section-help (message "10 Help...")

;; `catman' create the database files that are used by `apropos' or `man -k'.

;; You can read the instructions of a file by typing
;; `M-x finder-commentary RET file RET'

;; You might want to check out electric-help (ehelp.el). There is nothing more
;; satisfying than the window disappearing when you think it should!


;;*** 10.4 (info "(emacs)Apropos")

;; You can ask what pertains to a given topic by typing
;; `M-x apropos RET pattern RET'

;; check all variables and non-interactive functions as well
(setq apropos-do-all t)

;; add apropos help about variables (bind `C-h A' to `apropos-variable')
(GNUEmacs
    (define-key help-map (kbd "A") 'apropos-variable))


;;*** 10.8 (info "(emacs)Misc Help") Commands

;; ls -l /usr/share/info/dir*
;; update-info-dir

;; Info documentation browse
(when (try-require 'info)

    ;; enter Info
    (global-set-key (kbd "<f1>") 'info)

    ;; list of directories to search for Info documentation files
    ;; (in the order they are listed)
    (setq Info-directory-list
          (cond (running-ms-windows
                 (append Info-default-directory-list
                         `(,(concat (getenv "SHARE") "/info/")
                           "C:/cygwin/usr/info/")))
                (t
                 '("./"
                   "~/info/"
                   "/usr/local/share/info/"
                   "/usr/local/info/"
                   "/usr/share/info/emacs-snapshot/"
                   "/usr/share/info/emacs-23"
                   "/usr/share/info/"))))

    ;; I find this handy:
    ;; (setq Info-directory-list
    ;;       `("~/.emacs.d/info" ,@Info-directory-list))


;;;     ;; adding doc for Org mode
;;;     (setq Info-default-directory-list
;;;           (cons (expand-file-name
;;;                  (concat org-general-path "org/doc"))
;;;                 Info-default-directory-list))

    ;; adding TexLive?

    ;; FIXME
    (setq Info-directory-list
          (append
           '("~/Downloads/emacs/site-lisp/auctex-11.85/doc"
             "~/Downloads/emacs/site-lisp/emacs-w3m/doc"
             "~/Downloads/emacs/site-lisp/org-mode/doc")
           Info-directory-list))

;;;     dir files will be merged by emacs, so you can create
;;;     one for your local additions. Probably by copying the
;;;     systems one from /usr/share/info/dir or wherever, delete
;;;     all entries after '* Menu:' and put in your own.
;;;
;;;     (info "(texinfo)Other Info Directories")
;;;
;;;     The dir file has to have a special header format. Just copy the
;;;     systems one into a directory in INFOPATH, erase everything after
;;;     '*Menu ' and put in your own stuff.
;;;
;;;     Wouldn't the use of install-info be more appropriate? When the
;;;     actual info file is well prepared, then this programme does a
;;;     perfect job, sorting the info file into its proper section ...

;; The canonical way to do that is to set the environment variable `INFOPATH'
;; outside of Emacs, in the same shell from which you invoke Emacs.
;; `INFOPATH's value should be a list of Info directories in the same format
;; as the `PATH' variable on your system.

;; The environment variable INFOPATH tells GNU Emacs where to look for info
;; files.
;;
;; If you want, you can edit the dir files and remove entries. The utility
;; install-info is used to maintain the dir file.

;; See http://www.emacswiki.org/emacs-se/GnusTutorial, "Installing Gnus":
    ;; In order to get the Gnus info pages added to your documentation, also
    ;; augment the INFOPATH environment variable, like
    ;;   INFOPATH=$INFOPATH:/usr/local/share/emacs/site-lisp/gnus-5.8.8/texi
    ;;   export INFOPATH
    ;; The above should go in your shell startup file, such as ~/.profile.

;;; > Is there an automated procedure to transfer the additional info files
;;; > and info/dir entries into the new emacs?
;;;
;;; Copy the files, then run install-info on them to update the info/dir file.

;; Don't play with `Info-directory-list', it's not intended to be settable by
;; the user

    ;; display symbol definitions, as found in the relevant manual
    ;; (for C, Lisp, and other languages that have documentation in Info)
    (global-set-key (kbd "<C-f1>") 'info-lookup-symbol))

(GNUEmacs
    (try-require 'info+))
    ;; with `info+.el', you can merge an Info node with its subnodes into
    ;; the same buffer, by calling `Info-merge-subnodes' (bound to `+')

    ;; `C-h K' goes to the node in the Emacs manual describing the command
    ;; bound to a key.


;; dictem (dict protocol and dictem for a documentation)

;; describe-function

;; avoid the description of all minor modes
(defun describe-major-mode ()
  "Describe only `major-mode'."
  (interactive)
  (describe-function major-mode))


;; find convenient unbound keystrokes
(try-require 'unbound)                  ; `M-x describe-unbound-keys'


;; get a Unix manual page and put it in a buffer
(global-set-key (kbd "<S-f1>") 'man-follow)

;; (defun jump-man-page ()
;;   (interactive)
;;   (manual-entry (current-word)))

;; same behavior as woman when manpage is ready
(setq Man-notify-method 'newframe)

;; (setq Man-frame-parameters '((foreground-color . "black")
;;                              (background-color . "grey90")
;;                              (cursor-color . "black")
;;                              (mouse-color . "gold")
;;                              (width . 80)
;;                              (tool-bar-lines . 0)))

;; browse Unix manual pages "W.o. (without) Man"
(when (try-require 'woman)

    ;; list of directory trees to search for Unix manual files
    (setq woman-manpath
          (cond (running-ms-windows
                 `(,(concat (getenv "SHARE") "/man/")
                   "C:/cygwin/usr/man/"
                   "C:/cygwin/usr/share/man"
                   "C:/cygwin/usr/local/man"))
                (t
                 '("/usr/share/man/"
                   "/usr/local/man/")))))


;;*** Documentation Basics

;; jump to section in XEmacs Lisp Reference manual
(autoload 'lispref-search "lispref")
(define-key help-map (kbd "L") 'lispref-search)

(message "10 Help... Done"))


;;* Important Text-Changing Commands

;;** 11 The (info "(emacs)Mark") and the Region

(when section-mark (message "11 The Mark and the Region...")

;;*** 11.7 (info "(emacs)Persistent Mark")s

;; when the mark is active, the *region is highlighted*
;; (enabled by default in Emacs 23)
(GNUEmacs
    (when window-system
      (transient-mark-mode 1)))

(message "11 The Mark and the Region... Done"))


;;** 12 (info "(emacs)Killing") and Moving Text

(when section-killing (message "12 Killing and Moving Text...")

;;*** 12.3 (info "(emacs)Other Kill Commands")

;; Change cutting behavior:
;; "Many times you'll do a kill-line command with the only intention of
;; getting the contents of the line into the killring. Here's an idea stolen
;; from Slickedit, if you press copy or cut when no region is active, you'll
;; copy or cut the current line."
;; <http://www.zafar.se/bkz/Articles/EmacsTips>
(defadvice kill-ring-save (before slickcopy activate compile)
  "When called interactively with no active region, copy the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slickcut activate compile)
  "When called interactively with no active region, kill the
current line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(message "12 Killing and Moving Text... Done"))


;;** 13 (info "(emacs)Yanking")

(when section-yanking (message "13 Yanking...")

;;*** 13.1 The (info "(emacs)Kill Ring")

;; auto-indent pasted code
(defadvice yank (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))

(defadvice yank-pop (after indent-region activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode c-mode c++-mode
                objc-mode latex-mode plain-tex-mode python-mode))
      (indent-region (region-beginning) (region-end) nil)))


;;*** 13.3 Yanking (info "(emacs)Earlier Kills")

;; interactively insert items from kill ring
(when (try-require 'browse-kill-ring)

    ;; string separating entries in the `separated' style
    (setq browse-kill-ring-separator
          "\n--separator------------------------------")

    ;; temporarily highlight the inserted `kill-ring' entry
    (setq browse-kill-ring-highlight-inserted-item t)

    ;; face in which to highlight the `browse-kill-ring-separator'
    (defface separator-face '((t (:foreground "Blueviolet" :weight bold))) nil)
                                        ; slate gray
    (setq browse-kill-ring-separator-face 'separator-face)

    ;; use `M-y' to invoke `browse-kill-ring'
    (browse-kill-ring-default-keybindings))

(message "13 Yanking... Done"))


;;** 15 (info "(emacs)Rectangles")

(when section-rectangles (message "15 Rectangles...")

;; `kill-rectangle' (C-x r k) and `yank-rectangle' (C-x r y) can be very
;; useful for shifting cells up/down within a column while leaving remaining
;; columns intact.

(message "15 Rectangles... Done"))


;;** 16 (info "(emacs)CUA Bindings")

(when section-cua-bindings (message "16 CUA Bindings...")

;; CUA mode sets up key bindings used in many other applications (`C-x',
;; `C-c', `C-v' and `C-z').
;;
;; The `C-x' and `C-c' keys only do cut and copy when the region is active, so
;; in most cases, they do not conflict with the normal function of these
;; prefix keys.
;;
;; If you really need to perform a command which starts with one of the prefix
;; keys even when the region is active, you have three options:
;;
;; - press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds, or
;; - use the SHIFT key with the prefix key, i.e. `C-S-x' or `C-S-c'.
;;
;; You can customize `cua-enable-cua-keys' to completely disable the CUA
;; bindings, or `cua-prefix-override-inhibit-delay' to change the prefix
;; fallback behavior.

;; CUA mode also provides enhanced rectangle support with visible rectangle
;; highlighting. Check out "Emacs Column Editing" at
;; http://www.vimeo.com/1168225?pg=embed&sec=1168225.
;;
;; `C-RET' runs the command `cua-set-rectangle-mark'
;; `M-n' runs the command `cua-sequence-rectangle'

;; ;; activate CUA mode
;; (cua-mode t)

;; standard Windows behavior
(setq cua-keep-region-after-copy t)

;; fix funny things of cursor moving commands
(add-hook 'cua-mode-hook
          (lambda ()
            (dolist (cmd '(forward-char
                           backward-char
                           previous-line
                           next-line
                           forward-paragraph
                           backward-paragraph
                           beginning-of-buffer
                           end-of-buffer))
              (put cmd 'CUA nil))))

(message "16 CUA Bindings... Done"))


;;** 17 (info "(emacs)Registers")

(when section-registers (message "17 Registers...")



;; ;; Enable position saving through shortcuts.
;; ;; Save current position with  Ctrl-F1 Ctrl-F2 Ctrl-F3 and Ctrl-F4
;; (global-set-key [C-f1] '(lambda () (interactive) (point-to-register ?1)))
;; (global-set-key [C-f2] '(lambda () (interactive) (point-to-register ?2)))
;; (global-set-key [C-f3] '(lambda () (interactive) (point-to-register ?3)))
;; (global-set-key [C-f4] '(lambda () (interactive) (point-to-register ?4)))

;; (defun jump-to-register-other (reg)
;; (other-window 1)
;; (jump-to-register reg)
;; (hilit-recenter (/ (window-height) 2)))

;; (defun jump-to-register-here (reg)
;; (jump-to-register reg)
;; (hilit-recenter (/ (window-height) 2)))

;; ;; Move to saved position with F1 F2 F3 and F4
;; (global-set-key [f1] '(lambda () (interactive) (jump-to-register-here ?1)))
;; (global-set-key [f2] '(lambda () (interactive) (jump-to-register-here ?2)))
;; (global-set-key [f3] '(lambda () (interactive) (jump-to-register-here ?3)))
;; (global-set-key [f4] '(lambda () (interactive) (jump-to-register-here ?4)))




;;*** 17.7 (info "(emacs)Bookmarks")

;; Bookmarks are persistent and they have names; not markers. Bookmarked
;; positions can also be relocated (found) if they move slightly because of
;; text changes.

;; To navigate to a bookmark (linking to a file or directory), just press
;; `C-x r b'. You'll be prompted for the bookmark name, and it will open that
;; file or directory.

;; where to save the bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks.txt")

;; each command that sets a bookmark will also save your bookmarks
(setq bookmark-save-flag 1)



;; I have just added a key "O" to filter only these two bookmarks in
;; bookmark-extensions.el
;; http://mercurial.intuxication.org/hg/emacs-bookmark-extension/
;; So when you hit "O" from the bookmark list (C-x r l) you have that:
;; ,----
;; | % Bookmark Last Org Stored
;; | - ------------------------
;; |   org-refile-last-stored
;; |   org-remember-last-stored
;; `----



;; visible bookmarks in buffer
(GNUEmacs
    ;; repository should be restored when loading `bm'
    (setq bm-restore-repository-on-load t)

    (when (try-require 'bm)
        ;; key binding
        (global-set-key (kbd "<M-f2>") 'bm-toggle)

        ;; buffer should be recentered around the bookmark
        (setq bm-recenter t)

        ;; make bookmarks persistent as default
        (setq-default bm-buffer-persistence t)

        ;; loading the repository from file when on start up
        (add-hook' after-init-hook 'bm-repository-load)

        ;; restoring bookmarks when on file find
        (add-hook 'find-file-hooks 'bm-buffer-restore)

        ;; saving bookmark data on killing a buffer
        (add-hook 'kill-buffer-hook 'bm-buffer-save)

        ;; saving the repository to file when on exit
        ;; `kill-buffer-hook' is not called when emacs is killed, so we
        ;; must save all bookmarks first
        (add-hook 'kill-emacs-hook '(lambda nil
                                      (bm-buffer-save-all)
                                      (bm-repository-save)))

        ;; update bookmark repository when saving the file
        (add-hook 'after-save-hook 'bm-buffer-save))

    ;; lists all bookmarks in all buffers
    (try-require 'bm-ext))

(message "17 Registers... Done"))


;;** 18 Controlling the (info "(emacs)Display")

(when section-display (message "18 Controlling the Display...")

;;*** 18.1 (info "(emacs)Scrolling")

;; scroll line by line
(setq scroll-step 1)

;; better scrolling in Emacs (doing a `Pg Up' followed by a `Pg Dn' will
;; place the point at the same place)
(when (try-require 'pager)
    (global-set-key (kbd "<prior>") 'pager-page-up)
    (global-set-key (kbd "<next>") 'pager-page-down)
    (global-set-key (kbd "<M-up>") 'pager-row-up)
    (global-set-key (kbd "<M-down>") 'pager-row-down))


;;*** 18.8 (info "(emacs)Font Lock")

;; make buffer size irrelevant for fontification
(setq font-lock-maximum-size nil)

(XEmacs
    ;; stop showing that annoying progress bar when fontifying
    (setq progress-feedback-use-echo-area nil)

    ;; enable Font Lock mode
    (font-lock-mode))

;; highlight non-breaking spaces
(GNUEmacs
    (require 'disp-table)
    (aset standard-display-table
          (make-char 'latin-iso8859-1 (- ?\240 128))
          (vector (+ ?\267 (* 524288 (face-id 'nobreak-space))))))

;; special words
;; XXX add `fatal', and `Undefined'
(setq keywords-critical-pattern
      "\\(BUGS\\|FIXME\\|TODO\\|todo\\|XXX\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
(make-face 'keywords-critical)
(GNUEmacs (set-face-attribute 'keywords-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-org-critical-pattern
      "\\(BUGS\\|FIXME\\|XXX\\|[^*] TODO\\|[Ee][Rr][Rr][Oo][Rr]\\|[Mm][Ii][Ss][Ss][Ii][Nn][Gg]\\|[Ii][Nn][Vv][Aa][Ll][Ii][Dd]\\|[Ff][Aa][Ii][Ll][Ee][Dd]\\|[Cc][Oo][Rr][Rr][Uu][Pp][Tt][Ee][Dd]\\)")
                                        ; smaller subset of keywords for
                                        ; ensuring no conflict with Org mode
                                        ; TODO keywords

;; FIXME Highlighting all special keywords but "TODO" in Org mode is already a
;; good step. Though, a nicer integration would be that "TODO" strings in the
;; headings are not touched by this code, and that only "TODO" strings in the
;; text body would be. Don't know (yet) how to do that...
(make-face 'keywords-org-critical)
(GNUEmacs (set-face-attribute 'keywords-org-critical nil
                              :foreground "red" :background "yellow"
                              :weight 'bold))

(setq keywords-normal-pattern "\\([Ww][Aa][Rr][Nn][Ii][Nn][Gg]\\)")
(make-face 'keywords-normal)
(GNUEmacs (set-face-attribute 'keywords-normal nil
                              :foreground "magenta2" :background "yellow"))

;; set up highlighting of special words for proper selected major modes only
(dolist (mode '(fundamental-mode
                svn-log-view-mode
                text-mode))  ; no interference with Org mode (which derives
                             ; from text-mode)
  (font-lock-add-keywords mode
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; set up highlighting of special words for Org mode only
(dolist (mode '(org-mode))
  (font-lock-add-keywords mode
    `((,keywords-org-critical-pattern 1 'keywords-org-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))

;; add fontification patterns (even in comments) to a selected major mode
;; *and* all major modes derived from it
(defun fontify-keywords ()
  (interactive)
;;;   (font-lock-mode -1)
;;;   (font-lock-mode 1)
  (font-lock-add-keywords nil
    `((,keywords-critical-pattern 1 'keywords-critical prepend)
      (,keywords-normal-pattern 1 'keywords-normal prepend))))
;; FIXME                        0                  t

;; set up highlighting of special words for selected major modes *and* all
;; major modes derived from them
(dolist (hook '(c++-mode-hook
                c-mode-hook
                change-log-mode-hook
                cperl-mode-hook
                css-mode-hook
                emacs-lisp-mode-hook
                html-mode-hook
                java-mode-hook
                latex-mode-hook
                lisp-mode-hook
                makefile-mode-hook
                message-mode-hook
                php-mode-hook
                python-mode-hook
                sh-mode-hook
                shell-mode-hook
                ssh-config-mode-hook))
  (add-hook hook 'fontify-keywords))


;;*** 18.9 (info "(emacs)Highlight Interactively") by Matching

;; You can use `hi-lock-mode' to highlight words:
;;     `M-x hi-lock-mode RET'
;;     `C-x w h <match> RET hi-blue RET'
;; You can also write your settings to the buffer you're using with
;; `C-x w b', and read them back in again next time with `C-x w i'.


;; TODO Have a look at http://www.emacswiki.org/emacs/color-moccur.el for
;; searching regexp in buffer


(GNUEmacs
    ;; "identical token highlighting" commands
    (when (try-require 'highlight)

        (defface hlt-1 '((t (:background "#FFFFA0"))) nil)
        (defface hlt-2 '((t (:background "#A0FFA0"))) nil)
        (defface hlt-3 '((t (:background "#A0FFFF"))) nil)
        (defface hlt-4 '((t (:background "#FFA0FF"))) nil)
        (defface hlt-5 '((t (:background "#FFA0A0"))) nil)
        (defface hlt-6 '((t (:background "#FFFFA0"))) nil)
        (defface hlt-7 '((t (:background "#A0FFA0"))) nil)
        (defface hlt-8 '((t (:background "#A0FFFF"))) nil)
        (defface hlt-9 '((t (:background "#FFA0FF"))) nil)
        (defface hlt-10 '((t (:background "#FFA0A0"))) nil)

        (global-set-key (kbd "C-S-p") 'hlt-previous-highlight)
        (global-set-key (kbd "C-S-n") 'hlt-next-highlight)

        (defun hlt-highlight-current-word ()
          (interactive)
          (let ((var_name (current-word t)))
            (when var_name
              (save-excursion
                (hlt-highlight-regexp-region
                 (point-min)
                 (point-max)
                 (regexp-quote var_name))))))

        ;; emulation of Vim's `*' search
        (global-set-key (kbd "C-*") 'hlt-highlight-current-word)
        ))

;; ;; bind the hi-lock commands to more finger-friendly sequences
;; (define-key hi-lock-map (kbd "C-z i") 'hi-lock-find-patterns)
;; (define-key hi-lock-map (kbd "C-z p") 'highlight-phrase)
;; (define-key hi-lock-map (kbd "C-z r") 'unhighlight-regexp)

;; (define-key hi-lock-map (kbd "C-z h") 'highlight-regexp)
;; (define-key hi-lock-map (kbd "C-z C-h") 'highlight-lines-matching-regexp)
;; (define-key hi-lock-map (kbd "C-z b") 'hi-lock-write-interactive-patterns)

;; ;; Highlight based on regexps
;; (global-set-key [M-f1] 'highlight-regexp)
;; (global-set-key [M-f2] 'highlight-lines-matching-regexp)
;; (global-set-key [M-f3] 'hi-lock-mode)
;; (global-set-key [M-f4] 'hi-lock-write-interactive-patterns)


;; ;; highlight current symbol
;; (when (try-require 'light-symbol)
;;   (light-symbol-mode))


;; highlight current symbol
(setq highlight-symbol-idle-delay 0.5)
(when (try-require 'highlight-symbol)
  (highlight-symbol-mode))





;;*** 18.11 (info "(emacs)Displaying Boundaries")

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)


;;*** 18.12 (info "(emacs)Useless Whitespace")

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)

;; ;; FIXME When turned on, Gnus becomes black and white only...
;; (when (try-require 'show-wspace)
;;   ;; Highlight tabs
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
;;
;;   ;; Highlight trailing whitespace
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)
;;
;;   ;; Highlight non-breaking spaces
;;   (add-hook 'font-lock-mode-hook 'show-ws-highlight-hard-spaces))


;; FIXME Same problem as above
;; ;; highlight tabs in all modes
;; (add-hook 'font-lock-mode-hook
;;           '(lambda ()
;;              (font-lock-add-keywords
;;               nil
;;               '(("\t" 0 'trailing-whitespace prepend)))))

;; delete all the trailing whitespaces and tabs across the current buffer
(defun my-delete-trailing-whitespaces-and-untabify ()
  "Delete all the trailing white spaces, and convert all tabs to multiple
spaces across the current buffer."
  (interactive "*")
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max)))

;; visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)


;;*** 18.14 (info "(emacs)Optional Mode Line") Features

;; show the line number in each mode line
(line-number-mode 1)

;; show the column number in each mode line
(column-number-mode 1)

;; use inactive face for mode-line in non-selected windows
(setq mode-line-in-non-selected-windows t)

(GNUEmacs
    ;; code for including abbreviated file paths in mode line
    (when (try-require 'mode-line)
        (mode-line-toggle-display nil)))


;;*** 18.15 How (info "(emacs)Text Display")ed

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

;; Bad interaction with CVSNT/diff/... (not yet understood)
;; ;; Remove or convert trailing ^M
;; (defun remove-trailing-ctrl-M ()
;;   "Propose to remove trailing ^M from a file."
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-min))
;;     (if (and (not (string-match ".gz$" (buffer-file-name)))
;;              (search-forward-regexp "\015$" nil t))
;;                                         ;: a ^M is found
;;         (if (or (= (preceding-char) ?\^J)
;;                 (= (following-char) ?\^J) )
;;             (if (y-or-n-p (format "Remove trailing ^M from %s? "
;;                                   (buffer-file-name)))
;;                 (progn (goto-char (point-min))
;;                        (perform-replace "\015" "" nil nil nil)
;;                        (pop-mark)
;;                        (save-buffer))
;;               (message "No transformation."))))))
;; (add-hook 'find-file-hooks 'remove-trailing-ctrl-M)


;;*** 18.16 The (info "(emacs)Cursor Display")

(GNUEmacs
    ;; using cursor color to indicate some modes (read-only, insert and
    ;; overwrite modes)
    (setq my-set-cursor-color-color "")
    (setq my-set-cursor-color-buffer "")

    (defun my-set-cursor-color-according-to-mode ()
      "Change cursor color according to some minor modes."
      (let ((color
             (if buffer-read-only "purple1"
               (if overwrite-mode "red"
                 "#15FF00"))))  ; insert mode
        (unless (and (string= color my-set-cursor-color-color)
                     (string= (buffer-name) my-set-cursor-color-buffer))
          (set-cursor-color (setq my-set-cursor-color-color color))
          (setq my-set-cursor-color-buffer (buffer-name)))))

    (add-hook 'post-command-hook 'my-set-cursor-color-according-to-mode))

;; highlight columns 78 to 80 in some modes
(when (try-require 'column-marker)

    (dolist (hook '(emacs-lisp-mode-hook
                    cperl-mode-hook
                    shell-mode-hook
                    text-mode-hook
                    change-log-mode-hook
                    makefile-mode-hook
                    message-mode-hook
                    texinfo-mode-hook))
      (add-hook hook (lambda ()
                       (interactive)
                       (column-marker-1 78)
                       (column-marker-2 79)
                       (column-marker-3 80))))

    ;; use `C-c m' interactively to highlight with `column-marker-1-face'
    (global-set-key (kbd "C-c m") 'column-marker-1))


;;*** 18.17 (info "(emacs)Line Truncation")

;; respect the value of `truncate-lines' in all windows less than the full
;; width of the frame
(setq truncate-partial-width-windows nil)


;;*** 18.19 (info "(emacs)Display Custom")ization

;; see what I'm typing *immediately*
(setq echo-keystrokes 0.01)


;;*** Temporary Displays

;; make the help, apropos and completion windows the right height for their
;; contents
(GNUEmacs
    (temp-buffer-resize-mode t))  ; auto-fit the *Help* buffer to its contents

;; enhanced display of temporary windows (such as help buffers)
;; (try-require 'show-temp-buffer)  [bug with XEmacs 21.5]

(message "18 Controlling the Display... Done"))


;;** 19 (info "(emacs)Search")ing and Replacement

(when section-search (message "19 Searching and Replacement...")

;;*** 19.1 (info "(emacs)Incremental Search")

;; Have a look at the "Standard Isearch Keys" on
;; http://www.emacswiki.org/emacs/IncrementalSearch

;; always exit searches at the beginning of the expression found
(add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)

(defun custom-goto-match-beginning ()
  "Use with isearch hook to end search at first char of match."
  (when isearch-forward (goto-char isearch-other-end)))


;;*** 19.4 (info "(emacs)Regexp Search")

;; You can build regexps with visual feedback by using:
;; - `M-x re-builder' or
;; - `M-x regex-tool' (by John Wiegley, get it from
;;   http://www.newartisans.com/downloads_files/regex-tool.el)

;; Optimize regexps with `regexp-opt.el'

;; list the input line, followed by the first nine substrings matches,
;; to debug regexp searches (in IELM)
;; example: ELISP> (save-excursion (set-buffer "BUFFER")
;;                                 (re-search-forward "REGEXP" nil t)
;;                                 (my-buffer-matched-strings))
(defun my-buffer-matched-strings ()
  (interactive)
  (mapcar 'my-buffer-matched-string-nth '(0 1 2 3 4 5 6 7 8 9)))

(defun my-buffer-matched-string-nth (n)
  "Return the Nth pattern-matched string from the current buffer."
  (if (and (match-beginning n) (match-end n))
      (if (> (match-end n) (match-beginning n))
          (buffer-substring (match-beginning n) (match-end n))
        "")
    nil))


;;*** 19.8 (info "(emacs)Search Case")

;; searches and matches should ignore case
(setq-default case-fold-search t)
(setq default-case-fold-search t)       ; FIXME obsolete since Emacs 23.2, but
                                        ; still needed!?


;;*** 19.9 (info "(emacs)Replace")ment Commands

;; You can force a matched regex text pattern to upper case by entering
;; `C-M-% your_regexp RET \,(upcase \num_of_match)'


;;*** 19.10 (info "(emacs)Other Repeating Search") Commands

(defun isearch-occur ()
  "Invoke `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

;; `M-x flush-lines' deletes each line that contains a match for REGEXP

(message "19 Searching and Replacement... Done"))


;;** 20 Commands for (info "(emacs)Fixit") Typos

(when section-fixit (message "20 Commands for Fixing Typos...")

;;*** 20.1 (info "(emacs)Undo")

;; (GNUEmacs
;;     ;; keep no more undo information once it exceeds this size
;;     (setq undo-limit (* 4 undo-limit))  ; 4 * 20 MB (default)
;;
;;     ;; don't keep more than this much size of undo information
;;     (setq undo-strong-limit (* 4 undo-strong-limit)))  ; 4 * 30 MB (default)


;;*** 20.4 Checking and Correcting (info "(emacs)Spelling")

;;     aspell -H list Article.html
;;     aspell -H check Article.html
;; Then hold your finger down on the `a' (add) or `l' (add lower case) key.

;; use Hunspell or GNU Aspell instead of `ispell'
(setq-default ispell-program-name
  (cond (running-ms-windows
         "C:/Program Files/Aspell/bin/aspell.exe")
        (t
         (if (file-executable-p "/usr/bin/hunspell")
             "/usr/bin/hunspell"
           "/usr/bin/aspell"))))

(my-file-executable-p ispell-program-name)

(when (and ispell-program-name
           (file-executable-p ispell-program-name))

  (require 'ispell)

  (eval-after-load "ispell"
    ;; so that following modifications won't be lost when ispell is loaded
    '(progn

       ;; default dictionary to use (if `ispell-local-dictionary' is nil)
       (setq ispell-dictionary "fr_FR")

       ;; save the personal dictionary without confirmation
       (setq ispell-silently-savep t)

       ;; extra switches to pass to the `ispell' program
       ;; TODO Check they are right!
       (setq ispell-extra-args
             (cond ((equal ispell-program-name "/usr/bin/hunspell")
                    '())
                    ;; '("-a" "-i" "utf-8"))
                                        ; aspell doesn't understand `-i
                                        ; utf-8', hunspell needs it
                   (t
                    '("--sug-mode=ultra"))
                                        ; tell `aspell' to speed up, though
                                        ; this reduces somewhat the quality of
                                        ; its suggestions. According to the
                                        ; `aspell' documentation:
                                        ;
                                        ; - "ultra" is the fastest suggestion
                                        ;   mode, which is still twice as slow
                                        ;   as `ispell'.
                                        ;
                                        ; - If your machine is fast enough, a
                                        ;   better option might be to try
                                        ;   "fast" mode, which is twice as
                                        ;   slow as "ultra", but more
                                        ;   accurate.
                                        ;
                                        ; - The "normal" mode, which is the
                                        ;   `aspell' default, is even more
                                        ;   accurate, but is reportedly 10
                                        ;   times slower than "fast" mode.
                   ))

    ;; redefine the list of installed dictionaries
    ;; customize to ("-B" "-d" "spanish") or ("-C" "-d" "dutch") if
    ;; aliases are needed for the file names
    ;; FIXME This variable is reset once latter in this .emacs file!!!
    (setq ispell-dictionary-alist
          ;; those not here will be "undefined dictionary"
          '(
	    ;; default
            (nil
             "[A-Za-z]" "[^A-Za-z]"
             "[']" nil ("-B") nil iso-8859-1)

	    ;; Yankee English
            ("en_US"
             "[A-Za-z]" "[^A-Za-z]"
             "[']" nil ("-B") nil utf-8)

	    ;; Spanish mode
            ("castellano"
             "[a-zñáàéèíìóòúùüA-ZÑÁÀÉÈÍÌÓÒÚÙÜ]" "[^a-zñáàéèíìóòúùüA-ZÑÁÀÉÈÍÌÓÒÚÙÜ]"
             "[-]" nil ("-B") "~tex" iso-8859-1)

	    ;; standard French
            ("fr_FR"
             "[a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]" "[^a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
             "[-']" t nil "~list" utf-8)

	    ;; Nederlands.aff
            ("nederlands"
             "[a-zàâçëîïôùûüA-ZÀÂÇËÎÏÔÙÛÜ]" "[^a-zàâçëîïôùûüA-ZÀÂÇËÎÏÔÙÛÜ]"
             "[']" t ("-C") nil iso-8859-1)
            ))

    ;; `aspell' extensions should *not* be used
    (setq ispell-really-aspell nil)

    ;; `hunspell' extensions should be used
    (setq ispell-really-hunspell t)

    ;; ;; solve the problem of words separated by `-' flagged as erroneous by
    ;; ;; removing the `-' from the value of otherchars
    ;; (if (fboundp 'ispell-get-decoded-string)
    ;;     (defun ispell-get-otherchars ()
    ;;       (replace-regexp-in-string "-" "" (ispell-get-decoded-string 3))))

    ;; from Alex Schroeder
    (defun my-change-dictionary ()
      "Change the dictionary."
      (interactive)
      (let ((dict (or ispell-local-dictionary ispell-dictionary)))
        (setq dict (if (string= dict "fr_FR") "en_US" "fr_FR"))
        (message "Switched to %S" dict)
        (sit-for 0.4)
        (ispell-change-dictionary dict)
        (when flyspell-mode
          (flyspell-delete-all-overlays)
          (flyspell-buffer))))

    ;; key bindings
    (global-set-key (kbd "<f7>") 'ispell-word)
    (global-set-key (kbd "<S-f7>") 'my-change-dictionary)
    (global-set-key (kbd "<C-f7>") 'ispell-change-dictionary)

    ))


    ;; on-the-fly spelling checking
    (autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)

    ;; don't consider that a word repeated twice is an error
    (setq flyspell-mark-duplications-flag nil)

    ;; enable the likeness criteria
    (setq flyspell-sort-corrections nil)

    ;; don't use `M-TAB' to correct word (only use `C-.')
    (setq flyspell-use-meta-tab nil)

    ;; `flyspell-auto-correct-word' is bound to `C-.'
    ;; Press it one time to correct the word under the cursor.
    ;; If several spellings are possible, they appear in the minibuffer. Just
    ;; keep hitting `C-.' to replace the word with the successive suggestions.

    ;; dash character (`-') is considered as a word delimiter
    (setq flyspell-consider-dash-as-word-delimiter-flag t)

    (defun my-turn-on-flyspell-french ()
      "Unconditionally turn on flyspell-mode (in French) and call
`flyspell-buffer'."
      (interactive)
      (flyspell-mode 1) ;; instead of just toggling the mode
      (ispell-change-dictionary "fr_FR")
      (flyspell-buffer))

    (defun my-turn-on-flyspell-english ()
      "Unconditionally turn on flyspell-mode (in American English)
and call `flyspell-buffer'."
      (interactive)
      (flyspell-mode 1)
      (ispell-change-dictionary "en_US")
      (flyspell-buffer))

    ;; turn on `flyspell' when changing a buffer which is unmodified
    (when
        (or (file-readable-p "/usr/share/myspell/dicts/fr_FR.aff") ; hunspell
            (file-readable-p "/usr/lib/aspell/francais.alias")
            (when running-ms-windows
                (file-readable-p
                 "C:/Program Files/Aspell/dict/francais.alias")))
                                        ; check that the French dictionary
                                        ; can be opened for reading

        (defvar my-flyspell-major-mode-list
          '(html-mode
            latex-mode
            message-mode
;;;             nuweb-mode  (emmerdant: je sauve, compile, et recheck spell!)
            nxml-mode
;;            org-mode  ; FIXME Problem with flyspell (.emacs is *completely* screened...)
            text-mode))

        (add-hook 'first-change-hook
                  #'(lambda ()
                      (when (and (memq major-mode my-flyspell-major-mode-list)
                                 (not flyspell-mode))
                        (my-turn-on-flyspell-french)))))

;; Org mode is derived from outline-mode, which is derived from text mode.
;; A derived mode runs all the hooks from the parent modes.
;;
;; I don't know how to turn this off, but you can work around this
;; by changing the function you put into the text-mode-hook:
;;
;; (defun my-turn-on-flyspell-not-in-org-though ()
;;   (or (eq major-mode 'org-mode) (turn-on-flyspell)))


;;;     ;; don't print messages for every word (when checking the entire buffer)
;;;     ;; as it causes an enormous slowdown
;;;     (setq flyspell-issue-message-flag nil)

    ;; flyspell comments and strings in programming modes
    ;; (preventing it from finding mistakes in the code)
    (add-hook 'autoconf-mode-hook   'flyspell-prog-mode)
    (add-hook 'autotest-mode-hook   'flyspell-prog-mode)
    (add-hook 'c++-mode-hook        'flyspell-prog-mode)
    (add-hook 'c-mode-hook          'flyspell-prog-mode)
    (add-hook 'cperl-mode-hook      'flyspell-prog-mode)
    (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
    (add-hook 'makefile-mode-hook   'flyspell-prog-mode)
    (add-hook 'nxml-mode-hook       'flyspell-prog-mode)
    (add-hook 'python-mode-hook     'flyspell-prog-mode)
    (add-hook 'sh-mode-hook         'flyspell-prog-mode)
;; (add-hook 'c-mode-common-hook 'flyspell-prog-mode t)
;; (add-hook 'java-mode-common-hook 'flyspell-prog-mode t)

    ;; spell-check your XHTML
    (eval-after-load "flyspell"
      '(progn
         (add-to-list 'flyspell-prog-text-faces 'nxml-text-face))))

    ;; TODO Have a look at `diction' (style and grammar for English)


;; pull definitions from Google and display them in a buffer
(when (try-require 'google-define)
  (global-set-key (kbd "C-c D") 'google-define))


(defun google-define-word-or-phrase (query)
  (interactive "sInsert word or phrase to search: ")
  (let* ((url (concat "http://www.google.com.pe/search?hl=en&q=define%3A"
                      (replace-regexp-in-string " " "+" query)))
         (definition
           (save-excursion
             (with-temp-buffer
               (mm-url-insert url)
               (goto-char (point-min))
               (if (search-forward "No definitions found of " nil t)
                   "No definitions found"
                 (buffer-substring (search-forward "<li>")
                                   (- (search-forward "<") 1)))))))
    (message "%s: %s" query definition)))
(global-set-key (kbd "C-c d") 'google-define-word-or-phrase)
;; TODO This seems to be the first definition of the list returned by `google-define'


;; excellent!
(defun answers-define ()
  "Look up the word under cursor in a browser."
  (interactive)
  (browse-url
   (concat "http://www.answers.com/main/ntquery?s="
           (thing-at-point 'word))))


(when (try-require 'dictionary-FIXME)
    (load "dictionary-init")

    ;; server contacted for searching the dictionary
;;    (setq dictionary-server "localhost")

    ;; connect via a HTTP proxy (using the CONNECT command)
    (setq dictionary-use-http-proxy t)

    ;; name of the HTTP proxy to use
    (setq dictionary-proxy-server "hellman")

    ;; port of the proxy server
    (setq dictionary-proxy-port 8080)

    ;; ask for a new word to search
    (global-set-key (kbd "C-c s") 'dictionary-search)

    ;; ask for a pattern and list all matching words
    (global-set-key (kbd "C-c m") 'dictionary-match-words))


(when (try-require 'dictem-FIXME)
    (setq dictem-server "localhost")
    (dictem-initialize)
    (define-key mode-specific-map [?s] 'dictem-run-search)

    (define-key dictem-mode-map [tab] 'dictem-next-link)
    (define-key dictem-mode-map [(backtab)] 'dictem-previous-link)

    ;; For creating hyperlinks on database names and found matches.
    ;; Click on them with `mouse-2'
    (add-hook 'dictem-postprocess-match-hook
	      'dictem-postprocess-match)

    ;; For highlighting the separator between the definitions found.
    ;; This also creates hyperlink on database names.
    (add-hook 'dictem-postprocess-definition-hook
	      'dictem-postprocess-definition-separator)

    ;; For creating hyperlinks in dictem buffer that contains definitions.
    (add-hook 'dictem-postprocess-definition-hook
	      'dictem-postprocess-definition-hyperlinks)

    ;; For creating hyperlinks in dictem buffer that contains information
    ;; about a database.
    (add-hook 'dictem-postprocess-show-info-hook
	      'dictem-postprocess-definition-hyperlinks))


(message "20 Commands for Fixing Typos... Done"))


;;** 21 (info "(emacs)Keyboard Macros")

(when section-keyboard-macros (message "21 Keyboard Macros...")

;;*** 21.1 (info "(emacs)Basic Keyboard Macro") Use

;; If you want to check the result each time before repeating, then
;; `C-x e e e...'.
;; If you want to repeat only N times, then `C-u N C-x e'.
;; If you want to repeat forever or until error, then `C-u 0 C-x e'.

;; <shift>-<F8>  to start recording
;; <shift>-<F8>  again to stop recording
;; <F8>          to call it

(defun my-toggle-kbd-macro-recording-on ()
  "Start recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-off)
  (start-kbd-macro nil))

(defun my-toggle-kbd-macro-recording-off ()
  "Stop recording a keyboard macro and toggle functionality of key binding."
  (interactive)
  (global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)
  (end-kbd-macro))

;; start/stop recording a keyboard macro
(global-set-key (kbd "<S-f8>") 'my-toggle-kbd-macro-recording-on)

;; execute the most recent keyboard macro
(global-set-key (kbd "<f8>") 'call-last-kbd-macro)


;;*** 21.5 Name and (info "(emacs)Save Keyboard Macro")s

;; assign a name to the last keyboard macro defined
(global-set-key (kbd "<C-f8>") 'name-last-kbd-macro)

(message "21 Keyboard Macros... Done"))


;;* Major Structures of Emacs

;;** 22 (info "(emacs)Files") Handling

(when section-files (message "22 Files Handling...")

;;*** 22.2 (info "(emacs)Visiting") Files

;; visit a file
(global-set-key (kbd "<f3>") 'find-file)

;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs'."
  (interactive)
  (find-file "~/.emacs"))
(global-set-key (kbd "<S-f3>") 'my-open-dot-emacs)

;; open my Gnus configuration file
(defun my-open-dot-gnus ()
  "Opening `~/.gnus'."
  (interactive)
  (find-file "~/.gnus"))
(global-set-key (kbd "<C-f3>") 'my-open-dot-gnus)

;; open my Timeclock file
(defun my-open-timeclock ()
  "Opening `~/Projects/Work.org'."
  (interactive)
  (find-file "~/Projects/Work.org"))
(global-set-key (kbd "<C-f4>") 'my-open-timeclock)


;;*** 22.3 (info "(emacs)Saving") Files

;; make your changes permanent
(global-set-key (kbd "<f2>") 'save-buffer)

;; (add-hook 'after-save-hook
;;           'executable-make-buffer-file-executable-if-script-p)

;; offer save of `*scratch*' buffer on exit
(save-excursion
  (set-buffer "*scratch*")
  (setq buffer-file-name "~/*scratch*"))
  ;; `(setq buffer-offer-save t)' does not have its intended effect in my
  ;; `.emacs' file (i.e., `buffer-offer-save' still has its global default
  ;; value of nil in the `*scratch*' buffer). But if I immediately evaluate it
  ;; in the `*scratch*' buffer, it works.
  ;; That is because at startup, Emacs sets the major mode of `*scratch*'
  ;; according to `initial-major-mode', _after_ my `.emacs' is read.  Changing
  ;; major modes kills all local variables that are not permanently local,
  ;; including `buffer-offer-save'.

;; ;; major mode command symbol to use for the initial `*scratch*' buffer
;; (setq initial-major-mode 'text-mode)  ; to avoid autoloads for Lisp mode (cedet)

;; ensure a file ends in a newline when it is saved
(setq require-final-newline t)
;; TODO I should do this only for text and Fundamental modes, because I could
;; edit binary files (see `mode-require-final-newline')

;; directory used for temporary files
(XEmacs
    (setq temporary-file-directory (or (getenv "TEMP") "/tmp/")))

;; maintain last change time stamps (`Time-stamp: <>' occurring within the
;; first 8 lines) in files edited by Emacs
(when (try-require 'time-stamp)

    ;; format of the string inserted by `M-x time-stamp'
    (setq time-stamp-format "%Y-%02m-%02d %3a %02H:%02M %u on %s")
                          ; `YYYY-MM-DD Weekday HH:MM user on system'
    ;; see `system-time-locale' for non-numeric formatted items of time

    ;; update time stamps every time you save a buffer
    (add-hook 'write-file-hooks 'time-stamp))

;; insert a time stamp string
(defun my-insert-time-stamp ()
  "Insert a time stamp."
  (interactive "*")
  (insert (format "%s %s %s %s"
                  comment-start
                  (format-time-string "%Y-%m-%d")
                  (user-login-name)
                  comment-end)))

(defun insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument,
add day of week. With two prefix arguments, add day of week and
time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
                      ((equal prefix '(4)) "%Y-%m-%d %a")
                      ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c .") 'insert-date)

(GNUEmacs
    ;; update the copyright notice in current buffer
    (when (try-require 'copyright)
      ; XXX Check no other copyright.el gets in the way
      (add-hook 'write-file-hooks 'copyright-update)))


;;*** 22.4 (info "(emacs)Reverting") a Buffer

;; replace current buffer text with the text of the visited file on disk
(defun my-revert-buffer ()
  "Unconditionally revert current buffer."
  (interactive)
  (flet ((yes-or-no-p (msg) t))
    (revert-buffer)))

;; key binding
(global-set-key (kbd "<C-f12>") 'my-revert-buffer)


;;*** 22.6 (info "(emacs)Auto Save"): Protection Against Disasters

;; how to get Emacs to auto-save to your local disk [`#file#']

;; auto-save every 100 input events
(setq auto-save-interval 100)

;; auto-save after 15 seconds idle time
(setq auto-save-timeout 15)

;; Check out Kevin's `ebackup.el' for saving in a specified common directory
;; (not in local dir) and handling saving of buffers with spaces in their
;; name... (otherwise, problems when composing *mail replies to ...* )

;; ;; The `auto-save.el' and `backup.el' packages collect files in one place
;; (try-require 'auto-save)
;; (try-require 'backup)

;; put backup files (i.e., `foo~' or `foo.~i~') in one place
(GNUEmacs
    ;; regexp => directory mappings
    ;; filenames matching a regexp are backed up in the corresponding directory
    (setq backup-directory-alist
          '((".*" . "~/.emacs.d/backups/"))))  ;; '(("." . "~/.saves"))
              ;; or "/tmp/"?
          ;; Emacs will `make-directory' it if necessary

(XEmacs
    (when (try-require 'backup-dir)
        (make-variable-buffer-local 'backup-inhibited)
        (setq bkup-backup-directory-info
              '((t "~/.saves" ok-create full-path prepend-name)))))

;; always use copying to create backup files (don't clobber symlinks)
(setq backup-by-copying t)

;; make numeric backup versions
(setq version-control t)

;; number of oldest versions to keep when a new numbered backup is made
(setq kept-old-versions 0)  ; 2

;; number of newest versions to keep when a new numbered backup is made
(setq kept-new-versions 20)  ; 2

;; delete excess backup versions silently
(setq delete-old-versions t)


;; make the message "FILE has auto save data" unmissable
(defface recover-this-file
  '((t :background "orange"))
  "Face for buffers visiting files with auto save data."
  :group 'files)

(defvar recover-this-file nil
  "If non-nil, an overlay indicating that the visited file has auto save data.")

(defun recover-this-file-find-file-hook ()
  ;; see after-find-file
  (let ((warn (not buffer-read-only)))
    (when (and warn
	       ;; No need to warn if buffer is auto-saved
	       ;; under the name of the visited file.
	       (not (and buffer-file-name
			 auto-save-visited-file-name))
	       (file-newer-than-file-p (or buffer-auto-save-file-name
					   (make-auto-save-file-name))
				       buffer-file-name))
      (set (make-local-variable 'recover-this-file)
	   (make-overlay (point-min) (point-max)))
      (overlay-put recover-this-file 'face 'recover-this-file))))

(add-hook 'find-file-hook 'recover-this-file-find-file-hook)


;;*** 22.9 (info "(emacs)Comparing Files")

;; default to unified diffs
(setq diff-switches "-u")

;; compare text in current window with text in next window
(global-set-key (kbd "C-c =") 'compare-windows)
;;;;;;;;;;;;;;;; FIXME Conflict with reftex


;;*** 22.10 (info "(emacs)Diff Mode")

;; run `diff' in compilation-mode
(autoload 'diff-mode "diff-mode" "Diff major mode" t)

;; extensions to `diff-mode.el'
(try-require 'diff-mode-)

;; ediff, a comprehensive visual interface to diff & patch
;; setup for Ediff's menus and autoloads
(try-require 'ediff-hook)

;; auto-refine only the regions of this size (in bytes) or less
(setq ediff-auto-refine-limit (* 2 14000))

;; do everything in one frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; split the window depending on the frame width
(setq ediff-split-window-function (lambda (&optional arg)
                                    (if (> (frame-width) 160)
                                        (split-window-horizontally arg)
                                      (split-window-vertically arg))))


;;*** 22.12 Accessing (info "(emacs)Compressed Files")

;; Using the Emacs Dired utility, you can compress or uncompress a file by
;; pressing `Z'

;; easy editing of arc/zip/zoo/lhz archives
(GNUEmacs
    (require 'arc-mode)

    ;; TODO See `archive-zip-extract'

    (when running-ms-windows
      ;; Unfortunately, if you are trying to use gunzip to uncompress a file
      ;; under Dired, you will probably encounter a problem saying "Failed to
      ;; uncompress ..." or "spawning child process: exec format error". The
      ;; problem is due to that gunzip provided by Cygwin is not an executable
      ;; file. It is a symbolic link to gzip. (You can verify this by
      ;; "ls -l /usr/bin/gunzip". Since Gnu Emacs does not understand Cygwin's
      ;; symbolic link, it cannot execute gunzip. Here is the solution.

      (require 'dired-aux)

      (defun dired-call-process (program discard &rest arguments)
        ;; 09Feb02, sailor overwrite this function because Gnu Emacs cannot
        ;; recognize gunzip is a symbolic link to gzip. Thus, if the program
        ;; is "gunzip", replace it with "gzip" and add an option "-d".

        ;; "Run PROGRAM with output to current buffer unless DISCARD is t.
        ;; Remaining arguments are strings passed as command arguments to
        ;; PROGRAM."
        ;; Look for a handler for default-directory in case it is a
        ;; remote file name.
        (let ((handler
               (find-file-name-handler (directory-file-name default-directory)
                                       'dired-call-process)))
          (if handler (apply handler 'dired-call-process
                             program discard arguments)
            (progn
              (if (string-equal program "gunzip")
                  (progn
                    (setq program "gzip")
                    (add-to-list 'arguments "-d")))
              (apply 'call-process
                     program nil (not discard) nil arguments)))))))


;;*** 22.13 (info "(emacs)File Archives")

;; simple editing of tar files as a Dired-like listing of its contents
(try-require 'tar-mode)

;; reading/writing/loading compressed files
(try-idle-require 'jka-compr)

;; ;; code for handling all sorts of compressed and encrypted files
;; (try-idle-require 'crypt++)  ; EasyPG takes care of the encryption
;; ;; allows you to encrypt/decrypt files within Emacs. I use it regularly and
;; ;; it works very reliably. When I use `C-x C-f' to access an encrypted
;; ;; file, Emacs asks me for the passphrase and then decrypts the file before
;; ;; displaying it. When I save the file, Emacs automatically encrypts it
;; ;; again.


;; Once you've encrypted a file once, either externally or using a
;; crypt++-provided command, you don't need any new functions or
;; key-bindings -- you just visit it, and you get prompted for the
;; key, and when you save it it gets re-crypted with that same key.

;; This is achieved by sniffing every file as it is visited, and
;; prompting for a key if it "looks binary".  If you blow off the prompt,
;; you get the raw file.

;; I use `epa' now, but don't like the fact that it remembers the password
;; forever...
;;
;; Hmmm, interesting -- crypt++ remembers for output, but _always_ prompts on
;; input. Which did you mean wrt epa? And by 'forever' do you mean across
;; sessions?
;;
;; Crypt++ would forget the password after a certain time. Epa seems to
;; remember the password for each XEmacs session, which for me means between
;; reboots.
;;
;; For gpg-encrypted files you can use EasyPG (aka epg, the successor of pgg)
;; <http://www.easypg.org/> (which is also used by Gnus, when available, and
;; ships with current Emacs versions).
;;
;; See http://www.emacswiki.org/emacs/GnusEncryptedAuthInfo
;;
;; I think the only setup necessary is:
;;
;; (require 'epa-setup)


;; See further section "(info "(pgg)Top") (Emacs interface to GnuPG)"

;; enable EPA to get `.gpg' files to be automatically encrypted
(require 'epa)
;; (require 'epa-file)
(epa-file-enable)

;; stop EasyPG from asking for the recipient
(setq epa-file-encrypt-to "fni@mygooglest.com")

;; If no one is selected, symmetric encryption will be performed
;; (setq epa-file-encrypt-to "")

;; cache passphrase for symmetric encryption (VERY important)
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; Not to sound paranoid. But if you want caching, it's recommended to use
;; public-key encryption instead of symmetric encryption. gpg-agent is the
;; preferred way to do this. For more information see here:
;; (info "(pgg) Prerequisites")
;; Works fine with epa as well.

;; prompt for the password in the Emacs minibuffer (instead of using a
;; graphical password prompt for gpg)
(setenv "GPG_AGENT_INFO" nil)


;; What is PGP/MIME, what is OpenPGP, and how Gnus handles them.
;; - PGP/MIME is a standard, which mml2015* implements using ep[ag]-*.
;; - OpenPGP is a standard, which ep[ag]-* implements.

;; > `mml2015-use'
;; >      Symbol indicating elisp interface to OpenPGP implementation for
;; >      PGP/MIME messages.  The default is `pgg', but `mailcrypt' and
;; >      `gpg' are also supported although deprecated.


;;*** 22.14 (info "(emacs)Remote Files")

;;**** Ange-FTP

;; transparent FTP support
(when (try-require 'ange-ftp)

    ;; try to use passive mode in ftp, if the client program supports it
    (setq ange-ftp-try-passive-mode t))  ; needed for Ubuntu


;;**** (info "(tramp)Top") TRAMP - Transparent Remote Access, Multiple Protocols

;; (other protocols than just FTP)

;; Examples: C-x C-f /ssh:fni@server:/home/fni/.bashrc
;;           C-x C-f /plink:fni@server:/home/fni/.bashrc (from Windows)
;;           C-x C-f /sudo:root@localhost:/etc/group

;; Note -- `sshfs' can give me the same functionality as TRAMP: it is like a
;; personal NFS (another mounted file system) over SSH. If you can SSH to a
;; server, you can probably do `sshfs'.

;; > I'm in shell mode, logged in on another machine over ssh, and I want to
;; > do some 'crontab -e' editing on that machine. But that will bring up a
;; > new editor, which is whatever you set in your EDITOR env variable, and
;; > both vi and emacs cannot be used in this dumb shell. How can I edit the
;; > crontab in my emacs session?
;;
;; Create the crontab on the remote machine, open it using TRAMP from your
;; machine, edit and save and then reinstall it.
;; That or simply enable x forwarding so running emacs on the remote
;; bring up emacs gtk/x on your main machine editing your cron file
;; on the remote.

;; TRAMP is very slow to load! You definitely want to autoload!
(try-idle-require 'tramp-XXX)
(eval-after-load "tramp"
  '(progn

;;** 4 (info "(tramp)Configuration") of TRAMP for use

;;*** 4.6 Selecting a (info "(tramp)Default Method")

    ;; /method:user@host:/path/file

    ;; default transfer method
    (setq tramp-default-method  ; `scp' by default
          (cond (running-ms-windows
                 ;; (issues with Cygwin `ssh' which does not cooperate with
                 ;; Emacs processes -> use `plink' from PuTTY, it definitely
                 ;; does work under Windows)
                 ;; C-x C-f /plink:myuser@host:/some/directory/file
                 "plink")
                (t
                 "ssh")))

    ;; You might try out the `rsync' method, which saves the remote files
    ;; quite a bit faster than SSH. It's based on SSH, so it works the same,
    ;; just saves faster.


;;*** 4.7 Selecting a (info "(tramp)Default User")

    ;; default user
    (setq tramp-default-user "fni")


;;*** 4.9 Connecting to a remote host using (info "(tramp)Multi-hops")

    ;; new proxy system (introduced with Tramp 2.1, instead of the old
    ;; "multi-hop" filename syntax) to edit files on a remote server by going
    ;; via another server
    (when (boundp 'tramp-default-proxies-alist)
      (add-to-list 'tramp-default-proxies-alist
                   '("10.10.13.123" "\\`root\\'" "/ssh:%h:")))
    ;; Opening `/sudo:10.10.13.123:' would connect first `10.10.13.123' via
    ;; `ssh' under your account name, and perform `sudo -u root' on that
    ;; host afterwards. It is important to know that the given method is
    ;; applied on the host which has been reached so far.
    ;; The trick is to think from the end.


;;*** 4.12 (info "(tramp)Password handling") for several connections

    ;; how many seconds passwords are cached
    (setq password-cache-expiry 60)  ; default is 16


;;*** 4.15 (info "(tramp)Remote shell setup") hints

    ;; string used for end of line in rsh connections
    (setq tramp-rsh-end-of-line  ; `\n' by default
          (cond (running-ms-windows
                 "\n")
                (t
                 "\r")))


;;*** 4.16 (info "(tramp)Auto-save and Backup") configuration

    ;; faster auto saves
    (setq tramp-auto-save-directory temporary-file-directory)


;;** 9 How to Customize (info "(tramp)Traces and Profiles")

    ;; help debugging
    (setq tramp-verbose 9)  ; default is 0

    ;; call "M-x tramp-submit-bug" to generate an email with several trace
    ;; information

    (setq tramp-debug-buffer t)

;;

    ;; "turn off" the effect of `backup-directory-alist' for TRAMP files
    (add-to-list 'backup-directory-alist
                 (cons tramp-file-name-regexp nil))

    ;; make Emacs beep after reading from or writing to the remote host
    (defadvice tramp-handle-write-region
      (after tramp-write-beep-advice activate)
      " make tramp beep after writing a file."
      (interactive)
      (beep))
    (defadvice tramp-handle-do-copy-or-rename-file
      (after tramp-copy-beep-advice activate)
      " make tramp beep after copying a file."
      (interactive)
      (beep))
    (defadvice tramp-handle-insert-file-contents
      (after tramp-copy-beep-advice activate)
      " make tramp beep after copying a file."
      (interactive)
      (beep))

    ;; define own abbreviation (for use with bookmarks)
    (add-to-list 'directory-abbrev-alist
                 (if (memq system-type '(cygwin windows-nt))
                     '("^/RUSSELL" . "//RUSSELL/Users")
                   '("^/RUSSELL" . "/smb:fni@RUSSELL:/Users")))

    ;; after adding:
    ;;     (setq coding-system-for-read 'utf-8)
    ;;     (setq coding-system-for-write 'utf-8)
    ;; to my `.emacs', TRAMP works correctly with UTF-8 files.

;; Open a file as root, easily [from Alex Schroeder]
    (defvar find-file-root-prefix "/sudo:root@localhost:"
      "*The filename prefix used to open a file with `find-file-root'.
      This should look something like \"/sudo:root@localhost:\" (new style
      TRAMP) or \"/[sudo:root@localhost]/\" (XEmacs or old style TRAMP).")

    (defvar find-file-root-history nil
      "History list for files found using `find-file-root'.")

    (defvar find-file-root-hook nil
      "Normal hook for functions to run after finding a \"root\" file.")

    (defun find-file-root ()
      "*Open a file as the root user.
      Prepends `find-file-root-prefix' to the selected file name so that it
      maybe accessed via the corresponding TRAMP method."
      (interactive)
      (require 'tramp)
      (let* (;; We bind the variable `file-name-history' locally so we can
             ;; use a separate history list for "root" files.
             (file-name-history find-file-root-history)
             (name (or buffer-file-name default-directory))
             (tramp (and (tramp-tramp-file-p name)
                         (tramp-dissect-file-name name)))
             path dir file)
        ;; If called from a "root" file, we need to fix up the path.
        (when tramp
          (setq path (tramp-file-name-path tramp)
                dir (file-name-directory path)))
        (when (setq file (read-file-name "Find file (UID = 0): " dir path))
          (find-file (concat find-file-root-prefix file))
          ;; If this all succeeded save our new history list.
          (setq find-file-root-history file-name-history)
          ;; allow some user customization
          (run-hooks 'find-file-root-hook))))

    (defface find-file-root-header-face
      '((t (:foreground "white" :background "red3")))
      "*Face use to display header-lines for files opened as root.")

    (defun find-file-root-header-warning ()
      "*Display a warning in header line of the current buffer.
      This function is suitable to add to `find-file-root-hook'."
      (let* ((warning "WARNING: EDITING FILE WITH ROOT PRIVILEGES!")
             (space (+ 6 (- (frame-width) (length warning))))
             (bracket (make-string (/ space 2) ?-))
             (warning (concat bracket warning bracket)))
        (setq header-line-format
              (propertize warning 'face 'find-file-root-header-face))))

    (add-hook 'find-file-root-hook 'find-file-root-header-warning)

    (global-set-key (kbd "C-x C-S-r") 'find-file-root)
))


;;*** 22.17 (info "(emacs)File Conveniences")

;; setup a menu of recently opened files
(try-idle-require 'recentf)
(eval-after-load "recentf"
  '(progn

     ;; file to save the recent list into
     (setq recentf-save-file "~/.emacs.d/.recentf")

     ;; maximum number of items in the recentf menu
     (setq recentf-max-menu-items 30)

     ;; to protect from TRAMP -- FIXME not correctly supported (yet) under Win32
     (setq recentf-auto-cleanup 'never)

     ;; save file names relative to my current home directory
     (setq recentf-filename-handlers '(abbreviate-file-name))

     ;; toggle `recentf' mode
     (recentf-mode 1)

     ;; add key binding
     (global-set-key (kbd "C-x C-r") 'recentf-open-files)))

;; find file (or URL) at point
(try-idle-require 'ffap)
(eval-after-load "ffap"
  '(progn

  ;; don't use default key bindings, as I want some of them to be defined
  ;; differently (`C-x C-r', for example)

  ;; function called to fetch an URL
  (setq ffap-url-fetcher 'browse-url)   ; could be `browse-url-emacs' or
                                        ; `w3m-browse-url'

  ;; visit a file
  (global-set-key (kbd "<f3>") 'find-file-at-point)))


;; Possible error: Install w3m command in `exec-path' or set `w3m-command'
;; variable correctly
(GNUEmacs
 ;; open anything
 (when (try-require 'anything-config)  ; loads `anything.el' too

   (defun anything-c-define-dummy-source (name func &rest other-attrib)
     `((name . ,name)
       (candidates "dummy")
       ,@other-attrib
       (filtered-candidate-transformer
        . (lambda (candidates source)
            (funcall ',func)))
       (requires-pattern . 1)
       (volatile)
       (category create)))

   (defun anything-c-dummy-candidate ()
     ;; `source' is defined in filtered-candidate-transformer
     (list (cons (concat (assoc-default 'name source)
                         " '" anything-input "'")
                 anything-input)))

   ;;*dummy websearch
   (defun make-anything-c-source-websearch (name url &rest extra)
     (anything-c-define-dummy-source
      (concat "Websearch for " name)
      #'anything-c-dummy-candidate
      `(action . ,(eval `(lambda (args)
                           (browse-url
                            (apply 'concat
                                   ,url anything-pattern (quote ,extra))))))))

   ;; See http://www.emacswiki.org/emacs/RubikitchAnythingConfiguration

   ;; source of candidates for anything
   (setq anything-sources
         (list
          anything-c-source-buffers     ; needs w3m-command to be set!

          anything-c-source-files-in-current-dir
          anything-c-source-file-name-history
          anything-c-source-recentf
          anything-c-source-bookmarks
          anything-c-source-man-pages
          anything-c-source-org-headline ; show Org headlines (when Org mode)
          anything-c-source-emacs-functions
          anything-c-source-imenu
          anything-c-source-bbdb
          anything-c-source-occur
          anything-c-source-kill-ring
          anything-c-source-locate      ; find files everywhere

          ;; ;; FIXME 2010-02-26 sva
          ;; ;; When google suggest is on, selecting a buffer does not work
          ;; ;; anymore: it presents the file as being empty...
          ;; anything-c-source-google-suggest ; do a quick google search

          anything-c-source-emacs-commands
          anything-c-source-fixme
          anything-c-source-emacs-source-defun

          ;; triggered only when no exact match is found
          (make-anything-c-source-websearch "Google"
                                            "http://www.google.com/search?q="
                                            "&client=emacs-anything")
          (make-anything-c-source-websearch "Emacs Wiki"
                                            "http://www.google.com/cse?cx=004774160799092323420%3A6-ff2s0o6yi&q="
                                            "&client=emacs-anything")
          (make-anything-c-source-websearch "Wikipedia"
                                            "http://en.wikipedia.org/wiki/Special:Search?search="
                                            "&sourceid=emacs-anything")

;; ;; ;;               anything-c-source-bookmarks-local
;; ;; ;;               anything-c-source-bookmarks-su
;; ;; ;;               anything-c-source-bookmarks-ssh
;; ;; ;;               anything-c-source-emms-dired
          anything-c-source-colors
          anything-c-source-customize-face
          anything-c-source-emacs-process
          anything-c-source-info-cl
          anything-c-source-info-elisp
          anything-c-source-info-pages
          anything-c-source-register
          anything-c-source-semantic
          ))

;;;         Here the extensions i use: (you can find all of them on emacswiki)
;;;
;;;         anything-complete.el
;;;         anything-dabbrev-expand.el
;;;         anything-match-plugin.el
;;;         anything-traverse.el (if you don't use traverselisp.el have a look at
;;;                                  anything-grep.el)

   ;; do not show more candidates than this limit from individual sources
   (setq anything-candidate-number-limit 999)

   ;; the user has to be idle for this many seconds, before candidates from
   ;; *delayed* sources are collected (useful for sources involving heavy
   ;; operations, so that candidates from the source are not retrieved
   ;; unnecessarily if the user keeps typing)
   (setq anything-idle-delay 0.9) ; 1.3 works nicely

   ;; ;; make anything minibuffer better input latency
   ;; (defadvice anything-check-minibuffer-input (around sit-for activate)
   ;;   (if (sit-for anything-idle-delay t)
   ;;       ad-do-it))

   ;; the user has to be idle for this many seconds, before ALL candidates are
   ;; collected (also effective for *non-delayed* sources)
   (setq anything-input-idle-delay 0.4) ; 0.6 works nicely

   ;; uses the current window to show the candidates
   (setq anything-samewindow t)

   ;; candidates separator of `multiline' source
   (setq anything-candidate-separator
         (propertize (make-string 42 ?-) 'face 'traverse-match-face))

   ;; suppress displaying sources which are out of screen at first
   (setq anything-quick-update t)

   ;; don't save history information to file
   (remove-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

   ;; select anything
   (global-set-key (kbd "<f3>") 'anything)))



;; TODO Have a look at gpicker -- a solution for quickly choosing file from
;; (possibly large) projects!

;; show image files as images (not as semi-random bits)
(GNUEmacs
    (auto-image-file-mode 1))

;; inline image minor mode
(when (try-require 'iimage)

  ;; XXX Test me!

    ;; This allows for the viewing of images in-line in Org mode documents.
    (setq iimage-mode-image-search-path (expand-file-name "~/"))

    ;; Match org file: links
    (add-to-list 'iimage-mode-image-regex-alist
                 (cons (concat "\\[\\[file:\\(~?"
                               iimage-mode-image-filename-regex
                               "\\)\\]")  1))

    (defun org-toggle-iimage-in-org ()
      (interactive)
      (let ((turning-on (not iimage-mode)))
        (set-face-underline-p 'org-link (not turning-on))
        (iimage-mode (or turning-on 0)))))

(message "22 Files Handling... Done"))


;;** 23 Using Multiple (info "(emacs)Buffers")

(when section-buffers (message "23 Using Multiple Buffers...")

;;*** 23.2 (info "(emacs)List Buffers")

;; The C (current) column has a `.' for the buffer from which you came.
;; The R (read-only) column has a `%' if the buffer is read-only.
;; The M (modified) column has a `*' if it is modified.

;; rebind `C-x C-b'
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)
    ;; `buffer-menu' moves point in the window which lists your buffers
    ;; `electric-buffer-list' pops up a buffer describing the set of buffers

;; operate on buffers like Dired
(when (try-require 'ibuffer)
    ;; completely replaces `list-buffer'
    (defalias 'ibuffer-list-buffers 'list-buffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer)



    (setq ibuffer-show-empty-filter-groups nil)
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("version control" (or (mode . svn-status-mode)
                                    (mode . svn-log-edit-mode)
                                    (name . "^\\*svn-")
                                    (name . "^\\*vc\\*$")
                                    (name . "^\\*Annotate")
                                    (name . "^\\*git-")
                                    (name . "^\\*vc-")))
             ("emacs" (or (name . "^\\*scratch\\*$")
                          (name . "^\\*Messages\\*$")
                          (name . "^TAGS\\(<[0-9]+>\\)?$")
                          (name . "^\\*Help\\*$")
                          (name . "^\\*info\\*$")
                          (name . "^\\*Occur\\*$")
                          (name . "^\\*grep\\*$")
                          (name . "^\\*Compile-Log\\*$")
                          (name . "^\\*Backtrace\\*$")
                          (name . "^\\*Process List\\*$")
                          (name . "^\\*gud\\*$")
                          (name . "^\\*Man")
                          (name . "^\\*WoMan")
                          (name . "^\\*Kill Ring\\*$")
                          (name . "^\\*Completions\\*$")
                          (name . "^\\*tramp")
                          (name . "^\\*shell\\*$")
                          (name . "^\\*compilation\\*$")))
             ("emacs source" (or (mode . emacs-lisp-mode)
                                 (filename . "/Applications/Emacs.app")
                                 (filename . "/bin/emacs")))
             ("agenda" (or (name . "^\\*Calendar\\*$")
                           (name . "^diary$")
                           (name . "^\\*Agenda")
                           (name . "^\\*org-")
                           (name . "^\\*Org")
                           (mode . org-mode)
                           (mode . muse-mode)))
             ("latex" (or (mode . latex-mode)
                          (mode . LaTeX-mode)
                          (mode . bibtex-mode)
                          (mode . reftex-mode)))
             ("dired" (or (mode . dired-mode))))))

    (add-hook 'ibuffer-mode-hook
              (lambda ()
                (ibuffer-switch-to-saved-filter-groups "default")))

    ;; Order the groups so the order is : [Default], [agenda], [emacs]
    (defadvice ibuffer-generate-filter-groups (after reverse-ibuffer-groups ()
                                                     activate)
      (setq ad-return-value (nreverse ad-return-value)))
    )

;; customizable buffer-selection with multiple menus
(GNUEmacs
    (when window-system
      (require 'msb)))

;; buffer selection
(GNUEmacs
    (try-require 'ibs))

;; make a menu of buffers so you can manipulate buffers or the buffer list
(global-set-key (kbd "C-x C-b") 'bs-show)

;; `cyclebuffer.el'

;; put the current buffer at the end of the list of all buffers
(global-set-key (kbd "<f12>") 'bury-buffer)








;; ;; Like standard Emacs 22 commands (bound to C-x left/right)
;; (define-key global-map [f11] 'previous-buffer) ;; my-buffer-prev
;; (define-key global-map [f12] 'next-buffer)     ;; my-buffer-next

;; ;; Like standard Emacs 22 commands (bound to M-g n/p)
;; (define-key global-map [(control f11)] 'previous-error)
;; (define-key global-map [(control f12)] 'next-error)
;; (define-key global-map [(control shift f11)] 'compilation-previous-file)
;; (define-key global-map [(control shift f12)] 'compilation-next-file)

















;;*** 23.4 (info "(emacs)Kill Buffer")

;; kill buffer without confirmation (if not modified)
(defun my-kill-this-buffer ()
  "Kill the current buffer without confirmation (if not modified)."
  (interactive)
;;;   (let ((bufname (buffer-name)))
;;;     (if (or
;;;          (string-equal "*Group*" bufname))
;;;         (bury-buffer bufname)
      (kill-buffer nil))
;;;       ))

;; key binding
(global-set-key (kbd "<S-f12>") 'my-kill-this-buffer)


;;*** 23.7 (info "(emacs)Buffer Convenience") and Customization of Buffer Handling

;; unique buffer names dependent on file name
(try-idle-require 'uniquify)
(eval-after-load "uniquify"
  '(progn

    ;; style used for uniquifying buffer names with parts of directory name
    (setq uniquify-buffer-name-style 'forward)))

(message "23 Using Multiple Buffers... Done"))


;;** 24 Multiple (info "(emacs)Windows")

(when section-windows (message "24 Multiple Windows...")

;; TODO Have a look at `ido'

;;*** 24.1 Concepts of Emacs (info "(emacs)Basic Window")s

;; turn off this horrible tab thingy in XEmacs
(XEmacs
    (when (boundp 'default-gutter-visible-p)
      (set-specifier default-gutter-visible-p nil)))


;;*** 24.3 Using (info "(emacs)Other Window")

;; cycle through all windows on current frame
(global-set-key (kbd "<f6>") 'other-window)


;;*** 24.6 Deleting and (info "(emacs)Change Window")

;; delete all windows in the selected frame except the selected window
(global-set-key (kbd "<f5>") 'delete-other-windows)

;; enlarge or shrink windows more easily than with `C-x {' and the like
(global-set-key (kbd "<C-S-up>") 'enlarge-window)
(global-set-key (kbd "<C-S-down>") 'shrink-window)
(global-set-key (kbd "<C-S-left>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-S-right>") 'shrink-window-horizontally)

;; make all visible windows the same height (approximately)
(global-set-key (kbd "<C-f6>") 'balance-windows)

;; swap 2 windows
(defun my-swap-windows ()
  "If you have 2 windows, it swaps them."
  (interactive)
  (cond ((not (= (count-windows) 2))
         (message "You need exactly 2 windows to do this."))
        (t
         (let* ((w1 (first (window-list)))
                (w2 (second (window-list)))
                (b1 (window-buffer w1))
                (b2 (window-buffer w2))
                (s1 (window-start w1))
                (s2 (window-start w2)))
           (set-window-buffer w1 b2)
           (set-window-buffer w2 b1)
           (set-window-start w1 s2)
           (set-window-start w2 s1)))))

(global-set-key (kbd "C-c ~") 'my-swap-windows)

(defun my-toggle-window-split ()
  "Vertical split shows more of each line, horizontal split shows
more lines. This code toggles between them. It only works for
frames with exactly two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-c |") 'my-toggle-window-split)


;;  25.7 (info "(emacs)Window Convenience") Features and Customization

;; Use `M-x scroll-all-mode' to scroll all visible windows together in
;; parallel.

;; numbered window shortcuts
;; (It numbers windows and you can switch them easily with `M-<number>').
(when (try-require 'window-numbering)
  (window-numbering-mode 1))


;; >> Is there any way of making a particular window persistent? I have an erc
;; >> window which obviously disappears when I do other things and I'd like to
;; >> keep a small window at the bottom of the screen so I can keep an eye on
;; >> it. Is there a function or hack to do this?
;; >
;; > You can store the window configuration in a register, and jump back to it.
;; >
;; > But if you want to keep an eye on it, the best is to open another
;; > frame, if you're using a window manager.
;; >
;; > C-x 5 2    to create a new frame
;; > C-x 5 o    to switch from one frame to the other.
;;
;; Also, frob special-display-buffer-names: You can make a window dedicated,
;; which does just what you want. You can do it for all windows or windows for
;; buffers whose names match some pattern, and so on.
;;
;; Check the Elisp manual (that's Emacs Lisp), and look for `dedicated'
;; windows. See, in particular, user options `special-display-buffer-names'
;; and `special-display-regexps'.

;; winring

(message "24 Multiple Windows... Done"))


;;** 25 (info "(emacs)Frames") and Graphical Displays ]

(when section-frames (message "25 Frames and Graphical Displays...")

;;*** 25.1 (info "(emacs)Cut and Paste") on Graphical Displays

;; copy/paste with Gnome desktop
(GNUEmacs
    ;; cutting and pasting uses the clipboard
    (setq x-select-enable-clipboard t)

    ;; make cut, copy and paste (keys and menu bar items) use the clipboard
    (menu-bar-enable-clipboard)

    ;; ;; UNDER-TEST mouse drag copies region to kill-ring
    ;; (setq mouse-drag-copy-region nil)

    ;; ;; UNDER-TEST cutting and pasting does not use the primary selection
    ;; (setq x-select-enable-primary nil)

    ;; ;; UNDER-TEST an active region automatically becomes the window selection
    ;; (setq select-active-regions t)
    )


;;*** 25.5 (info "(emacs)Creating Frames")

;; - resize the frame to the size you want
;; - enter `(frame-parameters)' in the `*scratch*' buffer
;; - evaluate the form: place the cursor after the closing paren, and type
;;   `C-j', so that the output goes right into the `*scratch*' buffer

;; put Emacs exactly where you want it, every time it starts up, by
;; auto-detecting the screen dimensions and computing where it should be
(when window-system
  ;; list of frame parameters for creating the initial frame
  (setq initial-frame-alist '((top . 0) (left . 0)))

  (setq initial-frame-alist
        (append (list
                 '(internal-border-width . 2)
                 '(line-spacing	         . 1))
                initial-frame-alist))

  ;; list of default values for frame creation
  (setq default-frame-alist
        (cond ((= (x-display-pixel-height) 1200)
               '((left . 0) (height . 74)))

              ((= (x-display-pixel-height) 1024)
               '((left . 0) (height . 63)))

              ((= (x-display-pixel-height) 800)
               (cond (running-ms-windows
                      '((left . 0) (height . 55)))
                     (running-gnu-linux
                      '((left . 0) (height . 47)
                        (vertical-scroll-bars . right)))))

              ((= (x-display-pixel-height) 768)
               '((left . 0) (height . 46)))))

  (setq default-vertical-scroll-bar 'right))

(XEmacs
 (set-frame-width (buffer-dedicated-frame) 80)
 (set-frame-height (buffer-dedicated-frame) 42)
 (set-frame-position (buffer-dedicated-frame) 0 0))

;; title bar display of visible frames
(setq frame-title-format "Emacs")


;;; From sample .emacs
;;; local Emacs background:  default
;;; remote Emacs background: palegreen1
;;; root Emacs background:   coral2
;;
;;(cond
;; ((and (string-match "XEmacs" emacs-version)
;;       (eq window-system 'x)
;;       (boundp 'emacs-major-version)
;;       (= emacs-major-version 19)
;;       (>= emacs-minor-version 12))
;;  (let* ((root-p (eq 0 (user-uid)))
;;        (dpy (or (getenv "DISPLAY") ""))
;;        (remote-p (not
;;                   (or (string-match "^\\(\\|unix\\|localhost\\):" dpy)
;;                       (let ((s (system-name)))
;;                         (if (string-match "\\.\\(netscape\\|mcom\\)\\.com" s)
;;                             (setq s (substring s 0 (match-beginning 0))))
;;                         (string-match (concat "^" (regexp-quote s)) dpy)))))
;;        (bg (cond (root-p "coral2")
;;                  (remote-p "palegreen1")
;;                  (t nil))))
;;    (cond (bg
;;          (let ((def (color-name (face-background 'default)))
;;                (faces (face-list)))
;;            (while faces
;;              (let ((obg (face-background (car faces))))
;;                (if (and obg (equal def (color-name obg)))
;;                    (set-face-background (car faces) bg)))
;;              (setq faces (cdr faces)))))))))


;;*** 25.6 (info "(emacs)Frame Commands")

(XLaunch
    (defun toggle-full-screen ()
      "Toggle between full screen and partial screen display on X11;
    courtesy of http://www.emacswiki.org/cgi-bin/wiki/FullScreen"
      (interactive)
      (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                             '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

    (global-set-key (kbd "C-c z") 'toggle-full-screen))

(GNUEmacs
   (when running-ms-windows
        (defun w32-maximize-frame ()
          "Maximize the current frame."
          (interactive)
          (w32-send-sys-command 61488)
          (global-set-key (kbd "C-c z") 'w32-restore-frame))

        (global-set-key (kbd "C-c z") 'w32-maximize-frame)

        (defun w32-restore-frame ()
          "Restore a minimized frame."
          (interactive)
          (w32-send-sys-command 61728)
          (global-set-key (kbd "C-c z") 'w32-maximize-frame))))


;;*** 25.7 (info "(emacs)Speedbar") Frames

;; everything browser (into individual source files), or Dired on steroids
(when (try-require 'speedbar-XXX)

    ;; number of spaces used for indentation
    (setq speedbar-indentation-width 2)

    ;; expand/collapse LaTeX sections
    (speedbar-add-supported-extension '(".tex" ".bib" ".w" ".nw"))

    ;; jump to speedbar frame
    (global-set-key (kbd "<f4>") 'speedbar-get-focus)

    ;; bind the arrow keys in the speedbar tree
    ;; [http://www.uweb.ucsb.edu/~dreamtheorist/emacs.html]
    (define-key speedbar-key-map (kbd "<right>") 'speedbar-expand-line)
    (define-key speedbar-key-map (kbd "<left>") 'speedbar-contract-line)

    ;; parameters to use when creating the speedbar frame in Emacs
    (setq speedbar-frame-parameters '((width . 30)
                                      (height . 45)
                                      (foreground-color . "blue")
                                      (background-color . "white"))))

;; speedbar frame (vs window)
(when (try-require 'sr-speedbar)
    (global-set-key (kbd "<f4>") 'sr-speedbar-toggle))


;;*** 25.12 Scrolling with (info "(emacs)Wheeled Mice")

;; mouse wheel support
(GNUEmacs
 (mwheel-install))


;;*** 25.14 (info "(emacs)Menu Bars")

;; turn menus off
(unless window-system
    (menu-bar-mode 0))


;;*** 25.16 Using (info "(emacs)Dialog Boxes")

;; don't use dialog boxes to ask questions
(setq use-dialog-box nil)

;; don't use a file dialog to ask for files
(setq use-file-dialog nil)


;;*** 25.18 (info "(emacs)Mouse Avoidance")

(when window-system
    ;; make mouse pointer stay out of the way of editing
    (when (try-require 'avoid)
        (mouse-avoidance-mode 'jump)))


;; Move the mouse to the screen corner on any keypress.
(when (and (display-mouse-p) (require 'avoid nil t))
  ;; Move the mouse to the lower-right corner instead of default upper-right
  ;; (defun mouse-avoidance-banish-destination ()
  ;;   (cons (+ 3 (frame-width)) (frame-height)))
  (mouse-avoidance-mode 'banish))



(message "25 Frames and Graphical Displays... Done"))


;;** 26 (info "(emacs)International") Character Set Support ]

(when section-international (message "26 International Character Set Support...")

;; To open (or save) a file in UTF-8, you can press `C-x RET c utf-8 RET'
;; (`universal-coding-system-argument') before the `C-x C-f' (or `C-x C-s')

;; To help you find all the chars you need to replace by escape sequences, you
;; can use `C-u C-s [^[:ascii:]]'

;; To check your locale settings, you can have a look to what Emacs produce
;; (in a mail buffer) under "Important settings" when you type
;; `M-x report-emacs-bug RET foo RET':
;;
;; Important settings:
;;   value of $LC_ALL: nil
;;   value of $LC_COLLATE: nil
;;   value of $LC_CTYPE: nil
;;   value of $LC_MESSAGES: nil
;;   value of $LC_MONETARY: nil
;;   value of $LC_NUMERIC: nil
;;   value of $LC_TIME: nil
;;   value of $LANG: en_US.UTF-8
;;   value of $XMODIFIERS: nil
;;   locale-coding-system: utf-8-unix
;;   default-enable-multibyte-characters: t


;;*** 26.3 (info "(emacs)Language Environments")

;; system locale to use for formatting time values (e.g., timestamps in
;; Org mode files)
(setq system-time-locale "en_US.utf8")  ; "C"?


;;*** 26.5 (info "(emacs)Select Input Method")

;; `M-x describe-coding-system RET RET'

;; ;; default input method for multilingual text
;; (setq default-input-method "latin-1-prefix")

;; To see all the non-ASCII characters you can type with the `C-x 8' prefix,
;; type `C-x 8 C-h'.


;;*** 26.6 (info "(emacs)Coding Systems")

;; For any user who needs symbols that are not in the 7-bit ASCII set, our
;; recommendation is to move to Unicode UTF-8. That is the only encoding that
;; is the same across all platforms and operating systems that support it.


;;*** 26.7 (info "(emacs)Recognize Coding") Systems

(add-to-list 'file-coding-system-alist
             '("\\.owl\\'" utf-8 . utf-8))
             ;; and all the rest is utf-8:
             ;; '("" . utf-8)

;; In GNU Emacs, when you specify the coding explicitly in the file, that
;; overrides `file-coding-system-alist'. Not in XEmacs?

;; The variable `auto-coding-alist' is the strongest way to specify the coding
;; system for certain patterns of file names, or for files containing certain
;; patterns; this variable even overrides `-*-coding:-*-' tags in the file
;; itself.

;; default coding system (for new files),
;; also moved to the front of the priority list for automatic detection
(GNUEmacs
 (cond (running-ms-windows
        (prefer-coding-system 'iso-latin-1))  ; FIXME Temp for PFlow
       (t
        (prefer-coding-system 'utf-8))))
        ; or set environment variables like `LC_CTYPE', `LC_ALL' or `LANG'


;;*** 26.8 (info "(emacs)Specify Coding") System of a File

(GNUEmacs
 ;; to copy and paste to and from Emacs through the clipboard (with coding
 ;; system conversion)
 (cond (running-ms-windows
        (set-selection-coding-system 'compound-text-with-extensions))
       (t
        (set-selection-coding-system 'utf-8))))


;;*** 26.17 (info "(emacs)Undisplayable Characters")

(GNUEmacs
    ;; display page delimiter character `^L' as an horizontal line
    (when (try-require 'pp-c-l)

        ;; function to produce string displayed in place of each Control-l char
        (setq pp^L-^L-string-function
              (lambda nil
                (make-string (1- (window-width)) (string-to-char " "))))

        ;; string displayed just before `pp^L-^L-string'
        (setq pp^L-^L-string-pre "")

        ;; turn on pretty display of `^L'
        (pretty-control-l-mode 1)

        ;; normal hook run to initialize window system display
        (add-hook 'window-setup-hook
                  'refresh-pretty-control-l)

        ;; functions to call when window configuration changes
        (add-hook 'window-configuration-change-hook
                  'refresh-pretty-control-l)))

(message "26 International Character Set Support... Done"))


;;* Advanced Features

;;** 27 (info "(emacs)Major Modes")

(when section-major-modes (message "27 Major Modes...")

;; TODO
;; Have a look at http://www.emacswiki.org/emacs/AutomaticFileHeaders


;;*** 27.1 How (info "(emacs)Choosing Modes")

;; ;; default major mode for new buffers and any files with unspecified mode
;; (when (locate-library "org.el")
;;     (setq-default major-mode 'org-mode))

;; Since `foo-mode' toggles Foo (minor) mode, it's better to use a function on
;; those hooks that unconditionally turns it on. Many minor modes have
;; `turn-on-foo-mode' and `turn-off-foo-mode' convenience functions for that
;; purpose, but you can fake it with an anonymous function:
;;
;;     (add-hook 'text-mode-hook (lambda () (foo-mode 1)))
;;
;; or define your own convenience function and use that instead:
;;
;;     (defun turn-on-foo-mode ()
;;       "Turn on Foo mode."
;;       (foo-mode 1))
;;     (add-hook 'text-mode-hook 'turn-on-foo-mode)

(autoload 'dtd-mode "tdtd" "Major mode for SGML and XML DTDs." t)
(autoload 'sql-mode "sql" nil)
(autoload 'css-mode "css-mode")
(autoload 'nxml-mode  "nxml-mode" "XML mode" t)
(autoload 'ssh-config-mode "ssh-config-mode" t)

;; ledger
(try-require 'ledger)

;; 1. list of filename patterns
;;    vs. corresponding major mode functions
(setq auto-mode-alist
      (append '(
                ("\\.css\\'"                           . css-mode)
                ("\\.\\(htm\\|html\\|xhtml\\)$"        . nxhtml-mode)
                ("\\.sql$"                             . sql-mode)
;;                ("\\.js$"                              . java-mode)
                ("\\.dcl$"                             . dtd-mode)
                ("\\.dec$"                             . dtd-mode)
                ("\\.dtd$"                             . dtd-mode)
                ("\\.ele$"                             . dtd-mode)
                ("\\.ent$"                             . dtd-mode)
                ("\\.mod$"                             . dtd-mode)

                ;; sorted by chapter
                ("\\.\\(diffs?\\|patch\\|rej\\)\\'"    . diff-mode)
                ("\\.txt$"                             . org-mode)
                ("\\.dat$"                             . ledger-mode)

                ("\\.log$"                             . text-mode)
                ("\\.tex$"                             . LaTeX-mode)
                ("\\.tpl$"                             . LaTeX-mode)
                ("\\.cgi$"                             . perl-mode)
                ("[mM]akefile"                         . makefile-mode)
                ("\\.bash$"                            . shell-script-mode)
                ("\\.expect$"                          . tcl-mode)

                (".ssh/config\\'"                      . ssh-config-mode)
                ("sshd?_config\\'"                     . ssh-config-mode)
                ) auto-mode-alist))

;; major mode for editing comma-separated value files
(when (try-require 'csv-mode)

    ;; field separators: a list of *single-character* strings
    (setq csv-separators '("," ";")))


;; See (info "(elisp) Syntax of Regexps")
;;     \' matches end of string
;;      $ matches end of line


;; Some Emacs modes are over 10K lines of code. (e.g. js2-mode, nxml-mode,
;; CEDET). Many packages make use of the `autoload' feature, so that you only
;; need to load a single file that define autoloaded functions.

;; For example, nxml-mode's instruction tells you to do:
;; (when (load-library
;;        "~/Downloads/emacs/site-lisp/nxml-1.33-20080630/autostart.el")
;;
;;     ;; always skip the nXhtml welcome message
;;     (setq nxhtml-skip-welcome t))

(add-to-list 'auto-mode-alist
             (cons "\\.\\(xml\\|xsd\\|sch\\|rng\\|xslt\\|svg\\|rss\\|owl\\|xbl\\)\\'"
                   'nxml-mode))
(fset 'xml-mode 'nxml-mode)

    ;; instead of superseding the binding in `auto-mode-alist', you can
    ;; replace it (brute force) with
    ;; `(setcdr (rassq 'old-mode auto-mode-alist) 'new-mode)'

;; 2. list of buffer beginnings
;;    vs. corresponding major mode functions (Emacs 22+)
;;    see `magic-mode-alist'

;; 3. list of interpreters specified in the first line (starts with `#!')
;;    vs. corresponding major mode functions
(push '("expect" . tcl-mode) interpreter-mode-alist)

;; multiple major modes
;; - nXhtml includes `mumamo.el' (= one of the most compatible with Org-babel)
;; - MMM mode

;; load generic modes which support e.g. batch files
(try-require 'generic-x)

(message "27 Major Modes... Done"))


;;** 28 (info "(emacs)Indentation")

;; Just hit C-j -- as in other modes, it runs the command
;; `newline-and-indent'.
;; Usually one binds `RET' to `newline-and-indent'.

(when section-indentation (message "28 Indentation...")

;;*** 28.1 (info "(emacs)Indentation Commands") and Techniques

;; `C-M-\' runs the command `indent-region' (which does the job of
;; the imaginary command `unsuck-html-layout' in `html-mode')

(defun indent-whole-buffer ()
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (indent-for-tab-command)))


;;*** 28.3 Tabs vs. (info "(emacs)Just Spaces")

;; indentation can't insert tabs
(setq-default indent-tabs-mode nil)

(message "28 Indentation... Done"))


;;** 29 Commands for (info "(emacs)Text") Human Languages ]

(when section-text (message "29 Commands for Human Languages...")

;;*** 29.1 (info "(emacs)Words")

;; GNU Emacs default for killing back to the beginning of a word
(XEmacs
    (global-set-key [(control backspace)] 'backward-kill-word))

;; delete previous character, changing tabs into spaces
(global-set-key [(shift backspace)] 'backward-delete-char-untabify)


;;*** 29.2 (info "(emacs)Sentences")

;; FIXME When changing the ispell language, this should be changed
;; simultaneously.

;; a single space does end a sentence
(setq-default sentence-end-double-space nil)

;; See `sentence-end' and `(sentence-end)'


;;*** 29.5 (info "(emacs)Filling") Text

(defun my-text-mode-hook ()
  "Turn on filling modes in text mode."
  (turn-on-auto-fill)

  ;; adaptative filling
  (when (try-require 'filladapt)
      (setq-default filladapt-mode nil)

      ;; turn on filladapt mode everywhere but in ChangeLog files
      (cond ((equal mode-name "Change Log")
             t)
            (t
             (turn-on-filladapt-mode)))))

;; 2010-05-21 John Wiegley:
;; The Emacs ChangeLog is a file which predates the existence of freely
;; available, project-wide version control. It was a way to see, in one place,
;; the stream of changes occurring in a project -- something which RCS could
;; not do for you.
;; However, in this modern era of project-wide, atomic commits, the ChangeLog
;; is not only an archaism, but is a continuous source of merge conflicts. For
;; example, when I reverted Russell's latest change -- a one-liner that was
;; minor in the extreme -- I had to do with a merge conflict in
;; lisp/ChangeLog.
;; With a system like Git, and properly written commits, you can produce a
;; ChangeLog at any time with "git log". You even see a ChangeLog for just one
;; file, or a directory with "git log --follow PATH". This completes
;; supersedes any need for a ChangeLog file, and has led me to abandon the use
;; of ChangeLogs in all the projects I maintain.

;; 2010-05-21 Ben Finney:
;; It seems worth pointing out explicitly, though: Eliminating a
;; manually-maintained ChangeLog doesn't obviate the need for a ChangeLog (or
;; the equivalent) in the distributed source.
;; This is because the copyright holders license their works under the GPLv2,
;; and §2.a of those terms requires the work to include dated notice of all
;; modifications made to the work. This is conventionally understood to be
;; most directly satisfied by a ChangeLog in the distributed source for the
;; work.
;; Generating that file automatically from the VCS commit messages, at the
;; time a source release is packaged, is a good use of the VCS.



;; turn on my text setup
(add-hook 'text-mode-hook 'my-text-mode-hook)

;; fabrication automatique de la typo française avec la ponctuation
;; ajout automatique de l'espace insécable là où cela va bien

(defun my-insert-interrogation-mark ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~?")
        (insert " ?"))) ; non-breaking space
    (insert "?")))

(defun my-insert-exclamation-mark ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~!")
        (insert " !"))) ; non-breaking space
    (insert "!")))

;; FIXME Remove NBSP if two colons are put one after the other (for terms and
;; definitions in Org)

(defun my-insert-colon ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~:")
          (insert " :"))) ; non-breaking space
    (insert ":")))

(defun my-insert-semi-colon ()
  (interactive)
  (if (eq (char-before) ?\ ) ; normal space
      (progn
        (backward-delete-char 1)
        (if (equal mode-name "PDFLaTeX")
            (insert "~;")
        (insert " ;"))) ; non-breaking space
    (insert ";")))

(defun my-double-keys ()
  "Touches spécifiques"
  (interactive)
  (local-set-key "?" 'my-insert-interrogation-mark)
  (local-set-key "!" 'my-insert-exclamation-mark)
  (local-set-key ":" 'my-insert-colon)
  (local-set-key ";" 'my-insert-semi-colon))

;; typo auto pour les modes suivants
(add-hook 'text-mode-hook 'my-double-keys)
(add-hook 'message-mode-hook 'my-double-keys)






;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'fill-nobreak-predicate 'fill-french-nobreak-p)
;; (add-hook 'fill-nobreak-predicate 'fill-single-word-nobreak-p)










(defun insert-one-quote-or-two ()
  (interactive)
  (cond
   ((or (bolp) (not (looking-back "'")))
    ;; insert just one '
    (self-insert-command 1))
   ((save-excursion
      (backward-char)
      ;; Skip symbol backwards.
      (and (not (zerop (skip-syntax-backward "w_")))
	   (not (looking-back "`"))
	   (or (insert-and-inherit "`") t))))
   (t
    ;; insert `' around following symbol
    (delete-backward-char 1)
    (unless (looking-back "`") (insert-and-inherit "`"))
    (save-excursion
      (skip-syntax-forward "w_")
      (unless (looking-at "'") (insert-and-inherit "'"))))))

(global-set-key [39] 'insert-one-quote-or-two)


;; automatic line-wrapping beyond that column
(setq-default fill-column 78)


;;*** 29.6 (info "(emacs)Case") Conversion Commands

;; enable the use of the commands `downcase-region' and `upcase-region'
;; without confirmation
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;;*** 29.8 (info "(emacs)Outline Mode")

;; Outline is line-oriented and does not distinguish end-of-block.

;; `outline-minor-mode.el' is also used to collapse Lisp code (i.e., to see in
;; the buffer just the definition of a function instead of the whole body)

;; See also the library `foldout' and `hs-minor-mode', for instance, in the
;; Emacs manual.

;; outline mode commands for Emacs
(when (try-require 'outline)

    ;; bind the function `open-line' to `M-o' instead of `C-o' (by default)
    (global-set-key (kbd "M-o") 'open-line)

    ;; bind the outline minor mode functions to an easy to remember prefix key
    ;; (more accessible than the horrible prefix `C-c @')
    (setq outline-minor-mode-prefix (kbd "C-o"))

    ;; make other `outline-minor-mode' files (LaTeX, etc.) feel like Org files
    (when (try-require 'outline-magic)
        (add-hook 'outline-minor-mode-hook
                  (lambda ()
                    (define-key outline-minor-mode-map
                      (kbd "<backtab>") 'outline-cycle)

                    (define-key outline-minor-mode-map
                      (kbd "<M-left>") 'outline-promote)
                    (define-key outline-minor-mode-map
                      (kbd "<M-right>") 'outline-demote)
                    (define-key outline-minor-mode-map
                      (kbd "<M-up>") 'outline-move-subtree-up)
                    (define-key outline-minor-mode-map
                      (kbd "<M-down>") 'outline-move-subtree-down))))

    ;; extra support for outline minor mode
    (try-require 'out-xtra)


    ;; Org-style folding for a `.emacs' (and much more)
    (defun my-outline-regexp ()
      "Calculate the outline regexp for the current mode."
      (let ((comment-starter (replace-regexp-in-string
                              "[[:space:]]+" "" comment-start)))
        (when (string= comment-start ";")
          (setq comment-starter ";;"))
     ;; (concat "^" comment-starter "\\*+")))
        (concat "^" comment-starter "[*]+ ")))

    (defun my-outline-minor-mode-hook ()
      (interactive)
      (setq outline-regexp (my-outline-regexp))

      ;; highlight the headings
      ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
      ;; use `M-x customize-apropos-faces' to customize faces
      ;; to find the corresponding face for each outline level, see
      ;; `org-faces.el'

      ;; Added `\n?', after having read the following chunk of code (from org.el):
      ;; `(,(if org-fontify-whole-heading-line
      ;;        "^\\(\\**\\)\\(\\* \\)\\(.*\n?\\)"
      ;;      "^\\(\\**\\)\\(\\* \\)\\(.*\\)")

      (let ((org-fontify-whole-heading-line "") ; "\n?")
            (heading-1-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{1\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-2-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{2\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-3-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{3\\} \\(.*" org-fontify-whole-heading-line "\\)"))
            (heading-4-regexp
             (concat (substring outline-regexp 0 -1)
                     "\\{4,\\} \\(.*" org-fontify-whole-heading-line "\\)")))
        (font-lock-add-keywords
         nil
         `((,heading-1-regexp 1 'org-level-1 t)
           (,heading-2-regexp 1 'org-level-2 t)
           (,heading-3-regexp 1 'org-level-3 t)
           (,heading-4-regexp 1 'org-level-4 t)))))

    (add-hook 'outline-minor-mode-hook
              'my-outline-minor-mode-hook)

    ;; Add the following as the top line of your `.emacs':
    ;;
    ;; ; -*- mode: emacs-lisp; mode: outline-minor; -*-
    ;;
    ;; Now you can add `;;*' and `;;**', etc. as headings in your `.emacs' and
    ;; cycle using `M-tab', `M-left' and `M-right' will collapse or expand all
    ;; headings respectively. I am guessing you mean to make segments such as
    ;; `;;* SHORTCUTS' and `;;* VARIABLES', this will do that, but not too much
    ;; more.
    )


    ;; Explorer-like bindings (`M-left/right/up/down' to navigate outlines)
    (when (locate-library "outline-mode-easy-bindings.el")

      (add-hook 'outline-mode-hook
                '(lambda ()
                   (require 'outline-mode-easy-bindings)))

      (add-hook 'outline-minor-mode-hook
                '(lambda ()
                   (require 'outline-mode-easy-bindings))))


;; I really like the following for outline-node based navigation.  It is
;; similar to the behavior of paredit-mode in lisp files.
;; org-mode hook
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "\C-\M-n") 'outline-next-visible-heading)
	    (local-set-key (kbd "\C-\M-p") 'outline-previous-visible-heading)
	    (local-set-key (kbd "\C-\M-u") 'outline-up-heading)))



(global-set-key [backtab] 'org-cycle) ; that works (but on level 1+)
;; TODO Look at org-cycle-global and local below, they work better, but still
;; on level 1+
;; TODO Replace it by a function which alternatively does hide-body and
;; show-all

;; XXX 2010-06-21 Conflicts with outline-minor-mode bindings
;; ;;; Use `orgstruct-mode' in `emacs-lisp-mode' buffers
;; (add-hook 'emacs-lisp-mode-hook 'orgstruct-mode)

(defun org-cycle-global ()
  (interactive)
  (org-cycle t))

(defun org-cycle-local ()
  (interactive)
  (save-excursion
    (move-beginning-of-line nil)
    (org-cycle)))

(global-set-key (kbd "C-M-]") 'org-cycle-global) ; ok on Elisp, not on LaTeX
(global-set-key (kbd "M-]") 'org-cycle-local) ; ok on Elisp, not on LaTeX


;; unified user interface for Emacs folding modes
(GNUEmacs
 (when (try-require 'fold-dwim)
   (global-set-key (kbd "C-c f t") 'fold-dwim-toggle)
   (global-set-key (kbd "C-c f h") 'fold-dwim-hide-all)
   (global-set-key (kbd "C-c f s") 'fold-dwim-show-all)))

(global-set-key (kbd "<S-f6>") 'visible-mode)



;;*** 29.9 (info "(emacs)TeX Mode")

;; - text-mode-hook     =  all text modes
;; - (la)tex-mode-hook  =  default Emacs built-in (La)TeX mode
;; - (La)TeX-mode-hook  =  AUCTeX

;; Note -- AUCTeX aliases tex-mode to TeX-mode
;; Note -- Invoking `(la)tex-mode' also runs `text-mode-hook'


;;**** 2 (info "(auctex)Installation") of AUCTeX

;; support for LaTeX documents
(GNUEmacs
  (message "29.9 TeX mode....AAA.AAA.AAA..............")
  (try-idle-require 'latex-XXX)
  (message "29.9 TeX mode....BBB.BBB.BBB..............")
  (eval-after-load 'latex
    '(progn

    ;; (try-require 'tex-site)
    ;; should not be used anymore with current AUCTeX releases


       ;; ;; LaTeX-sensitive spell checking
       ;; (add-hook 'tex-mode-hook
       ;;           (lambda ()
       ;;             (make-local-variable 'ispell-parser)
       ;;             (setq ispell-parser 'tex)))



;;**** 3 (info "(auctex)Quick Start")

    ;; Press `C-c C-c File RET RET' to run `dvips'
    ;; (note that the command is `File' and not `Dvips' as one might expect)

    ;; Press `C-c C-c Print RET RET' to run `GSview'
    ;; (also somewhat misleading name)

    ;; If you want to print the document, do it from GSview.


;;**** 5 (info "(auctex)Advanced Features")

;;***** 5.2 (info "(auctex)Completion")

    ;; if this is non-nil when AUC TeX is loaded, the TeX escape character `\'
    ;; will be bound to `TeX-electric-macro'
    (setq TeX-electric-escape t)

;;***** 5.4 (info "(auctex)Indenting")

    ;; leave the `tikzpicture' code unfilled when doing `M-q'
    (add-to-list 'LaTeX-indent-environment-list '("tikzpicture"))

    ;; number of spaces to add to the indentation for each `\begin' not
    ;; matched by a `\end'
    (setq LaTeX-indent-level 4)

    ;; number of spaces to add to the indentation for `\item''s in list
    ;; environments
    (setq LaTeX-item-indent 0)  ; -4

    ;; number of spaces to add to the indentation for each `{' not matched
    ;; by a `}'
    (setq TeX-brace-indent-level 0)  ; 4

    ;; auto-indentation (suggested by the AUCTeX manual -- instead of adding a
    ;; local key binding to `RET' in the `LaTeX-mode-hook')
    (setq TeX-newline-function 'newline-and-indent)


;;**** 6 Controlling Screen (info "(auctex)Display")

;;***** 6.1 (info "(auctex)Font Locking")

    ;; (for Org mode) add the `comment' environment to the variable
    ;; `LaTeX-verbatim-environments' so that, if the `#+TBLFM' line contains
    ;; an odd number of dollar characters, this does not cause problems with
    ;; font-lock in latex-mode
    (add-to-list 'LaTeX-verbatim-environments "comment")
;;;     (add-to-list 'LaTeX-verbatim-environments "mcnuweb") ; FIXME Does not work in .nw files


;;**** 7 (info "(auctex)Running TeX and friends") Processors, Viewers and Other Programs

;;***** 7.1 Executing (info "(auctex)Commands")

    ;; use PDF mode by default (instead of DVI)
    (setq-default TeX-PDF-mode t)

;;***** 7.2 (info "(auctex)Viewing") the formatted output

    ;; use a saner PDF viewer (evince, SumatraPDF)
    (setcdr (assoc "^pdf$" TeX-output-view-style)
            (cond (running-ms-windows
                   '("." "\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" %o"))
                         ; under Windows, we could open the PDF file with:
                         ; start "" xxx.pdf
                  (t
                   '("." "evince %o"))))

    ;; A decent viewer reloads the PDF automatically when the file has changed
    ;; while staying on the same page (no need to close & reopen).

    ;; Support for forward search with PDF files was added. That means the
    ;; viewer jumps to the page in the output file corresponding to the
    ;; position in the source file. Currently this only works if you use the
    ;; pdfsync LaTeX package and xpdf or SumatraPDF as your PDF viewer.

;;***** 7.3 (info "(auctex)Debugging") Catching the errors

    ;; don't show output of TeX compilation in other window
    (setq TeX-show-compilation nil)


;;**** 8 (info "(auctex)Multifile") Documents

    ;; AUC TeX will will assume the file is a master file itself
    (setq-default TeX-master t)


;;**** 9 Automatic (info "(auctex)Parsing Files")

    ;; enable parse on load (if no style hook is found for the file)
    (setq TeX-parse-self t)

    ;; enable automatic save of parsed style information when saving
    ;; the buffer
    (setq TeX-auto-save t)


;;**** 11 (info "(auctex)Automatic") Customization

;;***** 11.1 (info "(auctex)Automatic Global") Customization for the Site

    ;; directory containing automatically generated TeX information. Must end
    ;; with a slash
    (setq TeX-auto-global
          "~/.emacs.d/auctex-auto-generated-info/")

;;***** 11.3 (info "(auctex)Automatic Local") Customization for a Directory

    ;; directory containing automatically generated TeX information. Must end
    ;; with a slash
    (setq TeX-auto-local
          "~/.emacs.d/auctex-auto-generated-info/")


    ;; (try-require 'beamer)

    (try-require 'babel)



    ;; minor mode with distinct support for `\label', `\ref', `\cite' and
    ;; `\index' in LaTeX
    (when (try-require 'reftex)
        ;; A Table of Contents of the entire (multifile) document with
        ;; browsing capabilities is available with `C-c ='.
        ;; Hitting `l' there will show all the labels and cites.
        ;;
        ;; Labels can be created with `C-c (' and referenced with `C-c )'.
        ;; When referencing, you get a menu with all labels of a given type
        ;; and context of the label definition. The selected label is
        ;; inserted as a `\ref' macro.
        ;;
        ;; Citations can be made with `C-c [' which will use a regular
        ;; expression to pull out a *formatted* list of articles from your
        ;; BibTeX database. The selected citation is inserted as a `\cite'
        ;; macro.
        ;;
        ;; Index entries can be made with `C-c /' which indexes the word at
        ;; point or the current selection. More general index entries are
        ;; created with `C-c <'. `C-c >' displays the compiled index.
        (add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode

        ;; turn all plug-ins on
        (setq reftex-plug-into-AUCTeX t)

        ;; use a separate selection buffer for each label type -- so the menu
        ;; generally comes up faster
        (setq reftex-use-multiple-selection-buffers t))


    ;; Remap default face in current buffer (Emacs 23)
;;;     (add-hook 'LaTeX-mode-hook
;;;               '(lambda ()
;;;                  (face-remap-set-base
;;;                   'default
;;;                   '(:family "LMRoman10" :height 100 :background "white"))))


;;*** (info "(preview-latex)Top")

    (add-hook 'LaTeX-mode-hook 'LaTeX-preview-setup)
    (autoload 'LaTeX-preview-setup "preview")

    ;; how to call gs for conversion from EPS
    (setq preview-gs-command
          (cond (running-ms-windows
                 "C:/Program Files/gs/gs8.64/bin/gswin32c.exe")
                (t
                 "/usr/bin/gs")))
    (my-file-executable-p preview-gs-command)

    ;; scale factor for included previews
    (setq preview-scale-function 1.2)



    (GNUEmacs
    ;; major mode to edit nuweb files with AUCTex
    (when (try-require 'nuweb)  ;; depends on `TeX-lisp-directory'

        ;; define what's needed to properly call nuweb
        (make-variable-buffer-local 'outline-prefix-char)
        (make-variable-buffer-local 'outline-regexp)
        (make-variable-buffer-local 'outline-level-function)

        ;; our version of nuweb knows about `@%' comments
        (setq nuweb-comment-leader "@%")

        ;; major mode
        (add-to-list 'auto-mode-alist '("\\.w$" . nuweb-mode))

        ;; to get a menu with scraps/files/index entries
        (add-hook 'nuweb-mode-hook
                  (lambda()
                    (imenu-add-to-menubar "Nuweb")))

        ;; recompute all the defs and uses point in the current file
        (add-hook 'nuweb-mode-hook 'nuweb-compute-d-u)

        ;; replace the existing `Web' command in order to use PDF mode by
        ;; default (instead of DVI) -- without writing explicitly the entire
        ;; `TeX-command-list' in the `.emacs' file (as `customize-variable'
        ;; would do):
        (setcdr (assoc "Web" TeX-command-list)
                '("nuweb %s && pdflatex \"\\nonstopmode\\input{%s}\""
                  TeX-run-LaTeX nil t
                  :help "Extract files, create LaTeX document, and run `pdflatex' on it"))))

    ;; Tangle = Extract

    ;; FIXME Noweb -- Problem with multi-mode?
    (try-require 'noweb)

    )))



;;*** (info "(emacs-goodies-el)boxquote")

;; quote text with a semi-box
(when (try-require 'boxquote)

    ;; put spaces before my boxquotes
    (setq boxquote-top-corner    "    ,")
    (setq boxquote-side          "    | ")
    (setq boxquote-bottom-corner "    `")

    (global-set-key (kbd "C-c b r") 'boxquote-region)
    (global-set-key (kbd "C-c b t") 'boxquote-title))

;; --8<---------------cut here---------------start------------->8---
;; In Gnus, you can mark some region with enclosing tags by pressing
;; `C-c M-m' (`message-mark-inserted-region') or by clicking on
;; `<menu-bar> <Message> <Insert Region Marked>'.
;; --8<---------------cut here---------------end--------------->8---

;; interface to the festival speech synthesizer system
(when (locate-library "festival-XXX")
    (autoload 'say-minor-mode "festival" "Menu for using Festival." t)
    (say-minor-mode t)
    (setq auto-mode-alist
          (append '(("\\.festivalrc$" . scheme-mode)) auto-mode-alist))

    (setq festival-program-name "/usr/bin/festival"))

;; phonetic spelling
(try-idle-require 'phonetic)

(message "29 Commands for Human Languages... Done"))


;;** 30 Editing (info "(emacs)Programs")

(when section-programs (message "30 Editing Programs...")

;;*** 30.1 Major Modes for (info "(emacs)Program Modes")

(autoload 'awk-mode "cc-mode" "Awk editing mode." t)
;; TODO Or use a new AWK Mode for AWK files, rather than the older mode
;; contained in the file `awk-mode.el'
;; [from http://people.smu.edu/zwang/awk-mode.html]

;; (try-require 'graphviz-dot-mode)

;; Have a look at:
;; - http://cedet.sourceforge.net for C/C++ development,
;; - http://common-lisp.net/project/slime for Common Lisp development,
;; - http://jdee.sunsite.dk/ for Java programs.

;; Emacs tool for ELISP code analysis (to keep overview of the
;; function calls and dependecies between functions/variables):
;; byte-compile-generate-call-tree
;; Also http://whome.phys.au.dk/~harder/who-calls.el


;;*** 30.3 (info "(emacs)Program Indent")ation

;; From (info "(ccmode)Indentation Commands"):
;;    Changing the "hanginess" of a brace and then reindenting, will not
;;    move the brace to a different line. For this, you're better off
;;    getting an external program like GNU `indent', which will rearrange
;;    brace location, amongst other things.

;; turn on auto-fill mode in Lisp modes
(add-hook 'lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)

;; use one of several different indentation styles for C-like modes
(setq c-default-style
      '((awk-mode . "stroustrup")
        (other . "stroustrup")))
                                        ; Try the different pre-defined styles
                                        ; of indentation via a call to
                                        ; `c-set-style'

(defun my-c-mode-hook ()
  "Customize my c/c++-mode and awk-mode."
  ;; auto-indentation
  (local-set-key (kbd "<return>") 'newline-and-indent)  ; (control m)
  (local-set-key (kbd "<linefeed>") 'newline))          ; (control j)

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c-mode-hook)
(add-hook 'awk-mode-hook 'my-c-mode-hook)

(defun back-to-indentation-or-beginning ()
  (interactive)
  (if (/= (point) (line-beginning-position))
      (beginning-of-line)
    (back-to-indentation)))

(defun align-with-spaces (beg end)
  "Align selected using only spaces for whitespace."
  (interactive "r")
  (let ((indent-tabs-mode nil))
    (align beg end)))


;;*** 30.4 Commands for Editing with (info "(emacs)Parentheses")

;; find matching parenthesis (% command in vim)
(defun match-paren (arg)
  "Go to the matching parenthesis, if on parenthesis; otherwise,
insert `%'."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "%") 'match-paren)

;; highlight matching parenthesis
(when (try-require 'paren)
    (GNUEmacs
        (show-paren-mode t)
        (setq show-paren-ring-bell-on-mismatch t))
    (XEmacs
        (paren-set-mode 'paren)))

;; if the matching paren is offscreen, show the matching line in the echo area
;; + many other useful things
(when window-system
  ;; advanced highlighting of matching parentheses
  (when (try-require 'mic-paren)

      ;; activating
      (paren-activate)))

;; from hall@grumpy.nl.nuwc.navy.mil
;; goto-matching-paren


;;*** 30.6 (info "(emacs)Documentation") Lookup

;; show the function arglist or the variable docstring in the echo area
(GNUEmacs
 (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
 (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))


;;*** 30.7 (info "(emacs)Hideshow") minor mode

;; You can have block-oriented folding in programming modes: Hideshow
;; distinguishes end-of-block.

;; `hs-minor-mode.el' collapses code for a lot of languages, not only Lisp.

;; See `outline-minor-mode' as well.

;; enable `hs-minor-mode' at startup
(add-hook 'emacs-lisp-mode-hook
          (lambda () (hs-minor-mode 1)))


;; Especially after changing a couple of those really awkward keybindings
;; with `@' in the middle.
;; Changing: C-c @ c-s  to C-c s  (hs-show-block)
;;           C-c @ c-h  to C-c h  (hs-hide-block)
;; Seems not to collide with anything when in cperl-mode at least.


;; (define-key hs-minor-mode-map [?\C-c ?\C-\M-h] 'hs-hide-all)
;; (define-key hs-minor-mode-map [?\C-c ?\C-\M-s] 'hs-show-all)


;; (global-set-key (kbd "C-c @ @") 'hs-hide-all)
;; (global-set-key (kbd "C-c @ @") 'hs-show-all)
(global-set-key (kbd "C-c @ h") 'hs-hide-block)
(global-set-key (kbd "C-c @ s") 'hs-show-block)
(global-set-key (kbd "C-c @ SPC") 'hs-show-block) ; second binding


;;*** 30.8 (info "(emacs)Symbol Completion")

;; It's more or less a convention that each language mode binds its symbol
;; completion command to `M-TAB' which is a reserved hot key under Windows.
;; Way to solve this: when you hit `C-TAB', the command normally bound to
;; `M-TAB' will be called.
(global-set-key (kbd "<C-tab>")
                '(lambda ()
                   (interactive)
                   (call-interactively (key-binding (kbd "M-TAB")))))

;; `M-/' runs the command `dabbrev-expand' by default
;; Expand previous word "dynamically". Expands to the most recent, preceding
;; word for which this is a prefix.
(global-set-key (kbd "C-`") 'dabbrev-expand)

;; `C-M-/' runs the command `dabbrev-completion'
;; Completion on current word. Like `M-/' but finds all expansions in the
;; current buffer and presents suggestions for completion.

;; expand text trying various ways to find its expansion
(when (try-require 'hippie-exp)

    ;; list of expansion functions tried (in order) by `hippie-expand'
    (setq hippie-expand-try-functions-list
          '(try-expand-dabbrev   ; from current buffer
            try-expand-dabbrev-visible   ; from visible parts of all windows
            try-expand-dabbrev-all-buffers   ; from all other buffers
            try-expand-dabbrev-from-kill
            try-complete-file-name-partially
            try-complete-file-name
            try-expand-all-abbrevs
            try-expand-list
            try-expand-line
            try-complete-lisp-symbol-partially
            try-complete-lisp-symbol
            try-expand-whole-kill))

    ;; expand-function
    (defun my-hippie-expand (arg)
      ;; called with a positive prefix `P', it jumps directly to the `P'-th
      ;; `try-function'
      (interactive "P")
      ;; `hippie-expand' does not have a customization-feature (like
      ;; `dabbrev-expand') to search case-sensitive for completions. So we
      ;; must set `case-fold-search' temporarily to nil!
      (let ((old-case-fold-search case-fold-search))
        (setq case-fold-search nil)
        (hippie-expand arg)
        (setq case-fold-search old-case-fold-search)))

    (global-set-key [(control tab)] 'my-hippie-expand))

;; (global-set-key (kbd "M-/") 'hippie-expand)

;; I recommend you split the key binding of those two command.
;; I binding TAB yas/expand, and binding M-/ hippie-expand.
;; So yas/expand don't conflict with hippie/expand.

;; predictive abbreviation expansion ("à la IntelliSense")
(when (try-require 'pabbrev)

  ;; don't print messages while scavenging on idle timer
  (setq pabbrev-idle-timer-verbose nil)

  ;; tab completion with continual, as-you-type feedback
  (global-pabbrev-mode))

;; > I'm trying to have code completion in Emacs, but i don't know what to
;; > do. In eclipse, when we writing a java code line, for example:
;; > System.out., we do C^SPACE to show a window with several methods
;; > associated (printl, print,etc).
;; > I would like to have something similar in Emacs. Can anybody help me?
;; Try M-TAB with cursor on the symbol; is that what you are looking for?


;; extensible inline text completion mechanism -- really brilliant!
(when (try-require 'company)
  (define-key company-mode-map (kbd "M-SPC") 'company-complete)

  (defun my-turn-on-company-mode ()
    (interactive)
    (company-mode 1))

  (dolist (hook (list
                 'asm-mode-hook
                 'c++-mode-hook
                 'c-mode-hook
                 'clojure-mode-hook
                 'emacs-lisp-mode-hook
                 'emms-tag-editor-mode-hook
                 'haskell-mode-hook
                 'java-mode-hook
                 'lisp-interaction-mode-hook
                 'lisp-mode-hook
                 'scheme-mode-hook
                 'sh-mode-hook
                 'slime-repl-mode-hook))
    (add-hook hook 'my-turn-on-company-mode))

  (setq company-idle-delay nil)

  (setq company-eclim-auto-save t)

  (setq company-eclim-executable
        "~/opt/eclipse/plugins/org.eclim_1.4.5/bin/eclim")

  (defun my-java-mode-init ()
    (setq company-backend 'company-eclim))

  (add-hook 'java-mode-hook 'my-java-mode-init))


;;*** 30.9 (info "(emacs)Glasses") minor mode

;; face to be put on capitals of an identifier looked through glasses
(setq glasses-face 'bold)

;; string to be displayed as a visual separator in unreadable identifiers
(setq glasses-separator "")


;;*** Add-Ons

;; XXX semanticdb-project-database-file ??

;; Collection of Emacs Development Environment Tools

;; ;; list of directories, where each directory is the root of some project
;; (setq semanticdb-project-roots '("~/emacs/site-lisp"))

;; turn on all "useful" features
(setq semantic-load-turn-useful-things-on t)

;; setup complete development environment
(GNUEmacs
  (try-idle-require 'cedet)
  (eval-after-load 'cedet
    '(progn

    ;; Enabling various SEMANTIC minor modes.  See semantic/INSTALL for more
    ;; ideas.
    ;; Select one of the following:

    ;; - This is the default. Enables the database and idle reparse engines
    (semantic-load-enable-minimum-features)

    ;; - This enables some tools useful for coding, such as summary mode imenu
    ;;   support, and the semantic navigator                               <<<
    (semantic-load-enable-code-helpers)

    ;; - This enables even more coding tools such as the nascent IntelliSense
    ;;   mode decoration mode, and stickyfunc mode (plus regular code helpers)
    (semantic-load-enable-guady-code-helpers)

    ;; - This turns on which-func support (plus all other code helpers)
    (semantic-load-enable-excessive-code-helpers)

    ;; ;; This turns on modes that aid in writing grammar and developing
    ;; ;; semantic tool. It does not enable any other features such as code
    ;; ;; helpers above.
    ;; (semantic-load-enable-semantic-debugging-helpers)

    ;; getting rid of semantic.caches
    (setq semanticdb-default-save-directory "~/.emacs.d/semantic-cache")
    (my-make-directory-yes-or-no semanticdb-default-save-directory)

    ;; fix the max CPU problem on latest Emacs snapshots
    (setq semantic-idle-scheduler-idle-time 432000)

    ;; Emacs Code Browser (provides views of directories and files)
    ;; see http://platypope.org/yada/emacs-demo/
    (when (try-require 'ecb-autoloads)  ; load all available autoloads of ECB

        ;; ECB version
        (setq ecb-options-version "2.32")

        ;; don't show tip of the day at start time of ECB
        (setq ecb-tip-of-the-day nil)

        ;; toggle activation of ecb
        (global-set-key (kbd "C-c e") 'ecb-minor-mode)))))

;; If you've installed CEDET and ECB, EAssist is worth trying out:
;;     http://www.mail-archive.com/gnu-emacs-sources@gnu.org/msg00292.html
;;
;; It uses CEDET to provide a handy symbols browser for the current file, that
;; narrows down the list as you type substrings. Tastes differ, but I for one
;; really like this.

(message "30 Editing Programs... Done"))


;;** 31 (info "(emacs)Building") Compiling and Testing Programs ]

(when section-building (message "31 Compiling and Testing Programs...")

;; >> It's possible to see, while we are programming, if we did a mistake. In
;; >> eclipse, when we do an error, for example, forget a ; , an underline
;; >> appears in the line indicating that something is wrong. It's possible to
;; >> have something like this in Emacs?
;; >
;; > There's a CWarn mode for C and C++, but I don't know about similar
;; > features for Java.  Anyone?

;; flymake can compile in the background and colorize lines with
;; errors/warnings
;; http://flymake.sourceforge.net/
;; http://www.emacswiki.org/cgi-bin/wiki/JdeeFlymake

(when (try-require 'flymake)

    ;; Setting up flymake
    (defun activate-flymake ()
      "Activates flymake when real buffer and you have write access"
      (if (and (buffer-file-name) (file-writable-p buffer-file-name))
          (flymake-mode t)))

    ;; Adding errors to modeline
    ;; With this the error output of othe current line will appear right below
    ;; in the modeline
    (defun my-flymake-show-help ()
      (when (get-char-property (point) 'flymake-overlay)
        (let ((help (get-char-property (point) 'help-echo)))
          (if help (message "%s" help)))))
    (add-hook 'post-command-hook 'my-flymake-show-help))


;; my build command: `cd /path/to/Makefile && make -f Makefile'



;;*** 31.1 Running (info "(emacs)Compilation")s under Emacs

;; http://www.emacswiki.org/emacs-en/eproject
;;
;; It allows to define projects, and in each project to define menu commands
;; and shortcut keys as you like. For example:
;;
;; make (f9)               : `-in src make' OR `make'
;; clean (C-f9)            : `rm -vf src/emacs-23.* etc/DOC* && make clean' OR `make clean'
;; run (f8)                : `src/emacs' OR `./my-program'
;; stop (C-f8)             : `-e kill-compilation'
;; ---
;; configure               : `./configure'
;; install                 : `echo root-pass | sudo -S make install'


;; You don't need a Makefile to perform simple tasks, because Make knows a
;; lot of built in rules out of the box. For example, to compile a `.c'
;; source file `foo.c' into a program `foo', all you need is say
;; "make -k foo", and Make will do it even without a Makefile.

;; invoke a compiler with the same command as in the last invocation of
;; `compile'
(global-set-key (kbd "<f9>") 'recompile)

;; scroll the `*compilation*' buffer window to follow output as it appears
(setq compilation-scroll-output t)

;; number of lines in a compilation window
(setq compilation-window-height (* 2 5))

;; ;; I also don't like that the compilation window sticks around after a
;; ;; successful compile. After all, most of the time, all I care about
;; ;; is that the compile completed cleanly. Here's how I make the
;; ;; compilation window go away, only if there was no compilation
;; ;; errors:
;; (setq compilation-finish-function
;;       (lambda (buf str)
;;         (if (string-match "exited abnormally" str)
;;             ;; there were errors
;;             (message "Compilation errors, press C-x ` to visit")
;;           ;; no errors, make compilation window go away in 0.5 sec
;;           (run-at-time 0.5 nil 'delete-windows-on buf)
;;           (message "NO COMPILATION ERRORS!"))))


(GNUEmacs
    (defun cc-goto-first-error( buffer exit-condition )
      (with-current-buffer buffer
        (goto-char (point-min))
        (compilation-next-error 1)))

    (add-to-list 'compilation-finish-functions 'cc-goto-first-error))


;; (add-hook 'c-mode-hook
;; 	  (lambda ()
;; 	    (set (make-local-variable 'compile-command)
;; 		 (format "make %s"
;; 			 (file-name-sans-extension
;; 			  (file-name-nondirectory buffer-file-name))))))
;;
;; Just set the CC=gcc and CFLAGs="-Wall -O3" environment variables, and
;; voila!

(defvar make-clean-command "make clean all"
  "*Command used by the `make-clean' function.")

(defun make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (require 'compile) ;; needed for compile-internal
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (compile-internal make-clean-command "No more errors"))

(global-set-key (kbd "<S-f9>") 'make-clean)


;;*** 31.2 (info "(emacs)Compilation Mode")

;; display the next compiler error message
(global-set-key (kbd "<f10>") 'next-error)

;; display the previous compiler error message
(global-set-key (kbd "<S-f10>") 'previous-error)

;; display the first compiler error message
(global-set-key (kbd "<C-f10>") 'first-error)

;; highlight and parse the whole compilation output as soon as it arrives
(setq compile-auto-highlight t)


;; Some code that will make it so the background color of the lines that gcc
;; found errors on, should be in another color.

(defvar all-overlays ())

(defun delete-this-overlay (overlay is-after begin end &optional len)
  (delete-overlay overlay))

(defun highlight-current-line ()
  (interactive)
  (setq current-point (point))
  (beginning-of-line)
  (setq beg (point))
  (forward-line 1)
  (setq end (point))
  ;; Create and place the overlay
  (setq error-line-overlay (make-overlay 1 1))

  ;; Append to list of all overlays
  (setq all-overlays (cons error-line-overlay all-overlays))

  (overlay-put error-line-overlay
               'face '(background-color . "pink"))
  (overlay-put error-line-overlay
           'modification-hooks (list 'delete-this-overlay))
  (move-overlay error-line-overlay beg end)
  (goto-char current-point))

(defun delete-all-overlays ()
  (while all-overlays
    (delete-overlay (car all-overlays))
    (setq all-overlays (cdr all-overlays))))

(defun highlight-error-lines (compilation-buffer, process-result)
  (interactive)
  (delete-all-overlays)
  (condition-case nil
      (while t
        (next-error)
            (highlight-current-line))
    (error nil)))

(setq compilation-finish-function 'highlight-error-lines)


;;*** 31.4 (info "(emacs)Grep Searching") under Emacs

;; ignore case distinctions in the default grep command
;;(if (my-file-executable-p "~/bin/wcgrep")
(setq grep-command "grep -n -i -e ")

;; grep + emacs 22 + cygwin does not follow file links
;; try adding "-nH" to your grep options.

;; The commands lgrep and rgrep are somehow more user-friendly than the M-x
;; grep command. The word at point can be captured using the command
;; (thing-at-point 'word). So you may try:
;;
;; (defun my-grep ()
;;   "look for word at point in files ending by .cpp and .h
;;    recursively starting from the work directory"
;;   (interactive)
;;   (rgrep (thing-at-point 'word) "*.cpp *.h" "~/work"))
;;
;; (global-set-key [(control shift f)] 'my-grep)


;;*** 31.6 Running (info "(emacs)Debuggers") Under Emacs

;; > Enable debug-on-error via 'M-x toggle-debug-on-error', then start
;; > flyspell-mode again and examine the error. If that does not work, try
;; > edebug. Open the file where flyspell-mode is defined. Reeval the
;; > function with 'C-u C-M-x' and again, start flyspell-mode. Now you are
;; > in edebug-mode. Hit Space till you get the error. Press 'i' to enable
;; > debugging of the called function after point.
;;
;; The cursor has to be inside the flyspell-mode function for this to work.
;; (C-M-x evals the current function , with prefix it also installs the
;;        debug routines.)
;; Alternatively this should enable edebug on all forms in the current buffer:
;; M-x edebug-all-defs
;; M-x eval-buffer


;;**** Debugging Mercury programs

;; 1. Put these lines in your .emacs file:

;; (setq mercury-dir (getenv "MERCURY_DIR"))
;; (load-file (concat mercury-dir "/lib/mercury/elisp/gud.el"))
;; (setq mdb-command-name "bash.exe mdb ./mas_server.exe
;;  -c ../online/mas_server/mas_config_local.xml -d ../data"))

;; 2. To start the debugger, open a file in your build directory,
;;    e.g. build/Makefile

;; 3. Run M-x and then type mdb

;; 4. At the prompt you should see the command from the .emacs file:
;; "bash.exe mdb ./mas_server.exe
;;  -c ../online/mas_server/mas_config_local.xml -d ../data"

;; Change if necessary and hit the `RET' key

;; 5. Find your bugs.

;; Known problems:
;;   - tab completion doesn't work


;;**** Debugging Lisp programs

;; Emacs has the basic debugger/stack trace, but it also has the edebug
;; facility, which is very powerful, for the more complex situation.

;; , (info "(elisp)Edebug") ]
;; | Edebug is a source-level debugger for Emacs Lisp programs with which
;; | you can:
;; |
;; |    * Step through evaluation, stopping before and after each expression.
;; |
;; |    * Set conditional or unconditional breakpoints.
;; |    [...]
;; `----

;; You can cause the debugger to be called at a certain point in your program
;; by writing the expression `(debug)' at that point. To do this, visit the
;; source file, insert the text `(debug)' at the proper place, and type
;; `C-M-x'.

;; `c'  Exit the debugger and continue execution
;; `d'  Continue execution, but enter the debugger the next time any Lisp
;;      function is called.

(define-key emacs-lisp-mode-map (kbd "C-x x") 'edebug-eval-top-level-form)
(define-key emacs-lisp-mode-map (kbd "C-x x") 'edebug-defun) ; other binding onto same key

(autoload 'edebug-eval-top-level-form "edebug")

(setq edebug-global-prefix "\C-xX")

(add-hook 'cl-load-hook
          (lambda ()
            (add-hook 'edebug-setup-hook
                      (lambda ()
                        (load-library "cl-specs")))))

;; toggle whether to enter Lisp debugger when an uncaught error is signaled
;; (global-set-key [(super c) (d)] 'toggle-debug-on-error)


;;*** 31.7 (info "(emacs)Executing Lisp") Expressions

(require 'lisp-mode)

;; nuke and reevaluate an elisp buffer
(try-require 'nukneval)

(add-hook 'emacs-lisp-mode-hook 'my-elisp-extra-keys)
(defun my-elisp-extra-keys ()
  ;; auto-indentation
  (define-key emacs-lisp-mode-map "\C-m" 'newline-and-indent)
  (define-key emacs-lisp-mode-map "\C-cc" 'nuke-and-eval))


;;*** 31.9 (info "(emacs)Lisp Eval") Expressions

;; enable the use of the command `eval-expression' without confirmation
(put 'eval-expression 'disabled nil)

;; enhanced eval-expression command
(when (try-require 'eval-expr)
    (eval-expr-install))


;;*** 31.10 (info "(emacs)Lisp Interaction") Buffers

;; to evaluate a non-interactive command, simply use IELM!

;; interaction mode for Emacs Lisp
(autoload 'ielm "ielm" "Start an inferior Emacs Lisp session" t)


;;*** 31.11 Running an (info "(emacs)External Lisp")

;; Just as in C, C++, Java, Perl, Python, etc, Lisp code is kept in files. All
;; the normal editing operations are performed on files. In this respect,
;; hacking in Lisp is like hacking in any other language that you are used to.
;; What's different is that what you are hacking is a running Lisp program.
;; When you edit a function definition or add a new one, you compile it into a
;; running program. There is no compile, link, run, debug cycle as you know it
;; from C or Java.
;;
;; Ponder that for a minute.
;;
;; When you fix a bug in a C function, you have to recompile, relink, and
;; reload your program before you can test the fix. You don't do that in Lisp.
;; You make the fix and then go straight to testing it. This process can be
;; even faster than fixing a bug in a scripting language like Perl.

;; see http://svn.peadrop.com/emacs/lisp/lisp-config.el

;; superior Lisp inferior mode extension
(try-idle-require 'slime)
(eval-after-load 'slime
  '(progn

    ;; indentation
    (slime-setup)

    (add-hook 'lisp-mode-hook
              (lambda ()
                (slime-mode t)))
    (add-hook 'inferior-lisp-mode-hook
              (lambda ()
                (inferior-slime-mode t)))

    ;; Gnu CLISP - Inferior Lisp Mode & ILISP (switches for ANSI & no banner)
    ;; TODO Have a look at SBCL
    (defvar clisp-dir
      (cond (running-ms-windows
             "e:/vmware-home/bin/win32/clisp-2.31/full/")
            (t
             "/usr/bin")))

    (defvar clisp-exe
      (cond (running-ms-windows
             (concat clisp-dir "lisp.exe"))
            (t
             (concat clisp-dir "/" "clisp"))))

    ;; optionally, specify the Lisp program you are using. Default is "lisp".
    ;; include the full linking set with `-K full'
    (setq inferior-lisp-program
          (cond (running-ms-windows
                 (concat clisp-exe
                         " -B " clisp-dir
                         " -M " clisp-dir "lispinit.mem"
                         " -ansi -q"))
                (t
                 (concat clisp-exe
                         " -B " clisp-dir
                         " -ansi -q"))))  ;; "clisp -K full"

    ;; connect automatically to my Lisp when opening a Lisp file
    (defun cliki:start-slime ()
      (unless (slime-connected-p)
        (save-excursion (slime))))
    (add-hook 'slime-mode-hook 'cliki:start-slime)

    ;; automatically show documentation for code near the point
    (add-hook 'slime-mode-hook
              (lambda ()
                (slime-autodoc-mode t)))

;;   ;; GNU CLISP - http://clisp.cons.org/
;;   (defun clisp-start ()
;;     (interactive)
;;     (shell-command (concat "c:/bin/clisp-2.32/full/lisp.exe "
;;                            "-B c:/bin/clisp-2.32/full/ "
;;                            "-M c:/bin/clisp-2.32/full/lispinit.mem "
;;                            "-i c:/usr/home/.slime.lisp "
;;                            "-ansi -q&"))))


; Functions and key bindings for getting Emacs to interact with GCL.
; Thomas R. Ioerger, Dept of Computer Science, Texas A&M University
; see http://www.cs.tamu.edu/faculty/ioerger/emacs-gcl.html for more details

(global-set-key "\C-t" '(make-keymap))

(defun run-gcl ()
  (interactive)
  (split-window)
  (other-window 1)
  (inferior-lisp "gcl"))

(defun gcl-debug-quit ()
  (interactive)
  (comint-send-string "*inferior-lisp*" ":q\C-M"))

(defun gcl-quit ()
  (interactive)
  (comint-send-string "*inferior-lisp*" "(bye)\C-M"))

(defun gcl-eval-buffer ()
  (interactive)
  (set-mark 0)
  (goto-char (point-max))
  (lisp-eval-region 1 (point))
  (exchange-point-and-mark))

(global-set-key "\C-tl" 'run-gcl)
(global-set-key "\C-te" 'lisp-eval-defun)
(global-set-key "\C-tw" 'switch-to-lisp) ; split screen!
(global-set-key "\C-tq" 'gcl-debug-quit)
(global-set-key "\C-tb" 'gcl-eval-buffer)
(global-set-key "\C-tx" 'gcl-quit)

; commands (after prefix of control-t)
; l = start lisp
; e = eval current expression
; w = switch to lisp buffer
; q = quit from debugger back to top-level
; b = eval buffer
; x = kill lisp process

))

(message "31 Compiling and Testing Programs... Done"))


;;** 32 (info "(emacs)Maintaining") Programs

(when section-maintaining (message "32 Maintaining Programs...")

;;*** 32.1 (info "(emacs)Version Control")

;; PCL-CVS
(when (try-require 'pcvs-XXX)

  ;; allow commit on whole directories
  (setq cvs-allow-dir-commit t)

  ;; when to reuse an existing cvs buffer
  (setq cvs-reuse-cvs-buffer 'always)  ;; subdir

  ;; examine
  (global-set-key (kbd "C-x v e") 'cvs-examine)

  ;; examine without asking for a directory
  (global-set-key (kbd "<C-f9>")
                  '(lambda ()
                     (interactive)
                     (cvs-examine (file-name-directory (buffer-file-name))
                                  nil)))

  ;; messages that should be ignored by the parser
  ;; TODO Should only ADD the last one to the default value of cvs-parse-...
  (setq cvs-parse-ignored-messages
        '("Executing ssh-askpass to query the password.*$"
          ".*Remote host denied X11 forwarding.*$"
          ".*-m wrapper option is not supported remotely.*$"))

  ;; change the CVS Id marker to reflect that a source file was edited
  ;; (from Brady Montz)
  (defun my-mark-cvs-modified ()
    "Called when a file has changed. Updates any RCS Id and Header keywords it
    finds to show that the file is modified."
    (let ((buffer-undo-list t))         ; don't let this change get into the
                                        ; undo list because of this, we must
                                        ; ensure that the edit is in-place,
                                        ; and doesn't move any text
      (when (and (buffer-modified-p) (boundp 'vc-mode) vc-mode)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward
                  (concat "\\(\\$\\(?:Id\\|Header\\): "
                          "[^\"'#;$]* \\)\\(Exp \\$\\)")
                  nil t)
            (replace-match "\\1Mod $" t))))))

  (defadvice basic-save-buffer (before my-basic-save-buffer first activate)
    (my-mark-cvs-modified))

  (defun run (command &optional to-buffer)
    "A variation of shell-command.
    With no optional argument this runs the command creating
    a special buffer to put the output in. The buffer is named
    after the first word in the command.

    The optional argument to-buffer allows the target
    buffer to be specified.

    With the interactive-prefix the target buffer is the
    current buffer (as in shell-command)."
    (interactive (list (read-from-minibuffer "Shell command: "
                                             nil nil nil 'shell-command-history)
                       current-prefix-arg))
    (shell-command command (or to-buffer
                               (get-buffer-create
                                (car (split-string command " "))))))

  (defun run-eval (command &optional func)
    "Evaluate the shell command optionally passing results to a function.
    Without the optional func this returns the result of
    running the command, as a string.

    With the function the results of the shell command are passed as
    a string to the function, the value of calling the function is
    returned.

    If you supply func then it must either be a function taking
    one string argument or a string which can be evaluated to a
    function taking one string argument.

    Interactively the prefix argument will cause a function to be
    prompted for."
    (interactive (list (read-from-minibuffer "Shell command: "
                                             nil nil nil 'shell-command-history)
                       (if current-prefix-arg
                           (read-from-minibuffer "Function: "))))
    (with-temp-buffer
      ;; This turns off the open window behavior of shell-command
      (let ((pop-up-windows nil))
        (shell-command command (current-buffer)))
      (let ((str (buffer-substring-no-properties (point-min)
                                                 (- (point-max) 1))))
        (cond
         ((functionp func)
          (funcall func str))
         ((stringp func)
          (funcall (eval (read func)) str))
         ('t
          str)))))

  (defun map-files (thunk filename-list)
    "Read in each file as a buffer and execute thunk on them.
    If any file does not already exist in the buffer list then that
    buffer is destroyed after thunk has been executed.

    If filename-list is a list then it's used directly, if it's
    a string we run string-to-words on it."
    (mapcar (lambda (filename)
              (if (not (get-file-buffer filename))
                  (let ((buf (find-file filename)))
                    (with-current-buffer buf
                      (funcall thunk)
                      (kill-buffer buf)))
                (with-current-buffer (get-buffer filename)
                  (funcall thunk))))
            (if (listp filename-list)
                filename-list
              (split-string filename-list))))

  ;; switch the entire module from one location to another, using the same
  ;; code base when being at different physical sites
  (defun my-cvs-hack ()
    "Toggle the CVS between local and remote"
    (interactive)
    (run-eval "find . -name 'Root'"
              (lambda (list-of-files)
                (map-files (lambda ()
                             (if (re-search-forward ":localhost:" nil 't)
                                 (replace-match ":rawls:")
                               (progn
                                 (re-search-forward ":rawls:" nil 't)
                                 (replace-match ":localhost:")))
                             (save-buffer))
                           list-of-files)))))

;; Unmodified-according-to-VC buffers use "-" as a separator in their VC
;; indicator, and modified buffer have ":" (e.g., "CVS-1.2" vs. "CVS:1.2").
;; The tooltip over the VC indicator also says more explicitly.

(GNUEmacs
 ;; Subversion
 (when (try-require 'psvn)

   ;; `svn-status-property-edit-svn-ignore' (`P TAB') allows user to edit
   ;; list of files ignored by Subversion

   ;; hide unmodified files
   (setq svn-status-hide-unmodified t)

   ;; use longer phrases
   (setq svn-status-short-mod-flag-p nil)

   ;; delete temporary files
   (setq svn-status-ediff-delete-temporary-files t)

   ;; show the diff we are about to commit
   (define-key svn-log-edit-mode-map (kbd "<f6>") 'svn-log-edit-svn-diff)

   ;; examine
   (global-set-key (kbd "C-x v e") 'svn-status)

   ;; examine without asking for a directory
   (global-set-key (kbd "<C-f9>")
                   '(lambda ()
                      (interactive)
                      (svn-status (file-name-directory (buffer-file-name))
                                  nil)))

   (defun my-svn-log-edit-mode-setup ()
     (setq ispell-local-dictionary "en_US")
     (flyspell-mode))

   (add-hook 'svn-log-edit-mode-hook 'my-svn-log-edit-mode-setup)))


(when (try-require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status))



;;*** 32.2 (info "(emacs)Change Log")s

;; don't make a new entry, when the last entry was made by you and on the same
;; date
(setq add-log-always-start-new-record nil)

;; adds the file's version number to the change log entry
(setq change-log-version-info-enabled t)


;;*** 32.3 (info "(emacs)Tags") Tables

;; Using tags tables is the most generic approach to setup code navigation.
;; Support for it has been in Emacs for a long time. Any installation of Emacs
;; should also come with the etags program, which supports many different
;; languages, compare:
;;
;; cscope
;;     C, C++
;;
;; global
;;     C, C++, Yacc, Java and PHP4
;;
;; etags (emacs 23)
;;     C, Objective C, C++, Java, Fortran, Ada, Cobol, Erlang,
;;     Forth, HTML, LaTeX, Emacs Lisp/Common Lisp, Lua, Makefile,
;;     Pascal, Perl, PHP, Postscript, Python, Prolog, Scheme and
;;     most assembler-like syntaxes
;;
;; etags (exuberant)
;;     Asm, Asp, Awk, Basic, BETA, C, C++, C#, Cobol, Eiffel,
;;     Erlang, Fortran, HTML, Java, JavaScript, Lisp, Lua, Make,
;;     Pascal, Perl, PHP, Python, REXX, Ruby, Scheme, Sh, SLang,
;;     SML, SQL, Tcl, Vera, Verilog, Vim, YACC
;;
;; It doesn't do fancy stuff, e.g. keeping an index of function
;; references. That's the kind of thing gnu global and cscope can do for
;; you, if you're working with a language that they support.
;;
;; I should look at CEDET again, though. 8-)


;; By default, Emacs TAGS do not record positions where a function is _called_.
;; They record only positions where a function (or variable etc.) is _defined_.


;; First of all, you must build a `TAGS' file (which keeps the symbols from
;; your project, by scanning all the source and header files with the
;; `etags' command).

;; list of file names of tags tables to search
(setq tags-table-list
      '(
        "~/TAGS"
;;;         "/usr/share/texmf-texlive/tex/latex/TAGS"
        ))

;; For example, you can have a "make TAGS" Makefile target to do this for
;; you:
;;
;; TAGS:
;;      rm -f TAGS
;;      find $$(pwd) \( -name \*.el \
;;                   -o -name \*.[chCH] \
;;                   \) -print | etags -
;;
;; You can create a tags file by using `M-x compile RET tags RET'.

;; Alternatively,
(try-require 'sure-tags)
;; will make sure that tags file exists (and builds it if it doesn't),
;; allowing you to first rebuild the tags file or specify a new one when the
;; search fails.

;; After this, you can use a tags table with the command
;; `M-x visit-tags-table RET'.

;; You can search for *definitions* of tags that match your regexp, by using
;; `M-x find-tag' (bound to `M-.').
;; To continue searching for next alternate definition, use `C-u M-.'.
;; To jump back, use `M-*'.

(defun find-next-tag ()
  (interactive)
  (find-tag nil t))

;; select from multiple tags
(when (try-require 'etags-select)

    ;; do a `find-tag-at-point', and display all exact matches
    (global-set-key (kbd "M-?") 'etags-select-find-tag-at-point))

;; find the definition of the Emacs Lisp function or variable near point
(GNUEmacs
    (find-function-setup-keys))

;; You can search for *occurrences* of tags that match you regexp on all
;; files in tags table, by using `M-x tags-search RET'.
;; To continue searching for next match, use `M-,'.

;; There is a cscope interface for emacs. I recommend it. tags are
;; extremely limited imo. cscope does a much better, if slower, job.


;;*** 32.4 Merging Files with (info "(emacs)Emerge")

;; merge file diffs under Emacs control
(try-require 'emerge)

;; `M-x smerge-mode RET'
;; That does not automatically select regions but provides convenient key
;; bindings to navigate between conflicts and to choose the A or B variant

(message "32 Maintaining Programs... Done"))


;;** 33 (info "(emacs)Abbrevs")

(when section-abbrevs (message "33 Abbrevs...")

;; See (info "(autotype)") as well


;;*** 33.3 Controlling (info "(emacs)Expanding Abbrevs")

;; I am aware of packages such as `ELSE', `tempo', `skeleton', some of which
;; do similar things. However, I believe this package does some things
;; better than these other packages.
;; It's particularly notable for having the easiest syntax of all, making it
;; possible to add new snippets without careful programming, and also very
;; nice navigation within the inserted text.
;; Check out the demo: http://www.bloomington.in.us/~brutt/msf-abbrev.html
(GNUEmacs
    (when (try-require 'msf-abbrev)
        ;; ensure abbrev mode is always on
        (setq-default abbrev-mode t)

        ;; do not bug me about saving my abbreviations
        (setq save-abbrevs nil)

;;;         ;; load up modes I use
;;;         (require 'cc-mode)
;;;         (require 'perl-mode)
;;;         (require 'cperl-mode)
;;;         (require 'sh-script)
;;;         (require 'shell)
;;;         (require 'tex-site) ;; I use AUCTeX
;;;         (require 'latex)    ;; needed to define LaTeX-mode-hook under AUCTeX
;;;         (require 'tex)      ;; needed to define TeX-mode-hook under AUCTeX

        ;; load up abbrevs for these modes
        (require 'msf-abbrev)
        (setq msf-abbrev-verbose t) ;; optional
        (global-set-key (kbd "C-c l") 'msf-abbrev-goto-root)

        ;; FIXME Conflict with Org agenda `C-c a'
        (global-set-key (kbd "C-c a") 'msf-abbrev-define-new-abbrev-this-mode)
        (setq msf-abbrev-root (concat my-site-lisp-directory "mode-abbrevs/"))
        (when (file-directory-p msf-abbrev-root)
              (msf-abbrev-load))))

;; There're some difference from msf-abbrev. For example, msf-abbrev doesn't
;; support mirror fields and transformations. It also doesn't support multiple
;; snippet with same name.
(GNUEmacs
    (when (try-require 'yasnippet) ;; not yasnippet-bundle

      ;; do necessary initialization
      (yas/initialize)

      ;; root directory that stores the snippets for each major mode
      (setq yas/root-directory
            (concat my-site-lisp-directory "yasnippet/snippets"))
      (unless (file-directory-p yas/root-directory)
        (setq yas/root-directory
              (concat local-site-lisp-directory "yasnippet/snippets")))

      ;; load snippet definition from a (existing) directory hierarchy
      (when (file-directory-p yas/root-directory)
          (yas/load-directory yas/root-directory))

      ;; ;; the key to bind as a trigger of snippet
      ;; (setq yas/trigger-key (kbd "SPC"))  ; default is TAB
))

;; For simple expansions, I prefer abbrev. Think: pub -> public, pro ->
;; protected, pri -> private, etc.

;; For more complex templates, I use yasnippet. For example, new file
;; templates (where it's lisp evaluation is handy), class and function
;; templates with docblocks, etc. I use it to reduce the repetitious parts of
;; programming, and let me focus on getting things done.

;; yasnippet will also expand snippets containing non-word-constituent
;; characters, which abbrev can't. So I can't have "@p" expand to "@param"
;; with abbrev, but I can with yasnippet.


;;*** 33.7 (info "(emacs)Dabbrev Customization")

;; preserve case when expanding the abbreviation
(setq dabbrev-case-replace nil)

(message "33 Abbrevs... Done"))


;;** 37 (info "(emacs)Dired"), the Directory Editor

(when section-dired (message "37 Dired, the Directory Editor...")

;; FIXME Some people report sorting problems in Org agenda view because of
;; this package!
;; provide the same facility of `ls --color' inside Emacs
(try-require 'dircolors)

;; emulate insert-directory completely in Emacs Lisp
(when (try-require 'ls-lisp)

    ;; disable the case sensitive sort of file names
    (setq ls-lisp-ignore-case t)

    ;; sort directories first in any ordering
    (setq ls-lisp-dirs-first t)

    ;; use ISO 8601 dates (on MS-Windows)
    (setq ls-lisp-format-time-list
           '("%Y-%m-%d %H:%M"
             "%Y-%m-%d %H:%M"))

    ;; use localized date/time format
    (setq ls-lisp-use-localized-time-format t))


;; directory-browsing commands
(when (try-require 'dired)

;;*** 37.1 (info "(emacs)Dired Enter")

    ;; switches passed to `ls' for Dired
    ;; (setq dired-listing-switches "-h")
                                 ;; "-alt --time-style=long-iso")


;;*** 37.7 (info "(emacs)Operating on Files")

    ;; try to guess a default target directory
    (setq dired-dwim-target t)

    ;; enable the use of the command `dired-find-alternate-file'
    ;; without confirmation
    (put 'dired-find-alternate-file 'disabled nil)

    ;; recursive deletes allowed, after asking for each directory at top level
    (setq dired-recursive-deletes 'top)

    ;; copy recursively without asking
    (setq dired-recursive-copies 'always)


    ;; extra Dired functionality
    (try-require 'dired-x)
        ; You can jump to the Dired buffer corresponding to the current
        ; buffer by pressing `C-x C-j' (`dired-jump').
        ; If in Dired already, pop up a level and goto old directory's line.

        ; `dired-x' also has a feature to "guess" the right shell command and
        ; the right external viewer for documents (see
        ; `dired-guess-shell-alist-user')


    ;; On top of the traditional ways, there's also an add-on called Extview
    ;; which opens files using outside programs, such as XPDF, based on their
    ;; extension. It does this both from Dired and with `find-file'. One
    ;; advantage is that using the traditional ! switch with Dired locks up
    ;; Emacs until you close the other program. Extview does not and leaves
    ;; Emacs free for continued used.

    ;; >> how to associate external programs to known file types
    ;; >> in Dired. For example, associating
    ;; >
    ;; > Hi, i use `extview.el', it's work fine.
    ;; > It reads in a `.mailcap' file.

    ;; If you need to open a file in Emacs that has an extension that Extview
    ;; will open in another viewer, like HTML, you use `find-file-literally'
    ;; to open it in Emacs.
    (try-require 'extview)

    ;; See news "Opening html-File in Dired with w3m" for extra info


    ;; Dired stuff to open files a la Windows (from Howard Melman):
    ;; execute file using windows associations
    (GNUEmacs (when running-ms-windows
                (defun dired-is-dir ()
                  (file-directory-p (dired-get-filename)))

                (defun dired-execute-file (&optional arg)
                  (interactive "P")
                  (mapcar #'(lambda (file)
                              (w32-shell-execute
                               "open" (convert-standard-filename file)))
                          (dired-get-marked-files nil arg)))

                (defun dired-mouse-execute-file (event)
                  "In Dired, execute the file or goto directory name you click
on."
                  (interactive "e")
                  (set-buffer (window-buffer (posn-window (event-end event))))
                  (goto-char (posn-point (event-end event)))
                  (if (dired-is-dir)
                      (dired-find-file)
                    (dired-execute-file)))
                (global-set-key [?\C-x mouse-2] 'dired-mouse-execute-file)

                (defun hrm-dired-mode-hook ()
                  "Hook run when entering Dired mode."
                  (define-key dired-mode-map (kbd "X") 'dired-execute-file)
                  (define-key dired-mode-map
                    [M-down-mouse-1] 'dired-mouse-execute-file))

                (add-hook 'dired-mode-hook 'hrm-dired-mode-hook)))


    ;; extensions to Dired
    (try-require 'dired+)

    ;; reuse the current Dired directory buffer to visit another directory
    ;; (limit Dired to 1 single buffer)
    (when (try-require 'dired-single)
        (define-key dired-mode-map [return] 'joc-dired-single-buffer)
        (define-key dired-mode-map [mouse-1] 'joc-dired-single-buffer-mouse)
        (define-key dired-mode-map "^"
          (function
           (lambda nil (interactive) (joc-dired-single-buffer ".."))))
        (define-key dired-mode-map (kbd "C-x C-j")
          (function
           (lambda nil (interactive) (joc-dired-single-buffer "..")))))

;;;         ;; dired-sort-map.el
;;;         ;; press s then s, x, t or n to sort by Size, eXtension, Time or Name
;;;         (require 'dired-sort-map)

;;; Check this out as well:
;;;     http://www.emacswiki.org/emacs/DiredSortMenu

    )

(add-hook 'dired-load-hook
          (lambda ()
            (load "dired-column-widths.el")))

(defun my-browse-dir ()
  "Open the current directory in your OS's file manager."
  (interactive)
  (let ((dir-as-string
         (file-name-as-directory (expand-file-name ".")))
        (file-manager
         (cond (running-ms-windows "explorer")
               (t "/usr/lib/kde4/bin/dolphin"))))
                ;; `nautilus --no-desktop', `gnome-open'
                ;; or `xdg-open' (in `xdg-utils')
    (start-process "browse" nil file-manager dir-as-string)))

(defun dired-open-externally ()
  "Open the current directory in your OS's file manager (see `~/.mailcap')."
  (interactive)
  (let ((fileobject (dired-get-file-for-visit)))
    (start-process "dired-external" nil "xdg-open" fileobject)
    (message "Opening file %s" fileobject)))

(define-key dired-mode-map (kbd "e") 'dired-open-externally)

(defun open-in-desktop ()
  "Open the current file's folder in desktop."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt") (w32-shell-execute "explore" "."))
   ((string-equal system-type "darwin") (shell-command "open ."))))



;;*** 37.16 (info "(emacs)Dired and Find")

;; For searches in Dired, see `dired-do-search' (`A').
;; Search through all marked files for a match for regexp.
;; Stops when a match is found.
;; To continue searching for next match, use command `M-,'.

;; search for files with names matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?1] 'find-name-dired)

;; search for files with contents matching a wild card pattern and Dired the
;; output
(global-set-key [(control c) ?2] 'find-grep-dired)

;; run grep via find, with user-specified arguments
(global-set-key [(control c) ?3] 'grep-find)

;; ignore `.svn' and `CVS' directories
(setq grep-find-command
      (concat
       "find . \\( -path '*/.svn' -o -path '*/CVS' \\) -prune -o -type f "
              "-print0 | "
              "xargs -0 -e grep -i -n -e "))


;;*** 37.17 Editing the (info "(emacs)Wdired") Buffer

;; Wdired mode is great for renaming (a lot of) files in a directory, as it
;; allows editing the Dired buffer like a text file, using all the power of
;; Emacs. That is, one can use keyboard macros, search and replace,
;; rectangle mode (great for adding prefixes to file names), flip mode bits
;; with the mouse, etc.!

;; in Dired, put a Dired buffer in a mode in which filenames are editable
(when (try-require 'wdired)
    (autoload 'wdired-change-to-wdired-mode "wdired")
    (add-hook 'dired-load-hook
              (lambda ()
                (define-key dired-mode-map
                  (kbd "E") 'wdired-change-to-wdired-mode))))


;;*** Add-Ons

;; add a binding "w" -> `dired-find-w3m' to Dired
(defun dired-find-w3m () (interactive)
  "In Dired, visit (with find-w3m) the file named on this line."
  (w3m-find-file (file-name-sans-versions (dired-get-filename) t)))

(eval-after-load "dired"
  '(progn (define-key dired-mode-map "w" 'dired-find-w3m)))


;; 2-pane file manager based on Dired and inspired by MC, with tree extension
(try-require 'sunrise-commander)
;; then `M-x sunrise'


(message "37 Dired, the Directory Editor... Done"))



;;** 38 The (info "(emacs)Calendar/Diary")

(when section-calendar-diary (message "38 The Calendar and the Diary...")

;; TODO All now being done within Org mode...

;;*** 38.1 (info "(emacs)Calendar Motion")

;; years must be written in full
(setq abbreviated-calendar-year nil)
(setq diary-abbreviated-year-flag nil)

;; ;; interpret the date 1/2/1990 as February 1, 1990
;; (setq european-calendar-style t)  ; obsolete!

;; set the style of calendar and diary dates to ISO
(setq calendar-date-style 'iso)

;; week in the calendar begins on Monday
(setq calendar-week-start-day 1)

;; mark all visible dates that have diary entries
(setq mark-diary-entries-in-calendar t)
;; (add-hook 'initial-calendar-window-hook 'mark-diary-entries)

;; marks the current date, by changing its face
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

;; bind calendar to `C-c c'
(global-set-key (kbd "C-c c") 'calendar)


;;*** 38.2 (info "(emacs)Scroll Calendar")

;; fix foolish calendar-mode scrolling
(add-hook 'calendar-load-hook
          (lambda ()
            (setq mark-holidays-in-calendar t)
            (define-key calendar-mode-map [(>)] 'scroll-calendar-left)
            (define-key calendar-mode-map [(<)] 'scroll-calendar-right)
            (define-key calendar-mode-map [(control x) (>)]
              'scroll-calendar-left)
            (define-key calendar-mode-map [(control x) (<)]
              'scroll-calendar-right)))


;;*** 38.7 Times of (info "(emacs)Sunrise/Sunset")

(setq calendar-latitude [50 87 north])
(setq calendar-longitude [4 71 east])
(setq calendar-location-name "Leuven, BE")

;; (setq calendar-latitude [43 41 north])
;; (setq calendar-longitude [6 81 east])
;; (setq calendar-location-name "Boulouris, FR")


;;*** 38.10 The (info "(emacs)Diary")

;; The Emacs diary keeps track of appointments or other events on a daily
;; basis, in conjunction with the calendar. To use the diary feature, you
;; must first create a "diary file" containing a list of events and their
;; dates.

(when (try-require 'diary-lib)

    ;; create an empty diary file (if it does not exist yet)
    (unless (file-exists-p diary-file)
      (shell-command (concat "touch " diary-file)))

    ;; copy the diary entries into a special buffer (also display the diary
    ;; when I do `M-x diary')
    (add-hook 'diary-display-hook 'fancy-diary-display)

    ;; TODO Sort each day's diary entries by their time of day?
    (add-hook 'diary-display-hook 'sort-diary-entries)
    (add-hook 'list-diary-entries-hook 'sort-diary-entries t)

    ;; allow `#includes' in `~/diary'
    (add-hook 'list-diary-entries-hook 'include-other-diary-files)

    ;; generate the diary window for 4 days starting with the current date
    (diary 4)

    ;; How do you arrange the entries of diary? Can they be automatically
    ;; arranged according to date and not just according to when they were
    ;; entered into the diary?
)


;;*** 38.11 (info "(emacs)Appointments")

;; enable appointment notification, several minutes beforehand
(add-hook 'diary-hook 'appt-make-list)


(when (try-require 'org-agenda)

    ;; Insinuate appt
    (require 'appt)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt)
    ;; When use 'r' (rebuild agenda) reload appt
    (add-hook 'org-agenda-mode-hook (lambda ()
                                      (setq appt-time-msg-list nil)
                                      (org-agenda-to-appt)))
    (setq appt-audible t)
    (setq appt-display-format 'echo)

;;;;
    ;; turn appointment checking on
    (appt-activate 1)

    ;; time in minutes before an appointment that the warning begins
    (setq appt-message-warning-time 15)  ; 12

    ;; number of minutes to wait between checking the appointment list
    (setq appt-display-interval 5)  ; 3

    ;; update appt each time agenda opened
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
;;;;

    (when window-system
      (setq appt-display-format 'window)

      ;; FIXME Check `notify-send' (in `libnotify-bin' Ubuntu package) is installed
      (defun rgr/org-display (min-to-app new-time msg)
        (shell-command
         (concat "notify-send "
                 "-i /usr/share/icons/gnome/32x32/status/appointment-soon.png "
                 "'Appointment' "
                 "'" msg "'")))
      ;; TODO For Windows users: use `todochicku.el' and the snarl notifier

      (setq appt-disp-window-function (function rgr/org-display)))
)


;;*** 38.14 Summing (info "(emacs)Time Intervals")

;; http://emacswiki.org/cgi-bin/wiki/TimeClock

;; check Org's capabilities in this area: (info "(org)Clocking work time")


;;*** 38.15 (info "(emacs)Advanced Calendar/Diary Usage")

;; ;; list of notable days
;; (setq calendar-holidays nil)

;; remove some holidays
(setq holiday-bahai-holidays nil)       ; get rid of Baha'i holidays
(setq holiday-general-holidays nil)     ; get rid of too U.S.-centric holidays
(setq holiday-hebrew-holidays nil)      ; get rid of religious holidays
(setq holiday-islamic-holidays nil)     ; get rid of religious holidays
(setq holiday-oriental-holidays nil)    ; get rid of Oriental holidays
(setq holiday-solar-holidays nil)

;; add some Belgian holidays
(setq holiday-local-holidays
      '(
        (holiday-fixed 01 01 "New Year's Day")
        (holiday-fixed 02 14 "Valentine's Day")
        (holiday-fixed 05 01 "Labor Day")
        (holiday-fixed 07 21 "Independence Day")
        (holiday-fixed 08 15 "Assumption")
        (holiday-fixed 11 01 "Toussaint")
        (holiday-fixed 11 11 "Armistice 1918")

        ;; holidays with variable dates
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 6 0 3 "Father's Day")))

;; user defined holidays
(setq holiday-other-holidays nil)  ; default

;; mark dates of holidays in the calendar
(setq mark-holidays-in-calendar t)


;;*** Add-Ons: Getting Things Done with (info "(org)Top") Mode

;; We should get this for Org mode as well: asking the user if...
;; ;; ask if the user wants to clock out before exiting Emacs
;; (add-hook 'kill-emacs-query-functions 'timeclock-query-out)
;;
;; ;; ask the user if they wish to clock in
;; (timeclock-query-in))


;; After all the configuration has been done, you can easily manage your
;; daily work and tasks with Org mode.
;; Press `C-c a a' to jump you to this week's task page from anywhere.

;; (info "(org)Top") outline-based notes management and organizer
(when (try-require 'org-install)

  (try-require 'org-list)

  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . auto)))

  (setq org-completion-use-ido t)

  (setq org-return-follows-link t)

  (add-to-list 'org-modules 'org-habit)

  ;; enable globally unique ID
  (add-to-list 'org-modules 'org-id)




  ;; (defun bh/insert-inactive-timestamp ()
  ;;   (interactive)
  ;;   (save-excursion
  ;;     (insert "\n")
  ;;     (org-cycle)
  ;;     (org-insert-time-stamp nil t t nil nil nil)))
  ;; (add-hook 'org-insert-heading-hook 'bh/insert-inactive-timestamp)


  ;; Unhiding edited areas
  ;; I like the idea of clustering undo but find it disconcerting
  (setf org-self-insert-cluster-for-undo nil)
  ;; somebody, I think Carsten, suggested this, and it might work for you, but
  ;; for some reason I commented it out. I don't remember what the reason was.
  ;; Maybe speed.
  '(defadvice undo (after org-undo-reveal activate)
     "Make point and context visible after an undo command in Org mode."
     (and (org-mode-p) (org-reveal)))
  ;;(ad-unadvise 'undo)



  (defun my/org-switch-language ()
    "Switch language for Org file, if a `#+LANGUAGE:' meta-tag is
on top 14 lines."
    (save-excursion
      (goto-line 15)
      (if (re-search-backward "#\\+LANGUAGE: +\\([A-Za-z_]*\\)" 1 t)
          (ispell-change-dictionary (match-string 1)))))



  (add-hook 'org-mode-hook
            (lambda ()
              ;; display images in your Org files
              (turn-on-iimage-mode)

              (local-set-key "\M-n" 'outline-next-visible-heading)
              (local-set-key "\M-p" 'outline-previous-visible-heading)

              ;; table
              (local-set-key "\M-\C-w" 'org-table-copy-region)
              (local-set-key "\M-\C-y" 'org-table-paste-rectangle)
              (local-set-key "\M-\C-l" 'org-table-sort-lines)

              ;; fix tab
              (local-set-key "\C-y" 'yank)

              ;; file modification date
              (set (make-local-variable 'time-stamp-format) "%:y-%02m-%02d")
              (set (make-local-variable 'time-stamp-start) "^#\\+DATE: +")
              (set (make-local-variable 'time-stamp-end) "$")

              ;; guess language
              (my/org-switch-language)

              ;; flyspell mode to spell check everywhere
              (flyspell-mode 1)))


    ;; getting started
    (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
    (define-key global-map (kbd "C-c l") 'org-store-link)
    (define-key global-map (kbd "C-c a") 'org-agenda)

    (global-set-key (kbd "C-c o a l") 'org-agenda-list)
    (global-set-key (kbd "C-c o a t") 'org-todo-list)


    ;; XXX
    (setq org-global-properties
          '(("Effort_ALL" .
             "0 0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00")))


    ;; 
    (setq org-enforce-todo-dependencies t)
    (setq org-enforce-todo-checkbox-dependencies t)
    (setq org-agenda-dim-blocked-tasks t)



    (setq org-special-ctrl-a/e t)

    ;;
    (setq org-show-siblings t)

    (setq org-show-hierarchy-above t)

    (setq org-show-following-heading t)

    ;; don't fontify the whole line for headings
    (setq org-fontify-whole-heading-line nil)


;;**** 1 (info "(org)Introduction")

    ;; 1.3 insert the first line setting Org mode in empty files
    (setq org-insert-mode-line-in-empty-file t)


;;**** 2 (info "(org)Document Structure")

    ;; 2.3 don't switch to OVERVIEW at startup
    (setq org-startup-folded nil)

    ;; 2.4 headlines in the current buffer are offered via completion
    ;; (interface also used by the refile command)
    (setq org-goto-interface 'outline-path-completion)

    ;; 2.5 ellipsis to use in the Org mode outline
    (setq org-ellipsis "……")
    ; (setq org-ellipsis 'org-column)

    ;; 2.7 don't make TAB cycle visibility on plain list items
    (setq org-cycle-include-plain-lists nil)


;;**** 3 (info "(org)Tables")

    ;; Have a look at "Org as a spreadsheet system: a short introduction"
    ;; (http://orgmode.org/worg/org-tutorials/org-spreadsheet-intro.php)

    ;; default export parameters for `org-table-export'
    (setq org-table-export-default-format "orgtbl-to-csv")

    ;; FIXME Only set calc-internal-prec to 12 (instead of 8 by default)
    ;; 3.5.2
    (setq org-calc-default-modes
          '(calc-internal-prec 12
            calc-float-format  (float 12)
            calc-angle-mode    deg
            calc-prefer-frac   nil
            calc-symbolic-mode nil
            calc-date-format (YYYY "-" MM "-" DD " " Www (" " hh ":" mm))
            calc-display-working-message t))

    ;; recalculate all tables in a file simultaneously
    (defun org-recalculate-all-tables ()
      (interactive)
      (org-table-map-tables (lambda () (org-table-recalculate t)) t))


;;**** 4 (info "(org)Hyperlinks")

    ;; directory with org files (used by the hooks for `remember.el')
    (setq org-directory
          (if (file-directory-p "~/Personal/")
              "~/Personal/"
            "~/"))

    ;; create web links to Google groups or Gmane (instead of Gnus messages)
    (setq org-gnus-prefer-web-links t)

    (defun org-toggle-link-style ()
      "Toggle between descriptive and literal link styles."
      (interactive)
      (if (member '(org-link) buffer-invisibility-spec)
          ;; descriptive -> literal
          (progn
            (org-remove-from-invisibility-spec '(org-link))
            (message "Showing literal links"))
        ;; literal -> descriptive
        (org-add-to-invisibility-spec '(org-link))
        (message "Showing descriptive links"))
      (org-restart-font-lock))

    ;; 4.3 function and arguments to call for following mailto links
    (setq org-link-mailto-program '(compose-mail "%a" "%s"))

    ;; 
    (setq org-link-frame-setup '((vm   . vm-visit-folder)
                                 (gnus . org-gnus-no-new-news)
                                 (file . find-file-other-window)))


;;**** 5 (info "(org)TODO Items")

    ;; 5.1 select a TODO state and bypass any logging associated with that
    (setq org-treat-S-cursor-todo-selection-as-state-change nil)

    ;; We can use TODO keywords to implement the different task states:
    ;; 5.2.4 list of TODO entry keyword sequences and their interpretation
    (setq org-todo-keywords
          '((sequence "TODO(t)" ; 16
                      ; "SOMEDAY(m!)" ; 8 inactive project
                      "STARTED(s!)" ; 5
                      "WAIT(w@/!)" ; 9
                      "DELEGATED(l)" ; 5
                      "DFRD(f)" ; 2    ;; XXX is DEFERRED a completion state?
                      "|"
                      "DONE(d!/!)" ; 14
                      "CANCELED(c@/!)") ; 9
            (sequence "QUOTE(q!)" "QUOTED(Q!)" "|"
                      "APPROVED(A@)" "EXPIRED(E@)" "REJECTED(R@)")
            (sequence "OPENPO(O@)" "|" "CLOSEDPO(C!/!)")))

    ;; 5.2.6 faces for specific TODO keywords
    (GNUEmacs
    (setq org-todo-keyword-faces
          '(("TODO" . org-todo)
            ("SOMEDAY" . fni-org-someday-kwd-face)
            ("STARTED" . fni-org-started-kwd-face)
            ("WAIT" . fni-org-waiting-kwd-face)
            ("DELEGATED" . fni-org-delegated-kwd-face)
            ("DFRD" . fni-org-deferred-kwd-face)
            ("DONE" . org-done)
            ("CANCELED" . fni-org-canceled-kwd-face)

            ("QUOTE" . fni-org-quote-kwd-face)
            ("QUOTED" . fni-org-quoted-kwd-face)
            ("APPROVED" . fni-org-approved-kwd-face)
            ("EXPIRED" . fni-org-expired-kwd-face)
            ("REJECTED" . fni-org-rejected-kwd-face)

            ("OPENPO" . fni-org-openpo-kwd-face)
            ("CLOSEDPO" . fni-org-closedpo-kwd-face))))

    ;; change the face of a headline (as an additional information) if it is
    ;; marked DONE (to face `org-headline-done')
    (setq org-fontify-done-headline t)

    ;; 5.3.1 insert a CLOSED time stamp each time a TODO entry is marked DONE
    (setq org-log-done nil)

    ;; 5.3.2 insert state change notes and time stamps into a drawer
    (setq org-log-into-drawer t)

    ;; 5.3.2 the notes will be ordered according to time
    (setq org-log-states-order-reversed nil)


;;**** 6 (info "(org)Tags")

    ;; Context (place, time or particular resources for doing a task) and
    ;; people are something best implemented with tags.

    ;; 6.2 list of tags ("contexts") allowed in Org mode files
    (setq org-tag-alist '((:startgroup . nil)
                            ("home" . ?h)
                            ("work" . ?w)
                          (:endgroup . nil)
                          ("errands" . ?e)
                          ("computer" . ?c)
                          ("online" . ?o)  ; Internet
                          ("mail" . ?m)  ; (e)mail
                          ("phone" . ?p)
                          ("reading" . ?r)

                          ("note" . ?n)
                          ))

    ;; ("waiting" . ?W)
    ;; ("someday" . ?s)

    ;; morning, midday, afternoon, evening
    ;; monday, tuesday, ...
    ;; weekend

    ;; important

    ;; ("note" . ?n)
    ;; ("crypt" . ?XXX)
    ;; ("appt" . ?a)
    ;; ("next" . ?n)

    ;; ("PROJ" . ?P)


    ;; faces for specific tags
    (setq org-tag-faces
          '(
            ("refile" . (:background "#D4EAFF"))
            ("home" . (:background "pale green" :italic t))
            ("work" . (:italic t :background "#F9E816"))
            ))

    ;; remove redundant tags of headlines (from David Maus)
    (defun my/org-remove-redundant-tags ()
      "Remove redundant tags of headlines in current buffer.
A tag is considered redundant if it is local to a headline and
inherited by a parent headline."
      (interactive)
      (when (eq major-mode 'org-mode)
        (save-excursion
          (org-map-entries
           '(lambda ()
              (let ((alltags (split-string (or (org-entry-get (point) "ALLTAGS") "") ":"))
                    local inherited tag)
                (dolist (tag alltags)
                  (if (get-text-property 0 'inherited tag)
                      (push tag inherited) (push tag local)))
                (dolist (tag local)
                  (if (member tag inherited) (org-toggle-tag tag 'off)))))
           t nil))))


;;**** 8 (info "(org)Dates and Times")

    ;; 8.2 number of minutes to round time stamps to
    (setq org-time-stamp-rounding-minutes '(5 5))

    ;; 8.3 no. of days to display before expiration of a deadline
    (setq org-deadline-warning-days 14)

    ;; skip deadline prewarning (up to 7 days before the actual deadline) when
    ;; entry is also scheduled
    (setq org-agenda-skip-deadline-prewarning-if-scheduled 7)

    ;; 8.3 don't show deadlines when the corresponding item is done
    (setq org-agenda-skip-deadline-if-done t)

    ;; 8.3 don't show scheduled items in agenda when they are done
    (setq org-agenda-skip-scheduled-if-done t)

    ;; 8.5 format string for displaying dates in the daily/weekly agenda and
    ;; in the timeline
    (setq org-agenda-format-date
          (concat "\n" "%Y-%m-%d" " %a "
                  (make-string (- (window-width) 15) ?_)))

    ;; faces for showing deadlines in the agenda
    (setq org-agenda-deadline-faces
          '((1.01 . fni-org-deadline-yesterday)
            (0.99 . fni-org-deadline-today)
            (0.49 . fni-org-deadline-tomorrow)
            (0.00 . fni-org-deadline-later)))

    ;; don't select item by timestamp or -range if it is DONE
    (setq org-agenda-skip-timestamp-if-done t)

    ;; show all days between the first and the last date
    (setq org-timeline-show-empty-dates t)

    (GNUEmacs
        ;; the time clocking code for Org mode
        (when (try-require 'org-clock)

            ;; If you have an `Effort' property defined, its value is also
            ;; shown in the mode line, and you can configure `org-clock-sound'
            ;; to get an alert when your planned time for a particular item is
            ;; over.
            (setq org-clock-sound "~/Music/Sounds/alarm.wav")
                                        ; sound that will used for
                                        ; notifications

            ;; remove the clock line when the resulting time is 0:00
            (setq org-clock-out-remove-zero-time-clocks t)

    ;;;         ;; when clocking into a task with a clock entry which has not
    ;;;         ;; been closed, resume the clock from that point
    ;;;         (setq org-clock-in-resume t)

            ;; 8.4 save both the running clock and the entire clock history
            ;; when Emacs is closed, and resume it next time Emacs is started
            ;; up
            (setq org-clock-persist t)

            ;; 8.4 set up hooks for clock persistence
            (org-clock-persistence-insinuate)

            ;; resume clocking task on clock-in if the clock is open
            (setq org-clock-in-resume t)

            ;; ;; 8.5 set task state to STARTED while clocking it
            ;; (setq org-clock-in-switch-to-state "STARTED")

            ;; 8.5 resolve open clocks if the user is idle more than 15
            ;; minutes
            (setq org-clock-idle-time 15)

            ;; clock won't be stopped when the clocked entry is marked DONE
            (setq org-clock-out-when-done nil)

            ;; time included for the modeline clock is all time clocked into
            ;; this task today
            (setq org-clock-modeline-total 'today)))



    ;; "C-c a t" should show all the TODO items.  You may also want to
    ;; take a look at "org-agenda-todo-ignore-deadlines",
    ;; "org-agenda-todo-ignore-scheduled" and
    ;; "org-agenda-todo-ignore-with-date" as well.


    (setq org-agenda-columns-add-appointments-to-effort-sum t)
    (setq org-agenda-default-appointment-duration 60)



    (defun my/org-clock-in-task-by-id (id)
      "Start the clock on the entry with id ID."
      (require 'org-id)
      (save-restriction
        (widen)
        (org-with-point-at (org-id-find id 'marker)
          (org-clock-in nil))))

    (defun my/org-clock-in-task-organization ()
      "Start the clock on the entry entry \"Organization\"."
      (interactive)
      (my/org-clock-in-task-by-id "94fa5272-008f-4a26-b4fc-cbc5304f2e05"))

    (defun my/org-goto-task-organization ()
      "Goto Organization heading."
      (interactive)
      (org-id-goto "94fa5272-008f-4a26-b4fc-cbc5304f2e05"))

    (defun my/org-clock-in-task-emails-and-news ()
      "Start the clock on the entry \"Emails and News\"."
      (interactive)
      (my/org-clock-in-task-by-id "3f14e63e-0cd5-4901-9c00-459bbf658a58"))

    ;; (global-set-key (kbd "<f9> o") 'my/org-clock-in-task-organization)
    ;; (global-set-key (kbd "<f9> m") 'my/org-clock-in-task-emails-and-news)
    (global-set-key (kbd "C-c C-x C-o") 'org-clock-out)

    ;; Maybe `C-u M-x org-clock-in RET' does what you want.
    (global-set-key (kbd "C-c C-x S-C-i")
                    (lambda ()
                      (interactive)
                      (org-clock-in '(4))))

    ;; (add-hook 'remember-mode-hook 'org-clock-in 'append)
    ;; (add-hook 'org-remember-before-finalize-hook 'my/org-clock-in-interrupted-task)

    ;; FIXME What if "No active clock"?

    (defun my/org-clock-in-interrupted-task ()
      "Clock back into the task that has been interrupted, if there is one."
      (interactive)
      (if (and (not org-clock-resolving-clocks-due-to-idleness)
               (marker-buffer org-clock-marker)
               (marker-buffer org-clock-interrupted-task))
          (org-with-point-at org-clock-interrupted-task
            (org-clock-in nil))
        (org-clock-out)))

    (global-set-key (kbd "C-c C-x C-q") 'my/org-clock-in-interrupted-task)
    (global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
    (global-set-key (kbd "C-c C-x C-i") 'org-clock-in)



    ;; add an effort estimate on the fly when clocking in
    (defun my/org-mode-ask-effort ()
      "Ask for an effort estimate when clocking in."
      (unless (org-entry-get (point) "Effort")
        (let ((effort
               (completing-read
                "Effort: "
                (org-entry-get-multivalued-property (point) "Effort"))))
          (unless (equal effort "")
            (org-set-property "Effort" effort)))))

    (add-hook 'org-clock-in-prepare-hook
              'my/org-mode-ask-effort)




    ;; number of clock tasks to remember in history
    (setq org-clock-history-length 9)





    ;; get a compact view during follow mode in the agenda
    (defun my-compact-follow ()
      "Make the view compact, then show the necessary minimum."
      (ignore-errors
        (save-excursion
          (while (org-up-heading-safe))
          (hide-subtree)))
      (let ((org-show-siblings nil)
            (org-show-hierarchy-above t))
        (org-reveal))
      (save-excursion
        (org-back-to-heading t)
        (show-children)))

    (add-hook 'org-agenda-after-show-hook 'my-compact-follow)





    (defun sacha/org-calculate-free-time (date start-time end-of-day)
      "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
DATE is a list of the form (MONTH DAY YEAR).
START-TIME and END-OF-DAY are the number of minutes past midnight."
      (save-window-excursion
        (let ((files org-agenda-files)
              (total-unscheduled 0)
              (total-gap 0)
              file
              rtn
              rtnall
              entry
              (last-timestamp start-time)
              scheduled-entries)
          (while (setq file (car files))
            (catch 'nextfile
              (org-check-agenda-file file)
              (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
              (setq rtnall (append rtnall rtn)))
            (setq files (cdr files)))
          ;; For each item on the list
          (while (setq entry (car rtnall))
            (let ((time (get-text-property 1 'time entry)))
              (cond
               ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
                (setq scheduled-entries (cons (cons
                                               (save-match-data (appt-convert-time (match-string 1 time)))
                                               (save-match-data (appt-convert-time (match-string 2 time))))
                                              scheduled-entries)))
               ((and time
                     (string-match "\\([^-]+\\)\\.+" time)
                     (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry)))
                (setq scheduled-entries
                      (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
                                        (appt-convert-time (match-string 2 time))))) 
                        (cons (cons start
                                    (and (string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
                                         (+ start (string-to-number (match-string 2 (get-text-property 1 'txt entry))))))
                              scheduled-entries))))
               ((string-match "^[A-Z]+ \\(\\[#[A-Z]\\] \\)?\\([0-9]+\\)" (get-text-property 1 'txt entry))
                (setq total-unscheduled (+ (string-to-number
                                            (match-string 2 (get-text-property 1 'txt entry)))
                                           total-unscheduled)))))
            (setq rtnall (cdr rtnall)))
          ;; Sort the scheduled entries by time
          (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

          (while scheduled-entries
            (let ((start (car (car scheduled-entries)))
                  (end (cdr (car scheduled-entries))))
              (cond
               ;; are we in the middle of this timeslot?
               ((and (>= last-timestamp start)
                     (<= last-timestamp end))
                ;; move timestamp later, no change to time
                (setq last-timestamp end))
               ;; are we completely before this timeslot?
               ((< last-timestamp start)
                ;; add gap to total, skip to the end
                (setq total-gap (+ (- start last-timestamp) total-gap))
                (setq last-timestamp end)))
              (setq scheduled-entries (cdr scheduled-entries))))
          (if (< last-timestamp end-of-day)
              (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
          (cons total-unscheduled total-gap))))


    (defun sacha/org-show-load ()
      "Show my unscheduled time and free time for the day."
      (interactive)
      (let ((time (sacha/org-calculate-free-time
                   ;; today
                   (calendar-gregorian-from-absolute (time-to-days (current-time)))
                   ;; now
                   (let* ((now (decode-time))
                          (cur-hour (nth 2 now))
                          (cur-min (nth 1 now)))
                     (+ (* cur-hour 60) cur-min))
                   ;; until the last time in my time grid
                   (let ((last (car (last (elt org-agenda-time-grid 2)))))
                     (+ (* (/ last 100) 60) (% last 100))))))
        (message "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
                 (/ (car time) (* .01 (cdr time)))
                 (car time)
                 (cdr time)
                 (- (cdr time) (car time)))))

    (defun sacha/org-agenda-load (match)
      "Can be included in `org-agenda-custom-commands'."
      (let ((inhibit-read-only t)
            (time (sacha/org-calculate-free-time
                   ;; today
                   (calendar-gregorian-from-absolute org-starting-day)
                   ;; now if today, else start of day
                   (if (= org-starting-day
                          (time-to-days (current-time)))
                       (let* ((now (decode-time))
                              (cur-hour (nth 2 now))
                              (cur-min (nth 1 now)))
                         (+ (* cur-hour 60) cur-min))
                     (let ((start (car (elt org-agenda-time-grid 2))))
                       (+ (* (/ start 100) 60) (% start 100))))
                   ;; until the last time in my time grid
                   (let ((last (car (last (elt org-agenda-time-grid 2)))))
                     (+ (* (/ last 100) 60) (% last 100))))))
        (goto-char (point-max))
        (insert (format
                 "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
                 (/ (car time) (* .01 (cdr time)))
                 (car time)
                 (cdr time)
                 (- (cdr time) (car time))))))



    (when (try-require 'org-habit)

      (add-to-list 'org-modules 'org-habit)

      (setq org-habit-show-habits-only-for-today nil))


;;**** 9 (info "(org)Remember")

    ;; (info "(remember)Top")

    ;; a mode for quickly jotting down things to remember
    (when (try-require 'remember-XXX)  ; the ultimate capture tool

      ;; default target for storing notes
      (setq remember-data-file (concat org-directory "refile.org"))

      ;; key bindings
      (global-set-key (kbd "C-c r") 'remember)
        ; save yourself some copying and pasting by marking a region of text
        ; and using `C-u C-c r' (remember): the selected text will be
        ; included in the buffer, so all you have to do is comment on it.

      (defun remember-review-file ()
        "Open `remember-data-file'."
        (interactive)
        (find-file-other-window remember-data-file))

      (global-set-key (kbd "C-c R") 'remember-review-file))


    ;; 9.1.2 default target for storing notes
    (setq org-default-notes-file (concat org-directory "refile.org"))
                                        ; Inbox for collecting

    ;; 9.1.2 templates for the creation of remember buffers
    (setq org-remember-templates
          '(("Buffer" ?b "* %a\n\n%i%?%!" org-default-notes-file "Buffer" nil)
            ("Org" ?o "* %a\n\n%i%?" org-default-notes-file "Org" nil)
            ("Infos" ?i "* %a\n\n%i%?%!" org-default-notes-file "Infos" nil)))
    (setq org-remember-templates
          '(("Tasks" ?t "* TODO %?\n  %i\n  %a" remember-data-file)))
    (setq org-remember-templates
          '(("Task" ?t "* TODO %?\n  %i\n  %a" "~/Personal/todo.org")
            ("Journal" ?j "* %U %?\n\n  %i\n  %a" "~/Personal/journal.org")))
    (setq org-remember-templates
          '((?t "* TODO %^{Todo} %^G\n %i\n %a\n %U\n")
            (?T "* TODO %^{Todo} %^G\n %i\n %U\n")
            (?n "* %^{Title}\n %i\n %a\n %U\n")
            (?N "* %^{Title}\n %i\n %U\n")))
    (setq org-remember-templates
          '(("Tasks" ?t "* TODO %?\n  %i\n  %a"            "~/Personal/organizer.org")
            ("RT"    ?R "* [[RT:%^{Number}][%^{Number}/%^{Description}]]" "~/Personal/rt.org")))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %? %^g\n %i\n " "F:/GTD/newgtd.org" "Office")
            ("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "L:journal.org")
            ("Book" ?b "\n* %^{Book Title} %t :READING: \n%[l:/booktemp.txt]\n" "L:journal.org")
            ("Private" ?p "\n* %^{topic} %T \n%i%?\n" "F:/gtd/privnotes.org")
            ("Contact" ?c "\n* %^{Name} :CONTACT:\n%[l:/contemp.txt]\n" "F:/gtd/privnotes.org")))
    (setq org-remember-templates
          '(("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" "~/Personal/newgtd.org" "Tasks")
            ("Journal"   ?j "** %^{Head Line} %U %^g\n%i%?"  "~/Personal/journal.org")
            ("Clipboard" ?c "** %^{Head Line} %U %^g\n%c\n%?"  "~/Personal/journal.org")
            ("Receipt"   ?r "** %^{BriefDesc} %U %^g\n%?"   "~/Personal/finances.org")
            ("Daily Review" ?a "** %t :COACH: \n%[~/.daily_review.txt]\n" "~/Personal/journal.org")
            ("Someday"   ?s "** %^{Someday Heading} %U\n%?\n"  "~/Personal/someday.org")
            ("Vocab"   ?v "** %^{Word?}\n%?\n"  "~/Personal/vocab.org")))
    (setq org-remember-templates
          '((?t "* TODO %?\n   %i\n %a" "~/Personal/glyn.org")
            (?n "*Note: %?\n%^T\n%i\n  %a" "~/Personal/notes.org"  )
            (?q "*Quote: %?\n%^T\n%i\n  %a" "~/Personal/barth.org"  )))
    (setq org-remember-templates
          '(("Task" ?t "* %^{Task status|TODO|STARTED|SUBTASK|DONE} %^{Brief
Description} %^G\n %^{subject}p  %^{other-subjects}p  %^{sub-subjects}p 
%^{keywords}p %?\n    Added: %U \n" "~/notes/notes-log-090410.org" "Task")))

    ;; Unsorted
    ;; Temporary
    ;; To Do
    ;; Applications
    ;; Web Quotes
    ;; Documentations
    ;; People
    ;; Projects
    ;; Job Ads

    ;; Family
    ;; Friends
    ;; Future
    ;; Graphics
    ;; Languages
    ;; Linux
    ;; Magical Moments
    ;; Networks
    ;; Projects
    ;; Traveling

    ;; setup `remember.el' for use with Org mode
    (eval-after-load 'remember
      '(org-remember-insinuate))


  ;; fast note taking in Org mode (the ultimate capture tool)
  (when (try-require 'org-capture)
    (define-key global-map "\C-cr" 'org-capture)

    ;;   + %a :: annotation (link)
    ;;   + %i :: initial content (selected text)
    ;;   + %? :: cursor position
    ;;   + %^T :: prompt for a date and time
    ;;   + %^G :: prompt for tags with completion on tags in all agenda files
    ;;   + %t :: time stamp (date only)
    ;;   + %^{prompt} :: prompt the user for a string
    ;;   + %[file] :: insert the contents of the file
    ;;   + %U :: inactive time stamp with date and time
    ;;   + %& :: tell remember to jump to the note after storing it
    (setq org-capture-templates
          '(("t" "Task" entry
             (file+headline "~/Personal/refile.org" "Tasks")
             "* TODO %^{Todo}%?
   :PROPERTIES:
   :Created: %U
   :END:

  %i"                                   ; include a date tag showing when it
                                        ; was added to the TODO file
             :empty-lines 1)

            ("a" "Appt" entry
             (file+headline "~/Personal/refile.org" "Appointments")
             "* %^{Appointment}%?
   %^T
   %i"
             :empty-lines 1)

            ("m" "Mail" entry
             (file+headline "~/Personal/refile.org" "Tasks")
             "* Answer this email....%?
   %^T
   %i"
             :empty-lines 1 :immediate-finish)
            ;; immediate-finish :: immediately store note without further
            ;; prompt (skipping `C-c C-c'), which is very handy for quick
            ;; storing of emails

            ("p" "Phone call" entry
             (file+headline "~/Personal/refile.org" "Phone calls")
             "* %?
   %U
   %i
   From %a"
             :empty-lines 1 :clock-in t :clock-resume t)

            ("n" "Note" entry
             (file+headline "~/Personal/refile.org" "Notes")
             "* %?
   %U
   %i
   From %a"
             :empty-lines 1)

            ("j" "Journal" entry
             (file+datetree "~/Personal/journal.org")
             "* %U  :blog:
   %?"
                                        ; "* %^{Title}  :blog:\n  :PROPERTIES:\n  :on: %T\n  :END:\n  %?\n  %x"
            :empty-lines 1)

            ;; notes
            ("N" "Templates adding notes")
            ("Ne" "Emacs" entry
             (file+headline "~/Public/Notes-on-Emacs.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("No" "Org mode" entry
             (file+headline "~/Public/Notes-on-Org.txt" "Notes")
             "* %^{Title}
  :PROPERTIES:
  :Created: %U
  :END:

   %i

   From %a"
             :empty-lines 1)
            ("NL" "Lisp" entry
             (file+headline "~/Public/Notes-on-Lisp.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ng" "Gnus" entry
             (file+headline "~/Public/Notes-on-Gnus.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nl" "LaTeX" entry
             (file+headline "~/Public/Notes-on-LaTeX.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("NT" "TikZ" entry
             (file+headline "~/Public/Notes-on-TikZ.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nb" "Beamer" entry
             (file+headline "~/Public/Notes-on-Beamer.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ns" "StumpWM" entry
             (file+headline "~/Public/Notes-on-StumpWM.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nu" "Linux" entry
             (file+headline "~/Public/Notes-on-Linux.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nc" "Ledger" entry
             (file+headline "~/Public/Notes-on-Ledger.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Nr" "RFID" entry
             (file+headline "~/Public/Notes-on-RFID.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ("Ns" "Security" entry
             (file+headline "~/Public/Notes-on-Security.txt" "Notes")
             "* %^{Title}

   %i

   From %a"
             :empty-lines 1)
            ;; ("web-clippings" ?w
            ;;  "* %^{Title} %^g \n  :PROPERTIES:\n  :date: %^t\n  :link: %^{link}\n  :END:\n\n %x %?"
            ;;  "~/org/data.org" "Web Clippings")
            ("Nw" "Web" entry
             (file+headline org-default-notes-file "Web Snippets")
             "* %^{Title}

   %?%i
   From %c
   %u"
             :empty-lines 1)



            ("w" "org-protocol" entry
             (file "~/Personal/refile.org")
             "* TODO Review %c
   %U"
             :immediate-finish t :clock-in t :clock-resume t)



            ;; ideas
            ("i" "Idea" entry
             (file+headline "~/Personal/refile.org" "Ideas")
             "* %^{Title}
   %?%i
   From %a"
             :empty-lines 1)
            )))


    ;; "Once a date has been scheduled, use cut and paste to move the task to
    ;; the appropriate category."
    ;;
    ;; I find it much easier to use the refile note command `C-c C-w'. This
    ;; lets me select (with completion) the header under which the entry will
    ;; be placed. By default, this only allows first-level headers to be
    ;; selected. But:

    ;; 9.1.2 headline that should be the default location in the notes file
    (setq org-remember-default-headline "Unfiled")

    ;; 9.1.4 any headline with level <= 2 is a target
    (setq org-refile-targets '((nil :maxlevel . 2)
                                        ; all top-level headlines in the
                                        ; current buffer are used (first) as a
                                        ; refile target
                               (org-agenda-files :maxlevel . 2)))

    ;; 9.1.4 provide refile targets as paths, including the file name (without
    ;; directory) as level 1 of the path
    (setq org-refile-use-outline-path 'file)

    ;; 9.1.4 allow to create new nodes (must be confirmed by the user) as
    ;; refile targets
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; refile only within the current buffer
    (defun my/org-refile-within-current-buffer ()
      "Move the entry at point to another heading in the current buffer."
      (interactive)
      (let ((org-refile-targets '((nil :maxlevel . 5))))
        (org-refile)))

    ;; 9.4 capture from Firefox (to store links and text)
    (try-require 'org-protocol)         ; have a look at
                                        ; http://vimeo.com/5662410

    ;; 9.6.1 subtrees should be archived in the current file
    (setq org-archive-location "::* Archive")


;;**** 10 (info "(org)Agenda Views")


    (defcustom find-recursive-exclude-files '(".*.class$" ".*~$" ".*.elc$")
      "List of regular expressions of files to be excluded when recursively searching for files."
      :type '(repeat (string :tag "File regexp")))

    (defun find-file-recursively (file-regexp directory)
      (interactive "sFile name to search for recursively: \nDIn directory: ")
      (let ((directory (if (equal (substring directory -1) "/")
                           directory
                         (concat directory "/")))
            (matches
             (find-recursive-filter-out
              find-recursive-exclude-files
              (find-recursive-directory-relative-files directory "" file-regexp))))
        (cond ((eq (length matches) 0) (message "No file(s) found!"))
              ((eq (length matches) 1)
               (find-file (concat directory (car matches))))
              (t
               (run-with-timer 0.001 nil
                               (lambda ()
                                 (dispatch-event
                                  (make-event 'key-press '(key tab)))))
               (let ((file (completing-read "Choose file: "
                                            (mapcar 'list matches)
                                            nil t)))
                 (if (or (eq file nil) (equal file ""))
                     (message "No file selected.")
                   (find-file (concat directory file))))))))

    (defun find-recursive-directory-relative-files (directory
                                                    relative-directory
                                                    file-regexp)
      (let* ((full-dir (concat directory "/" relative-directory))
             (matches
              (mapcar
               (function (lambda (x)
                           (concat relative-directory x)))
               (find-recursive-filter-out '(nil)
                                          (directory-files full-dir nil
                                                           file-regexp nil t))))
             (inner
              (mapcar
               (function
                (lambda (dir)
                  (find-recursive-directory-relative-files directory
                                                           (concat relative-directory
                                                                   dir "/")
                                                           file-regexp)))
               (find-recursive-filter-out '(nil "\\." "\\.\\.")
                                          (directory-files full-dir nil ".*"
                                                           nil 'directories)))))
        (mapcar (function (lambda (dir) (setq matches (append matches dir))))
                inner)
        matches))

    (defun find-recursive-filter-out (remove-list list)
      "Remove all the elements in *remove-list* from *list*"
      (if (eq list nil)
          nil
        (let ((elem (car list))
              (rest (cdr list)))
          (if (some
               (lambda (regexp)
                 (if (or (eq elem nil) (eq regexp nil))
                     nil
                   (not (eq (string-match regexp elem) nil))))
               remove-list)
              (find-recursive-filter-out remove-list rest)
            (cons elem (find-recursive-filter-out remove-list rest))))))

    (defvar find-recursive-running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

    (if find-recursive-running-xemacs
        nil
      (defadvice directory-files (after
                                  directory-files-xemacs
                                  (dirname &optional full match nosort files-only)
                                  activate)
        "Add an additional argument, FILES-ONLY to the list of arguments
for GNU Emacs. If the symbol is t, then only the files in the
directory will be returned. If FILES-ONLY is nil, then both files and
directories are selected. If FILES-ONLY is not nil and not t, then
only sundirectories are returned."
        (setq ad-return-value
              (cond ((null files-only) ad-return-value)
                    ((eq files-only t)
                     (find-recursive-remove-if (lambda (f)
                                                 (file-directory-p
                                                  (concat dirname "/" f)))
                                               ad-return-value))
                    (t
                     (find-recursive-remove-if (lambda (f)
                                                 (not (file-directory-p
                                                       (concat dirname "/" f))))
                                               ad-return-value)))))

      (defun find-recursive-remove-if (func list)
        "Removes all elements satisfying FUNC from LIST."
        (let ((result nil))
          (while list
            (if (not (funcall func (car list)))
                (setq result (cons (car list) result)))
            (setq list (cdr list)))
          (nreverse result))))





    ;; load-path enhancement
    (defun fni/find-org-files-recursively (this-directory)
      "Add THIS-DIRECTORY at the beginning of the load-path, if it exists."
      (when this-directory
        (when (file-directory-p this-directory)
          (let* ((this-directory (expand-file-name this-directory))
                 (files (directory-files this-directory t "^[^\\.]")))

            ;; completely canonicalize the directory name (*may not* begin with `~')
            (while (not (string= this-directory (expand-file-name this-directory)))
              (setq this-directory (expand-file-name this-directory)))

            (message "Searching for Org files in `%s'..." this-directory)
            (add-to-list 'load-path this-directory)

            (while files
              (setq dir-or-file (car files))
              (when (file-directory-p dir-or-file)
                  (fni/find-org-files-recursively dir-or-file))
              (setq files (cdr files)))))))



    ;; always start the overview on the current day!
    (setq org-agenda-start-on-weekday nil)

    ;; 10.1 set which files to search for TODO entries and scheduled items
    ;; (avoiding hidden files)
    (setq org-agenda-files
          (append (directory-files org-directory t "^[^\\.].*\\.org$")
                  (if (file-exists-p "~/Projects/")
                      (directory-files "~/Projects/" t "^[^\\.].*\\.org$")
                    nil)
                  (if (file-exists-p "~/Public/")
                      (directory-files "~/Public/" t "^[^\\.].*\\.txt$")
                    nil)
                  ))                    ; be careful that no
                                        ; `custom-set-variables' (at the end
                                        ; of your `.emacs') overrides this!

    ;; 10.3.1 include entries from the Emacs diary into Org mode's agenda
    (setq org-agenda-include-diary t)

    ;; DEPRECATED
    ;; ;; 10.3.1 weekly/daily agenda will always contain all TODO entries
    ;; (setq org-agenda-include-all-todo t)

    ;; 10.3.2 don't show scheduled entries in the global todo list
    (setq org-agenda-todo-ignore-scheduled t)

    ;; 10.3.2 don't show scheduled entries in the global todo list (until they
    ;; are within the warning period)
    (setq org-agenda-todo-ignore-deadlines t)

    ;; don't show entries with a date in the global todo list
    (setq org-agenda-todo-ignore-with-date t)

    ;; 10.3.2 don't check the sublevels of a TODO entry for TODO entries,
    ;; resulting in potentially much shorter TODO lists
    (setq org-agenda-todo-list-sublevels nil)

    ;; 10.4.2 settings for time grid for agenda display
    (setq org-agenda-time-grid '((daily require-timed)
                                 "________"
                                 (0800 1000 1200 1400 1600 1800 2000 2200)))

    ;; 10.4.3 sorting structure for the agenda items of a single day
    ;; (setq org-agenda-sorting-strategy   ; original value
    ;;       '((agenda habit-down time-up priority-down category-keep)
    ;;         (todo priority-down category-keep)
    ;;         (tags priority-down category-keep)
    ;;         (search category-keep)))

    (setq org-agenda-sorting-strategy   ; custom value
          '((agenda time-up category-up priority-down)
            (todo priority-down category-keep)
            (tags priority-down category-keep)
            (search category-keep)))

    ;; 10.5 number of days to include in overview display
    (setq org-agenda-ndays 7)  ; 1 or 7





;;    (setq org-sort-agenda-notime-is-late nil)

    (setq org-agenda-restore-windows-after-quit t)

    (setq org-agenda-window-frame-fractions '(1.0 . 1.0))



    ;; 10.5 Commands in the agenda buffer
    (defun my/weekday-p ()
      "Check if ..."
      (let ((wday (nth 6 (decode-time))))
        (and (< wday 6) (> wday 0))))

    (defun my/working-p ()
      "Check if ..."
      (let ((hour (nth 2 (decode-time))))
        (and (my/weekday-p) (or (and (>= hour 8) (<= hour 11))
                                (and (>= hour 13) (<= hour 17))))))

    (defun my/online-p ()
      "Check if the Internet is available."
      (= 0 (call-process "/bin/ping" nil nil nil
                         "-c1" "-q" "-t1" "www.gnu.org")))


    ;; I've submitted a feature today which provide contextual auto-exclusion
    ;; for tags in the Agenda view. For example, I use the following tags for
    ;; TODOs:
    ;;
    ;; Net      Needs internet access
    ;; Call     Needs a phone
    ;; Errand   Done in town
    ;; Home     Done at home
    ;;
    ;; Now, it's quite easy for my computer to figure out which of these are
    ;; possible, based on my location:
    ;;
    ;; Net      Can I ping mail.gnu.org?
    ;; Call     Am I outside of normal calling hours?
    ;; Errand   Am I outside of business hours?
    ;; Home     Does my IP address begin with 192.168.9?
    ;;
    ;; With the patch I've submitted, I can now define this function to auto-
    ;; exclude based on this type of context information:
    (defun org-my-auto-exclude-function (tag)
      (and (cond
            ((string= tag "online")
             (/= 0 (call-process "/sbin/ping" nil nil nil
                                 "-c1" "-q" "-t1" "mail.gnu.org")))
            ((string= tag "home")
             (with-temp-buffer
               (call-process "/sbin/ifconfig" nil t nil "en0" "inet")
               (goto-char (point-min))
               (not (re-search-forward "inet 192\\.168\\.9\\." nil t))))
            ((or (string= tag "errands")
                 (string= tag "phone"))
             (let ((hour (nth 2 (decode-time))))
               (or (< hour 8) (> hour 21)))))
           (concat "-" tag)))
    ;;
    ;; All I have to do is type `/ RET' in the agenda view now, and it
    ;; excludes based on my machine's current temporal and physical context.



    ;;! ensure that `:refile:' tags never will be excluded!
    (defun my/org-auto-exclude-function (tag)
      (and (cond
            ((string= tag "home")
             (my/working-p))
            ((string= tag "work")
             (not (my/working-p)))
            ((or (string= tag "errands") (string= tag "phone"))
             (let ((hour (nth 2 (decode-time))))
               (or (< hour 8) (> hour 21)))))
           (concat "-" tag)))

    (setq org-agenda-auto-exclude-function 'my/org-auto-exclude-function)

    ;; faces for specific Priorities (#A, #B and #C)
    (setq org-priority-faces
          '((?A . (:foreground "red" :background "white" :weight bold :underline t))
            (?B . (:foreground "#00BB00" :background "white"))
            (?C . (:foreground "blue" :background "white" :italic t))
            ))

    ;; 10.6.3 custom commands for the agenda
    ;; Or use your agenda and use `/' to limit the view to what you want
    ;; (`C-c a a / TAG').

    ;; See http://tiddlywiki.org/wiki/MGSD/Users_Guide
    ;; and http://tiddlywiki.org/wiki/Getting_started_with_MonkeyGTD_2.1_alpha

    ;; Review from mGSD
    ;; - Projects Dashboard
    ;; - Projects Dashboard by Area
    ;; - Action Dashboard by Project
    ;; - Active Projects With No Next Action
    ;; - Someday Projects With No Tickler
    ;; - Completed Projects
    ;; - Done Actions
    ;; - SomedayMaybe and Future
    ;; - Delegated Tasks Dashboard
    ;; - Cleanup
    ;; - Mismatched Realms
    ;; - Subprojects

    ;; (setq org-agenda-custom-commands
    ;;       '(("f" "Agenda without Items tagged xyz"
    ;;          ((agenda ""))
    ;;          ((org-agenda-show-log nil)
    ;;           (org-agenda-ndays 1)
    ;;           (org-agenda-log-mode-items '(state))
    ;;           (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":XYZ:"))))
    ;;         ;; other commands here
    ;;         ("d" "Agenda only Items tagged xyz"
    ;;          ((agenda ""))
    ;;          ((org-agenda-show-log nil)
    ;;           (org-agenda-ndays 1)
    ;;           (org-agenda-log-mode-items '(state))
    ;;           (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":XYZ:"))))
    ;;         ;; other commands here
    ;;         ("w" todo "WAIT")
    ;;         ("g" todo "STARTED")
    ;;         ))

    (setq org-agenda-custom-commands
          '(
            ("W" "Work agenda"
             ((tags-todo "work")
              (tags-todo "office/!+TODO")
;;;                          ((org-agenda-overriding-header "Office\n-------"))
              (tags-todo "work+phone")
              (tags-todo "work+computer")
              (tags-todo "work+online")))
            ("H" "Home agenda"
             (
              (tags "home")
;;;           (tags-todo "home")
;;;           (tags-todo "phone")
;;;           (tags-todo "computer")
;;;           (tags-todo "online")
             ))

;;;             ("D" "Daily Action List"
;;;              ((agenda ""
;;;                       ((org-agenda-ndays 1)
;;;                        (org-agenda-sorting-strategy
;;;                         '((agenda time-up priority-down tag-up)))
;;;                        (org-deadline-warning-days 0)))))
            ("d" "Daily Agenda"
             ((agenda ""
                      ((org-agenda-todo-keyword-format "")
                       (org-agenda-remove-tags t)))
              (tags "LEVEL=2+goals"
                    ((org-agenda-remove-tags t)
                     (org-agenda-prefix-format "  ")
                     (org-agenda-todo-keyword-format "")))
              (todo "NEXT"
                    ((org-agenda-sorting-strategy '(tag-up))
                     (org-agenda-show-inherited-tags nil)
                     (org-agenda-todo-keyword-format "")))
              (todo "PENDING"
                    ((org-agenda-todo-keyword-format "")))
              (stuck ""
                     ((org-agenda-remove-tags t)))))

            ))

    (setq org-agenda-custom-commands
          '(
            ("p" "Printed agenda"
             ((agenda ""
                      ((org-agenda-ndays 7)
                       (org-agenda-start-on-weekday nil)
                       (org-agenda-time-grid nil)
                       (org-agenda-repeating-timestamp-show-all t)
                       (org-agenda-prefix-format "  -->  %t %s")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-sorting-strategy '(time-up tag-up))
                       (org-agenda-todo-keyword-format "[ ]")
                       (org-agenda-scheduled-leaders '("" ""))
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                       (org-agenda-prefix-format "%t %T %s")
                       ))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-deadline-warning-days 7)
                       (org-agenda-time-grid nil)
                       (org-agenda-include-diary nil)
                       (org-agenda-todo-keyword-format "[ ]")
                       (org-agenda-scheduled-leaders '("" ""))
                       (org-agenda-overriding-header "Deadlines:")
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))
                       (org-agenda-prefix-format "%t %s")))
              (todo "TODO|STARTED|NOW"
                    ((org-agenda-sorting-strategy '(tag-up priority-down))
                     (org-agenda-todo-ignore-with-date t)))
              (todo "WAIT"))
             ((org-agenda-with-colors nil)
              (org-agenda-prefix-format "%T [ ]")
              (org-agenda-todo-keyword-format "")
              (org-agenda-include-all-todo nil)
              (org-agenda-block-separator "---------------\n")
              (org-agenda-remove-tags t)
              (ps-number-of-columns 2)
              (ps-print-header nil)
              (ps-landscape-mode t))
             ("~/storage/agenda/agenda.pdf"))
            ("n" "Now"
             ((todo "NOW|STARTED")
              ((org-agenda-todo-ignore-with-date nil))))
            ("h" "Habits"
             ((agenda ""))
             ((org-agenda-show-log t)
              (org-agenda-include-diary nil)
              (org-agenda-include-all-todo nil)
              (org-agenda-ndays 1)
              (org-agenda-start-on-weekday nil) 
              (org-agenda-log-mode-items '(state))
              (org-agenda-time-grid nil)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
            ;; Today - daily tasks view
            ("d" "Today"
             ((agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-include-all-todo nil)))
              (alltodo "" ((org-agenda-sorting-strategy '(todo-state-up))
                           (org-agenda-todo-ignore-with-date t)))))
            ("y" "Projects"
             ((agenda ""
                      ((org-agenda-skip-function
                        '(org-agenda-skip-entry-if 'notregexp "* PROJECT")))
                      ((org-agenda-include-diary nil)
                       (org-agenda-include-all-todo nil)
                       (org-agenda-time-grid nil)))
              (todo "PROJECT" ((org-agenda-todo-ignore-deadlines-t)
                               (org-agenda-sorting-strategy '(priority-down))))))
            ))


    ;; (key desc type match settings files)
    (setq org-agenda-custom-commands
          '(

            ("R" "Review"
             ((stuck "")
              (agenda ""
                      ((org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
              (agenda ""
                      ((org-agenda-ndays 1)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))
              (todo "PROJECT"
                    ((org-agenda-sorting-strategy '(todo-state-down priority-down))))
              (todo "NOW|STARTED|TODO"
                    ((org-agenda-sorting-strategy '(todo-state-down priority-down))))
              (todo "WAIT")
              (todo "MAYBE"))
             ((org-agenda-todo-ignore-with-date t)
              (org-agenda-todo-ignore-deadlines t)
              (org-agenda-time-grid nil)
              (org-agenda-include-all-todo nil)
              (org-deadline-warning-days 360)))


;;; Calendar style views

            ("d" agenda "X-Agenda 0 days deadline preview"
             ((org-deadline-warning-days 0)))
            ("D" agenda "X-Agenda 1 days deadline preview"
             ((org-deadline-warning-days 1)))
            ("2" agenda "X-Agenda 2 days deadline preview"
             ((org-deadline-warning-days 2)))
            ("3" agenda "X-Agenda 3 days deadline preview"
             ((org-deadline-warning-days 3)))
            ("7" agenda "X-Agenda 7 days deadline preview"
             ((org-deadline-warning-days 7)))

            ;; from Matt Lundin
            ("C" "Daily appointments"
             agenda ""
             ((org-agenda-ndays 1)
              (org-agenda-time-grid nil)
              (org-agenda-prefix-format " %-12:t ")
              (org-agenda-include-all-todo nil)
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("c" "Schedule" agenda ""
             ((org-agenda-ndays 7)
              (org-agenda-start-on-weekday 1)
              (org-agenda-time-grid nil)
              (org-agenda-prefix-format " %12:t ")
              (org-agenda-include-all-todo nil)
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("r" "Weekly appointments"
             agenda ""
             (
              ;; agenda will start in week view
              (org-agenda-ndays 7)

              ;; ensures that repeating events appear on all relevant dates
              (org-agenda-repeating-timestamp-show-all t)

              ;; limits agenda view to timestamped items
              (org-agenda-skip-function
               '(org-agenda-skip-entry-if 'deadline 'scheduled))))

            ("E" "Week's errands"
             tags "errands&SCHEDULED<=\"<+1w>\"&TODO<>\"DONE\"&TODO<>\"CANCELED\"" nil)

            ;; Past due
            ("H" "Due in the next 3 days"
             agenda ""
             ((org-agenda-entry-types '(:deadline))
              (org-deadline-warning-days 3)))

            ;; Toodledo
            ;; Hotlist: The hotlist contains tasks that are due soon as well
            ;; as tasks that have a high priority. This is a convenient way to
            ;; see your most important tasks at a glance.

            ;; Hotlist Settings: At least a priority of '3 Top' or a due date
            ;; in the next 14 days.


            ;; Carsten
            ;; FIXME We don't see "timed" DEADLINE
            ("D" "Due today"
             agenda ""
             ((org-agenda-ndays 1)
              (org-deadline-warning-days 0)
              (org-agenda-skip-scheduled-if-deadline-is-shown t)
              (org-agenda-skip-function
               (lambda ()
                 (let* ((dl (org-entry-get nil "DEADLINE")))
                   (if (or (not dl)
                           (equal dl "")
                           (org-time> dl (org-time-today)))
                       (progn (outline-next-heading) (point))))))))

            ("F" "Upcoming deadlines (6 months)"
             ;; FIXME We don't see DEADLINE with `-1m' (or so) specifications
             ;; (if they are more than 1 m ahead of now)!
             agenda ""
             ((org-agenda-ndays 1)
              (org-deadline-warning-days 180)
              (org-agenda-include-all-todo nil)
              (org-agenda-time-grid nil)
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'notdeadline))))
            ;; Some SCHEDULED are shown

;;; Stuck

            ("u" "Someday"
             alltodo ""
             ((org-agenda-skip-function
               (lambda nil
                 (org-agenda-skip-entry-if 'scheduled 'deadline
                                           'regexp "<[^>\n]+>")))
              (org-agenda-overriding-header "Unscheduled TODO entries: ")))


;;; Priorities

            ;; priority levels
            ("p" . "Priorities")

            ("pa" "A items"
             tags-todo "+PRIORITY=\"A\"")

            ("pb" "B items"
             tags-todo "+PRIORITY=\"B\"")

            ("pc" "C items"
             tags-todo "+PRIORITY=\"C\"")

            ;; list only priority A tasks for the current day
            ("pA" "Agenda of priority A tasks for current day"
             agenda ""
             ((org-agenda-skip-function
               (lambda nil
                 (org-agenda-skip-entry-if 'notregexp "\\=.*\\[#A\\]")))
              (org-agenda-ndays 1)
              (org-agenda-overriding-header "Today's Priority #A tasks: ")))

            ;; list only priority A and B tasks for the current day
            ("pB"
             agenda ""
             ((org-agenda-ndays 1)
              (org-agenda-overriding-header "Today's Priority #A and #B tasks: ")
              (org-agenda-skip-function
               (quote (org-agenda-skip-entry-if 'regexp "\\=.*\\[#C\\]")))))


;;; GTD contexts

            ("g" . "Context lists")

            ("gw" "Work"
             tags-todo "work")

            ("go" "Online"
             tags-todo "online")

            ("gc" "Computer"
             tags-todo "computer")

            ("gp" "Phone"
             tags-todo "phone")

            ("gh" "Home"
             tags-todo "home")

            ("gr" "Reading"
             tags-todo "reading")

            ("ge" "Errands"
             tags-todo "errands")

            ("G" "Group actions by Context"
             ;; contains unscheduled things...!
             ((tags-todo "work")
              (tags-todo "online")
              (tags-todo "computer")
              (tags-todo "phone")
              (tags-todo "home")
              (tags-todo "reading")
              (tags-todo "errands")))

            ;; ("s" "Scorpios tasks"
            ;;  alltodo ""
            ;;  ((org-agenda-files
            ;;    '("~/Personal/Business/Real-Estate/Appartment-Scorpios-Boulouris/TODO-Documents.org"))))


;;; TODO keyword

            ;; list entries with a DELEGATED keyword, in all agenda files
            ("d"
             todo "DELEGATED" nil)

            ;; list entries which are at some kind of completion state (DONE,
            ;; DEFERRED or CANCELED), in all agenda files
            ("c"
             todo "DONE|DFRD|CANCELED" nil)

            ;; FIXME This does not show any (or all?) WAITING task!
            ("W" "Waiting"
             todo "WAIT" nil)


;;; Printed agenda

            ;; `c-x c-w' = write the agenda view to a file
            ("n" "Call list"
             tags-todo "phone"
             ((ps-number-of-columns 1)
              (ps-landscape-mode t)
              (org-agenda-prefix-format " %-20:c [ ] " )
              (Orgae-agenda-with-colors nil)
              (org-agenda-remove-tags t))
             ;; ("~/My Dropbox/calls.ps")
             ("/home/sva/calls.pdf"))


;;; Custom queries

            ("Q" . "Custom queries") ;; gives label to `Q'

            ("Qd" "Notes files"
             search ""
             ((org-agenda-files
               (file-expand-wildcards "~/Public/*.txt"))))

            ("Qw" "Website search"
             search ""
             ((org-agenda-files
               (file-expand-wildcards "~/Public/Websites/Org/source/*.org"))))

            ("Qa" "Artemis search"
             search ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))))

            ("Qx" "Artemis with deadline columns"
             alltodo ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))
              (org-agenda-overriding-columns-format "%40ITEM %DEADLINE")
              (org-agenda-view-columns-initially t)))

            ("QA" "Artemis tags search"
             org-tags-view ""
             ((org-agenda-files
               (file-expand-wildcards
                "~/Projects/Transport/SNCB/admin/Timesheets/*.txt"))))

            ("n" . "SNCB+Name tags searches") ; description for `n' prefix

            ("nb"
             tags "+sncb+Be")

            ("no"
             tags "+sncb+Oz")

            ("nf"
             tags "+sncb+FNI")

            ("ni"
             tags "+sncb+IML")

            ("nl"
             tags "+sncb+LLG")

            ("nm"
             tags "+sncb+MVA")

            ("np"
             tags "+sncb+PRO")

            ("nr"
             tags "+sncb+RCO")

            ("nw"
             tags "+sncb+PWA")



            ;; Carsten
            ;; Is it listing only tasks *without* DEADLINE?
            ("x" "With deadline columns"
             alltodo ""
             ((org-agenda-overriding-columns-format "%40ITEM %DEADLINE")
              (org-agenda-view-columns-initially t)))



            ("N" "Notes" tags "note" nil)

            ))


    ;; ;; (setq org-agenda-deadline-leaders '("Deadline:  " "In %3d d.: "))
    ;; (setq org-agenda-deadline-leaders '("Deadl.:  " "In %2dd: "))

    ;; ;; text preceeding scheduled items in the agenda view
    ;; (setq org-agenda-scheduled-leaders '("Sched.: " "S. %2dx: "))
    ;;                                     ; ("⌚ " "Sched.%2dx: "))

    ;; DUPLICATE Obey `eval' variables -- RISKY!
    (setq enable-local-eval t)

    ;; generate -- after all initialization --the agenda buffer for this week
    ;; (add-hook 'after-init-hook 'org-agenda-list)


    (org-agenda-list)
    (delete-other-windows)





    ;; I have the following snippet in my .emacs file, which I find very
    ;; useful. Basically what it does is that if I don't touch my Emacs for 15
    ;; minutes, it displays the current agenda. This keeps my tasks "always in
    ;; mind" whenever I come back to Emacs after doing something else, whereas
    ;; before I had a tendency to forget that it was there.
    (defun jump-to-org-agenda ()
      (interactive)
      (let ((buf (get-buffer "*Org Agenda*"))
            wind)
        (if buf
            (if (setq wind (get-buffer-window buf))
                (select-window wind)
              (if (called-interactively-p)
                  (progn
                    (select-window (display-buffer buf t t))
                    (org-fit-window-to-buffer)
                    ;; (org-agenda-redo)
                    )
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )))
          (call-interactively 'org-agenda-list)))
      ;;(let ((buf (get-buffer "*Calendar*")))
      ;;  (unless (get-buffer-window buf)
      ;;    (org-agenda-goto-calendar)))
      )

    (run-with-idle-timer 900 t 'jump-to-org-agenda)



    (defun my/highlight-line ()
      (hl-line-mode 1))

    (add-hook 'org-agenda-mode-hook 'my/highlight-line)



;;**** 11 (info "(org)Embedded LaTeX")

    ;; 11.4 convert LaTeX fragments to images when exporting to HTML
    (setq org-export-with-LaTeX-fragments t)


;;**** 12 (info "(org)Exporting")

    ;; special syntax for emphasized text
    (setq org-emphasis-alist '(("*" bold "<b>" "</b>")
                               ("/" italic "<i>" "</i>")
                               ("_" underline "<span style=\"text-decoration:underline;\">" "</span>")
                               ("=" org-code "<code>" "</code>" verbatim)
                               ("~" org-verbatim "<code>" "</code>" verbatim)
                               ("+" (:strike-through t) "<del>" "</del>")
                               ("@" org-warning "<b>" "</b>")))

    ;; alist of LaTeX expressions to convert emphasis fontifiers
    (setq org-export-latex-emphasis-alist '(("*" "\\textbf{%s}" nil)
                                            ("/" "\\emph{%s}" nil)
                                            ("_" "\\underline{%s}" nil)
                                            ("+" "\\st{%s}" nil)
                                            ("=" "\\verb=%s=" nil)
                                            ("~" "\\verb~%s~" t)
                                            ("@" "\\alert{%s}" nil)))



    ;; ASCII export for Org mode
    (try-require 'org-ascii)

    ;; ;; 12.1 add the unbreakable space as allowed character after an emphasis
    ;; ;; string, and modify the maximum number of newlines allowed in an
    ;; ;; emphasis
    ;; (setq org-emphasis-regexp-components [...])

    ;; 12.7.3 LaTeX exporter for Org mode
    (when (try-require 'org-latex)

      ;; update the list of LaTeX classes and associated header (encoding,
      ;; etc.) and structure
      (add-to-list 'org-export-latex-classes
                   '("mcarticle"
                     "\\documentclass{mcarticle}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mccommercial"
                     "\\documentclass{mccommercial}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mcreport"
                     "\\documentclass{mcreport}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mcbook"
                     "\\documentclass{mcbook}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("alta"
                     "\\documentclass{alta}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\chapter{%s}" . "\\chapter*{%s}")
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")))

      (add-to-list 'org-export-latex-classes
                   '("mccontract"
                     "\\documentclass{mccontract}\n\\usepackage[AUTO]{inputenc}\n"
                     ("\\mccarticle{%s}" . "\\mccarticle*{%s}")
                     ("\\mccparagraph{%s}" . "\\mccparagraph*{%s}")))

      ;; XXX 2010-03-25 TEMP Fix for conflict TikZ/hyperref: loading atbegshi
      ;; before document class beamer
      (add-to-list 'org-export-latex-classes
                   '("beamer"
                     "\\RequirePackage{atbegshi}\n\\documentclass{beamer}\n"
                     org-beamer-sectioning))

      (setq org-beamer-frame-default-options "")

      ;; ;; "Org2Beamer" (TODO lookat Goettingen theme)
      ;; \\usetheme{Goettingen}
      ;; \\useoutertheme{infolines}
      ;; \\setbeameroption{show notes}
      ;;
      ;; \\mode<{{{beamermode}}}>
      ;; {
      ;; \\usetheme{{{{beamertheme}}}}
      ;; }
      ;; \\institute{{{{beamerinstitute}}}}
      ;; \\subject{{{{beamersubject}}}}"


      ;; 2010-03-31 From Carsten
      ;; =org-export-latex-classes= no longer should be customized for
      ;; packages
      ;;
      ;; The HEADER part of this variable should now only contain the
      ;; documentclass macro, nothing else - at least normally.  All the
      ;; package calls via usepackage should go into
      ;; org-export-latex-packages-alist.  I moved all the default packages
      ;; that into a new variable org-export-latex-default-packages-alist.
      ;; This will allow me to add more packages (as needed) in the
      ;; future, withour requiring you to erase and then redo your
      ;; configuration of org-export-latex-classes.
      ;;
      ;; So if you have customized this variable, please remove once more
      ;; (hopefully for the last time) your customization, so that it can
      ;; revert to its now much simpler default value.  Put all your
      ;; package definitions into org-export-latex-packages-alist.
      ;; I hope this works, and we will not get conflicts because of the
      ;; sequence in which packages are called.  If there are problems,
      ;; please let me know so that we can find a solution.

      (setq org-export-latex-default-packages-alist
            '(("AUTO" "inputenc" t)
              ("T1" "fontenc" t)
              ("" "fixltx2e" nil)
              ("" "graphicx" t)
              ("" "longtable" nil)
              ("" "float" nil)
              ("" "wrapfig" nil)
              ("" "soul" t)
              ("" "t1enc" t)
              ("" "textcomp" t)
              ("" "marvosym" t)
              ("" "wasysym" t)
              ("" "latexsym" t)
              ("" "amssymb" t)
              ("" "hyperref" nil)
              "\\tolerance=1000"))

      ;; tell org to use listings
      (setq org-export-latex-listings t)

      ;; you must include the `listings' package
      (add-to-list 'org-export-latex-packages-alist '("" "listings"))

      ;; if you want colored source code, then you need to include the
      ;; `xcolor' package
      (add-to-list 'org-export-latex-packages-alist '("" "xcolor"))

      (setq org-export-latex-packages-alist
            '(("" "xcolor")
              ("" "listings")))


      ;; default class
      ;; TODO Put this in a personal settings file
      (setq org-export-latex-default-class "article")


      (setq org-export-copy-to-kill-ring nil)

      )


;;**** 13 (info "(org)Publishing")

    ;; publish related Org mode files as a website
    (try-require 'org-publish)

    ;; 13.1.1 association list to control publishing behavior
    (setq org-publish-project-alist
          '(("project-mygooglest.com-fni"

             :base-directory "~/Public/www.mygooglest.com/source/fni/"
             :base-extension "txt"
             :publishing-directory "~/Public/www.mygooglest.com/public_html/fni/"

             :publishing-function org-publish-org-to-html

             :section-numbers nil
             :table-of-contents t
             :style-include-default nil
             :style "
<link rel=\"shortcut icon\" href=\"pic/favicon.ico\"/>
<link rel=\"stylesheet\" href=\"css/common.css\" type=\"text/css\"/>
<!--[if IE]>
    <link rel=\"stylesheet\" href=\"css/common-ie.css\" type=\"text/css\" title=\"IE specific stylesheet\"/>
<![endif]-->"
             :preamble "
<!-- Preamble of Page published by Emacs Org mode begins here -->
    <div id=\"navigation\">
        <h2>Navigation</h2>
        <ul>
            <li><a href=\"index.html\" title=\"Home\" id=\"current-home\">Home</a></li>
            <li>About Me
                <ul>
                    <li><a href=\"curriculum-vitae.html\" title=\"CV\" id=\"current-cv\">CV</a></li>
                    <li><a href=\"pgp-public-key.html\" title=\"PGP Public Key\" id=\"current-pgp-public-key\">PGP Public Key</a></li>
                    <li><a href=\"contact-me.html\" title=\"Contact Me\" id=\"current-contact-me\">Contact Me</a></li>
                </ul>
            </li>
            <li>Resources
                <ul>
                    <li><a href=\"ubuntu.html\" title=\"Ubuntu\" id=\"current-ubuntu\">Ubuntu</a></li>
                    <li><a href=\"dot-emacs.html\" title=\"Emacs\" id=\"current-emacs\">Emacs</a></li>
                    <li><a href=\"emacs-lisp-programming.html\" title=\"Emacs Lisp\" id=\"current-emacs-lisp\">Emacs Lisp</a></li>
                    <li><a href=\"shell-scripting.html\" title=\"Shell Scripting\" id=\"current-shell-scripting\">Shell Scripting</a></li>
                    <li><a href=\"freeware.html\" title=\"Freeware\" id=\"current-freeware\">Freeware</a></li>
                    <li><a href=\"latex.html\" title=\"LaTeX\" id=\"current-latex\">LaTeX</a></li>
                    <li><a href=\"virtualbox.html\" title=\"VirtualBox\" id=\"current-virtualbox\">VirtualBox</a></li>
                    <li><a href=\"google.html\" title=\"Google\" id=\"current-google\">Google</a></li>
                    <li><a href=\"ledger.html\" title=\"Ledger\" id=\"current-ledger\">Ledger</a></li>
                    <li><a href=\"stumpwm.html\" title=\"StumpWM\" id=\"current-stumpwm\">StumpWM</a></li>
                    <li><a href=\"networking.html\" title=\"Networking\" id=\"current-networking\">Networking</a></li>
                    <li><a href=\"electronics.html\" title=\"Electronics\" id=\"current-electronics\">Electronics</a></li>
                </ul>
            </li>
            <li>About this Site
                <ul>
                    <li><a href=\"sitemap.html\" title=\"Site Map\" id=\"current-site-map\">Site Map</a></li>
                </ul>
            </li>
        </ul>
    </div>

    <div id=\"org-content\">
<!-- Preamble of Page published by Emacs Org mode ends here -->
"
             :postamble "
<!-- Postamble of Page published by Emacs Org mode begins here -->
    </div>

    <div id=\"footer\">
        <p>
            Copyright (C) 2007 - 2010 Fabrice Niessen<br/>
            Please <a href=\"contact-me.html\">contact me</a> for corrections, additions and suggestions.<br/>
            Made with <a href=\"http://www.orgmode.org/\">Org Mode</a>.<br/>
            <a href=\"http://validator.w3.org/check?uri=referer\"
            title=\"Check the validity of this site's XHTML\"
            style=\"text-decoration: none;\">
            <img src=\"http://www.w3.org/Icons/valid-xhtml10-blue\"
            alt=\"Valid XHTML 1.0!\" height=\"31\" width=\"88\"/>
            </a> &nbsp; 
            <a href=\"http://jigsaw.w3.org/css-validator/check/referer\"
            title=\"Check the validity of this site's CSS\"
            style=\"text-decoration: none;\">
            <img src=\"http://www.w3.org/Icons/valid-css-blue.png\"
            alt=\"Valid CSS!\" height=\"31\" width=\"88\"/>
            </a> &nbsp;
        </p>
    </div>
<script type=\"text/javascript\">
    var gaJsHost = ((\"https:\" == document.location.protocol) ? \"https://ssl.\" : \"http://www.\");
    document.write(unescape(\"%3Cscript src='\" + gaJsHost + \"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));
</script>
<script type=\"text/javascript\">
    var pageTracker = _gat._getTracker(\"UA-192135-1\");
    pageTracker._initData();
    pageTracker._trackPageview();
</script>
<!-- Postamble of Page published by Emacs Org mode ends here -->
"
             :auto-preamble t  ; FIXME How to get TITLE before navigation menu?
             :auto-postamble nil

             :auto-sitemap t                  ; Generate index.org automagically...
             :sitemap-filename "sitemap.org"  ; ... call it sitemap.org ...
             :sitemap-title "Sitemap"         ; ... with title 'Sitemap'.
             )))

    ;; fontify what is treated specially by the exporters
    (setq org-highlight-latex-fragments-and-specials t)

    ;; 13.1.5 default option for images
    (setq org-export-latex-image-default-option "width=0.9\\linewidth")

    (setq org-export-latex-inputenc-alist '(("utf8" . "utf8x")))

    ;; 13.1.5 don't include the javascript snippets in exported HTML files
    (setq org-export-html-style-include-scripts nil)

    ;; XML declaration for exported HTML files
    (setq org-export-html-xml-declaration
          '(("html" . "")
            ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")
            ("php" . "<?php echo \"<?xml version=\\\"1.0\\\" encoding=\\\"%s\\\" ?>\"; ?>")))

    ;; ;; 13.1.5 export all drawers
    ;; (setq org-export-with-drawers t)

    ;; 13.2 always publish all files (do not use timestamp checking for
    ;; skipping unmodified files)
    (setq org-publish-use-timestamps-flag nil)


    ;; indentation for the content of a source code block
    (setq org-edit-src-content-indentation 4)

    ;; switch from org-exp-blocks to Org-babel!
    ;; ;; pre-process blocks when exporting org files (ditaa, dot, comment, R,
    ;; ;; etc.)
    ;; (try-require 'org-exp-blocks)

    ;;;; FIXME This is responsible of problems when used with Org-babel
    ;;;; (`\LaTeX{}' environments are created around verbatim environments)
    ;; turn Org blocks into LaTeX environments and HTML divs
    ;; (markup in environments in LaTeX export, or giving LaTeX attributes to
    ;; sections in export)
    (try-require 'org-special-blocks)


;;**** 14 Working with source code

    ;; literate programming
    ;; (when (try-require 'org-babel-init)  ; 2010-06-24 now loaded by default
    ;;                                      ; with Org

      ;; control the insertion of comments into tangled code
      (setq org-babel-tangle-w-comments t)

      ;; activate a subset of languages
      (try-require 'ob-sh)
      (try-require 'ob-ditaa)           ; TODO Install this for Ditaa
                                        ; sudo aptitude install openjdk-6-jre
      (try-require 'ob-dot)
      (try-require 'ob-emacs-lisp)
      (try-require 'ob-gnuplot)         ; requires gnuplot-mode
      (try-require 'ob-latex)
      (try-require 'ob-perl)
      (try-require 'ob-python)          ; requires python, and python-mode
      (try-require 'ob-R)               ; requires R and ess-mode
      (try-require 'ob-ruby)            ; requires ruby, irb, ruby-mode, and
                                        ; inf-ruby
      (try-require 'ob-sql)


;;      (require 'ob-identifier) ; ????????

      ;; support for interactive terminals. Mostly shell scripts. Heavily
      ;; inspired by 'eev'
      (try-require 'ob-screen)
                                        ; Eric Schulte believes screen has
                                        ; more of a focus on sustained
                                        ; interaction with an interactive
                                        ; terminal, although to be honest I
                                        ; haven't really used it and

      ;; load the source-code blocks defined in an Org mode file into the
      ;; global `org-babel-library-of-babel' variable
      (when (fboundp 'org-babel-lob-ingest)
        (org-babel-lob-ingest
         "~/Downloads/emacs/site-lisp/org-mode/contrib/babel/library-of-babel.org"))

      ;; I don't want to be prompted on every code block evaluation
      (setq org-confirm-babel-evaluate nil)

      ;; I don't want to execute code blocks with `C-c C-c' (evaluate code
      ;; block with `C-c C-v e')
      (setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

      ;; mapping between languages (listings in LaTeX) and their major mode
      ;; (in Emacs)
      (setq org-src-lang-modes
            '(("ocaml" . tuareg)
              ("elisp" . emacs-lisp)
              ;; ("Delphi" . perl)
              ("ditaa" . artist)
              ("asymptote" . asy)
              ("dot" . fundamental)))
      ;; )


;;**** 14 (info "(org)Miscellaneous")

    ;; 14.2 speed keys
    ;; activate single letter commands (for example outline navigation with
    ;; `f', `b', `n', and `p') at beginning of a headline:
    ;; - f :: org-forward-same-level
    ;; - b :: org-backward-same-level
    ;; - n :: outline-next-visible-heading
    ;; - p :: outline-previous-visible-heading
    (setq org-use-speed-commands t)

    ;; show next/prev heading tidily (from Dan Davison)
    (defun my/org-show-next-heading-tidily ()
      "Show next entry, keeping other entries closed."
      (if (save-excursion (end-of-line) (outline-invisible-p))
          (progn (org-show-entry) (show-children))
        (outline-next-heading)
        (unless (and (bolp) (org-on-heading-p))
          (org-up-heading-safe)
          (hide-subtree)
          (error "Boundary reached"))
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)))

    (defun my/org-show-previous-heading-tidily ()
      "Show previous entry, keeping other entries closed."
      (let ((pos (point)))
        (outline-previous-heading)
        (unless (and (< (point) pos) (bolp) (org-on-heading-p))
          (goto-char pos)
          (hide-subtree)
          (error "Boundary reached"))
        (org-overview)
        (org-reveal t)
        (org-show-entry)
        (show-children)))

    (when (fboundp 'org-speed-commands-user)
      (add-to-list 'org-speed-commands-user
                   '("n" my/org-show-next-heading-tidily))
      (add-to-list 'org-speed-commands-user
                   '("p" my/org-show-previous-heading-tidily)))





    (if window-system (try-require 'org-mouse))



    ;; keep my encrypted data (like account passwords) in my org-mode files
    ;; with a special tag instead
    (when (try-require 'org-crypt)

        ;; encrypt all entries before saving
        (org-crypt-use-before-save-magic)

        ;; To later decrypt an entry that's encrypted, use `M-x
        ;; org-decrypt-entry' or `C-c C-r' (fits nicely with the meaning of
        ;; "reveal").

        ;; which tag is used to mark headings to be encrypted
        (setq org-tags-exclude-from-inheritance '("crypt"))

        ;; GPG key to use for encryption
        (setq org-crypt-key "7F376D89"))



    ;; 14.5 hide the first N-1 stars in a headline
    (setq org-hide-leading-stars t)

    ;; ;; 14.5 skip even levels and only use odd levels for the outline
    ;; (setq org-odd-levels-only t)
    ;;
    (when (try-require 'org-indent)
      (org-indent-mode))

;;    (setq org-startup-indented t)


    ;; 14.7.2 yasnippet (allow yasnippet to do it's thing in Org files)
    ;;! make sure you initialise yasnippet *before* Org mode
    ;; FIXME Make sure yasnippet is found and loaded before executing this
    (when (locate-library "yasnippet.el")
      (add-hook 'org-mode-hook
                (lambda ()
                  (defun yas/org-very-safe-expand ()
                    (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

                  ;; yasnippet (using the new org-cycle hooks)
                  ;; (make-variable-buffer-local 'yas/trigger-key)
                  ;; (setq yas/trigger-key (kbd "TAB"))
                  (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                  (define-key yas/keymap (kbd "TAB") 'yas/next-field))))


    (defun my/open-work-org ()
      "Open my Work Org file."
      (interactive)
      (find-file "~/Projects/Work.org"))
    (global-set-key (kbd "<S-f2>") 'my/open-work-org)

    (defun my/open-personal-org ()
      "Open my Personal Org file."
      (interactive)
      (find-file "~/Personal/Personal.org"))
    (global-set-key (kbd "<C-f2>") 'my/open-personal-org)



    ;; http://gopher.ruricolist.com/lisp/org-velocity.el
    (when (try-require 'org-velocity)
      (setq org-velocity-bucket (concat org-directory "bucket.org"))
      (global-set-key (kbd "C-c v") 'org-velocity-read))




;;;     Use Org mode together with org-annotate-file.el, this is mentioned at
;;;     http://orgmode.org/worg/org-contrib/index.php.

    )


;; How to create web page by Emacs and Muse?
;; http://www.xshi.org/notes/WebPage.html

;; load generic module
(GNUEmacs
  (when (try-require 'muse)

    ;;     (define-key muse-mode-map (kbd "C-c C-p") 'ywb-muse-publish-project)
    ;;     (define-key muse-mode-map (kbd "C-c C-c") 'ywb-muse-preview-source)
    ;;     (define-key muse-mode-map (kbd "C-c C-j") 'ywb-muse-preview-html)
    ;;     (define-key muse-mode-map (kbd "C-c C-m") 'ywb-muse-preview-with-w3m)

        (defun tidy-do ()
          "Run the HTML Tidy program on the current buffer."
          (interactive)
          (shell-command-on-region (point-min) (point-max)
            (concat "tidy"
                    " -config ~/.tidyrc"
                    " --error-file ./temp-tidy-errors") t)
          (delete-file "./temp-tidy-errors")
          (message "Buffer tidy'ed"))

;;;         ;; TODO Check that tidy is in PATH
;;;         ;; run in the buffer to be published
;;;         (eval-after-load "tidy"
;;;           '(progn (add-hook 'muse-after-publish-hook 'tidy-do)))


    ;; See `org-publish-escript.el' for htmlize of code


    ;; How to preview in a browser?
    ;;
    ;; Press `C-c C-v' (`browse-url-of-buffer'). You can also get a textual
    ;; preview by pressing `C-c TAB' (`sgml-tags-invisible'), which will hide
    ;; all the tags. Press `C-c TAB' again to show tags.


    ;;Auto Publish when muse saved
    ;;(eval-after-load "muse-mode"
    ;;    (add-hook 'after-save-hook
    ;;          (lambda ()
    ;;          (when (planner-derived-mode-p 'muse-mode)
    ;;                           (muse-project-publish nil)))
    ;;))
    ;;         nil t))

))

    ;; You can create and publish a blog with Org mode
    ;; http://dto.freeshell.org/e/org-blog.el

    ;; See `muse-project-copy-files' with `rsync'

    ;; ;; automatically update web copy
    ;; (write-region
    ;;  (point-min) (point-max)
    ;;  "/scorpios@ftp.scorpioscotedazur.com:/public_html/emacs/mydotemacs.el")

    ;; or `sitecopy'

(message "38 The Calendar and the Diary... Done"))


;; FIXME This variable has been reset by something in the previous section.
;; So, put back my value!
(setq ispell-dictionary-alist
      ;; TODO Add es_ES
      '(
        ;; default dictionary (see `ispell-dictionary')
        (nil
         "[a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
         "[^a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
         "[-']" nil
         ("-d" "fr_FR")  ; for hunspell
         nil utf-8)

        ;; standard French
        ("fr_FR"
         "[a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
         "[^a-zàâäéèêëîïôöùûüçA-ZÀÂÄÉÈÊËÎÏÔÖÙÛÜÇ]"
         "[-']" nil
         ("-d" "fr_FR")  ; for hunspell
         nil utf-8)

        ;; American English
        ("en_US"
         "[A-Za-z]"
         "[^A-Za-z]"
         "[']" nil
         ("-d" "en_US")  ; for hunspell
         nil utf-8)
        ))


;;** 39 (info "(emacs)Document View")

(when section-document-view (message "39 Document Viewing...")

;; view PDF/PostScript/DVI files in Emacs
(when (try-require 'doc-view)

;;*** 39.1 (info "(emacs)Navigation")

  ;; `doc-view' integrates with the usual bookmark facility. So simply use
  ;; `C-x r m' (`bookmark-set') to jump back to the last page you've read
  ;; in a PDF document.


;;*** 39.4 (info "(emacs)Conversion")

  ;; DPI resolution used to render the documents
  ;; `doc-view-enlarge' (`+') and `doc-view-shrink' (`-') work fine to zoom
  ;; in or out
  (setq doc-view-resolution 96)

  ;; DPI your screen supports
  (setq doc-view-display-size 96)

  ;; You can open the *text* of the current doc in a new buffer, by pressing
  ;; `C-c C-t' in doc-view-mode
)

;; Another option, without `doc-view', is `! pdtotext ? - RET'


;; antiword will be run on every doc file you open
;; TODO sudo aptitude install antiword
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; un-xls
;; TODO sudo aptitude install xlhtml
(defun no-xls (&optional filename)
  "Run xlhtml and w3m -dump on the entire buffer.
Optional FILENAME says what filename to use.
This is only necessary for buffers without
proper `buffer-file-name'.  FILENAME should
be a real filename, not a path."
  (interactive "fExcel File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "xlhtml -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))

;; no-ppt
;; TODO sudo aptitude install ppthtml
;; FIXME Not that good! (some text repeated multiple times)
(defun no-ppt (&optional filename)
  "Run ppthtml and w3m -dump on the entire buffer.
Optional FILENAME says what filename to use.
This is only necessary for buffers without
proper `buffer-file-name'.  FILENAME should
be a real filename, not a path."
  (interactive "fPowerPoint File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "ppthtml %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.ppt\\'" . no-ppt))

(message "39 Document Viewing... Done"))


;;** 40 (info "(emacs)Gnus")

(when section-gnus (message "40 Gnus...")

;; perform Caesar ciphers
(when (try-require 'rot13)
  (defvar rot13-translate-table
    (let ((str (make-string 127 0)) (i 0))
      (while (< i 127)
        (aset str i i) (setq i (1+ i)))
      (setq i 0)
      (while (< i 26)
        (aset str (+ i ?a) (+ (% (+ i 13) 26) ?a))
        (aset str (+ i ?A) (+ (% (+ i 13) 26) ?A))
        (setq i (1+ i))) str)
    "String table for rot 13 translation.")

  (defun rot13-string (string)
    "Return Rot13 encryption of STRING."
    (with-temp-buffer
      (insert string)
      (rot13-region (point-min) (point-max))
      (buffer-string)))

  (defun rot13-region (start end)
    "Rot13 encrypt the region between START and END in current buffer."
    (interactive "r")
    (translate-region start end rot13-translate-table))

  ;; XXX vvv Customize this! vvv XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

  ;; full name of this user
  (setq user-full-name "Fabrice Niessen")

  ;; full mailing address of this user
  ;; (used in MAIL envelope FROM, and to select the default personality ID)
  (setq user-mail-address
        (concat (rot13-string "sav") "@" "missioncriticalit.com")))

  ;; XXX ^^^ Customize this! ^^^ XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

;; a newsreader for GNU Emacs
(when (try-require 'gnus)

  ;; Gnus startup file name
  (setq gnus-init-file "~/.gnus")

  (global-set-key (kbd "<C-f5>")
                  '(lambda ()
                     (interactive)
                     (let ((buffer (get-buffer "*Group*")))
                       (if buffer
                           (progn
                             (switch-to-buffer buffer)
                             (gnus-group-get-new-news))
                         (call-interactively 'gnus)))))

  ;; package to compose an outgoing mail (Message, with Gnus paraphernalia)
  (setq mail-user-agent 'gnus-user-agent)
  (XEmacs
   (setq toolbar-mail-reader 'gnus))

  ;; reading mail with Gnus
  (setq read-mail-command 'gnus))

;; some info related functions
;; (to insert links such as `(info "(message)Insertion Variables")')
(when (try-require 'rs-info)
  (autoload 'rs-info-insert-current-node "rs-info"
    "Insert reference to current Info node using STYPE in buffer." t nil)
  (autoload 'rs-info-boxquote "rs-info"
    "Yank text (from an info node), box it and use current info node as title."
    t nil)
  (autoload 'rs-info-reload "rs-info" "Reload current info node." t nil)
  (autoload 'rs-info-insert-node-for-variable "rs-info"
    "Insert a custom style info node for the top level form at point." t nil)
  (defalias 'boxquote-info 'rs-info-boxquote))


;;*** Insidious (info "(bbdb)Top")

(when (try-require 'bbdb)

    ;; coding system used for reading and writing `bbdb-file' (BBDB 2.35+)
    (setq bbdb-file-coding-system 'utf-8)

    ;; ensure `~/.bbdb' never becomes non utf-8 again (it is defined with
    ;; `defconst', so it is reset whenever `bbdb.el' is loaded)
    (add-hook 'bbdb-load-hook
              (lambda () (setq bbdb-file-coding-system 'utf-8)))


;;**** (info "(bbdb)Installation")

    ;; enable the various package-specific BBDB functions
    (bbdb-initialize 'gnus 'message)

    ;; add bindings for the default keys to Gnus and configure Gnus to
    ;; notify the BBDB when new messages are loaded (required if the BBDB is
    ;; to be able to display BBDB entries for messages displayed in Gnus)
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)

    ;; add a binding for `M-TAB' to Message mode
    ;; this will enable completion of addresses based on BBDB records
    (add-hook 'gnus-startup-hook 'bbdb-insinuate-message)

    ;; customizable completion in message headers
    ;; (to avoid conflict between `flyspell' and `BBDB')
    (try-require 'message-x)


;;**** (info "(bbdb)Interfaces")

    ;; mail aliases (local mailing lists)
    (add-hook 'message-setup-hook 'bbdb-define-all-aliases)

    ;; always use full name when sending mail
    ;; (even if User Name has an address of the form <user.name@somedomain>)
    (setq bbdb-dwim-net-address-allow-redundancy t)

    ;; no popup on auto-complete
    (setq bbdb-completion-display-record nil)

    ;; completion is done across the set of all full-names and user-ids
    (setq bbdb-completion-type nil)


;;**** (info "(bbdb)Reader-specific Features")

    ;; marking posters with records in the BBDB
    (setq bbdb/gnus-summary-mark-known-posters t)

    ;; mark authors in the Summary Buffer who have records in the BBDB
    (setq bbdb/gnus-summary-known-poster-mark "B")

    ;; display the poster's name from the BBDB if we have one
    (setq bbdb/gnus-summary-prefer-real-names t)

    ;; replace the information provided in the From header with data from
    ;; the BBDB if we have one
    (setq bbdb/gnus-summary-prefer-bbdb-data t)

    (setq bbdb/gnus-summary-show-bbdb-names t)


;;**** (info "(bbdb)Options")Options

    ;; You can add the author of a mail or posting to the BBDB
    ;; by hitting `:'

    ;; name of the file which contains your personal database
    (setq bbdb-file "~/.bbdb")

    ;; no default area code to use when prompting for a new phone number
    (setq bbdb-default-area-code nil)

    ;; default country to use if none is specified
    (setq bbdb-default-country "")

    ;; disable syntax-checking of telephone numbers
    (setq bbdb-north-american-phone-numbers-p nil)

    ;; restoration of the window configuration
    (setq bbdb-electric-p t)

    ;; don't display a continuously-updating BBDB window while in GNUS
    ;; (setq bbdb-use-pop-up nil)

    ;; desired number of lines in a GNUS pop-up BBDB window
    (setq bbdb-pop-up-target-lines 1)

    ;; default display layout
    (setq bbdb-display-layout 'multi-line)

    ;; default display layout pop-up BBDB buffers
    (setq bbdb-pop-up-display-layout 'one-line)

    ;; omit creation-date and timestamp from BBDB display
    (setq bbdb-display-layout-alist
          '((one-line          (order     . (phones notes))
                               (name-end  . 24)
                               (toggle    . t)
                               (omit      . (net AKA mail-alias gnus-private
                                             creation-date timestamp)))
            (multi-line        (indention . 14)
                               (toggle    . t)
                               (omit      . (AKA creation-date timestamp)))
            (pop-up-multi-line (indention . 14))))

    ;; allow cycling of email addresses while completing them
    (setq bbdb-complete-name-allow-cycling t)

    ;; save the database without asking (any time it would ask)
    (setq bbdb-offer-save 'auto)

    ;; automatically add some text to the notes field of the BBDB record
    (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)

    ;; capture auto-notes
    (setq bbdb-auto-notes-alist
          ;; organization
          `(("Organization" (".*" Organization 0))

            ;; mailer
            ("User-Agent" (".*" mailer 0 t))  ;; t = overwrite
            ("X-Mailer" (".*" mailer 0 t))
            ("X-Newsreader" (".*" mailer 0 t))

            ;; X-Face bitmaps of the people
            ("x-face" ,(list (concat "[ \t\n]*\\([^ \t\n]*\\)"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?"
                                     "\\([ \t\n]+\\([^ \t\n]+\\)\\)?")
                             'face
                             "\\1\\3\\5\\7"))))


;;**** (info "(bbdb)Utilities")

    ;; search the BBDB
    (global-set-key (kbd "<C-f11>") 'bbdb)

    ;; search the BBDB by regexp
    (when (try-require 'bbdb-query)
        (global-set-key (kbd "<C-f11>") 'bbdb-query))

    ;; use BBDB to store PGP preferences
    (when (try-require 'bbdb-pgp)
        ;; what to do if the recipient is not in the BBDB
        (setq bbdb/pgp-default-action nil))

    ;; BBDB SCHDPLUS Filter
    (when (try-require 'bbdb-ldif)
        ;; You can output the `*BBDB*' buffer in SCHDPLUS .CSV format
        ;; by invoking `M-x bbdb-output-schdplus'

        (load "bbdb-schdplus")

        (setq bos-filename "~/bbdb-schdplus.csv")))

(message "40 Gnus... Done"))


;;** 41 Running (info "(emacs)Shell") Commands from Emacs ]

(when section-shell (message "41 Running Shell Commands from Emacs...")

;;*** 41.1 Single Shell

;; force interactive behavior (to get my handy shell aliases)
;; FIXME Fix for Zsh (zsh:1: command not found: shopt)
;; (defadvice shell-command (before my-shell-command activate)
;;   (ad-set-arg 0
;;               (concat "source ~/.bashrc; shopt -s -q expand_aliases;\n "
;;                       (ad-get-arg 0))))

;; for single shell commands
(setq shell-file-name                   ; must be in the `PATH' (Windows users)
      (if (file-executable-p "/usr/bin/zshXXX")
          "zsh"
        "bash"))

;; use `shell-file-name' as the default shell
(when (try-require 'env)
  (setenv "SHELL" shell-file-name))

;; name of shell used to parse TeX commands
(GNUEmacs
 (setq TeX-shell shell-file-name))
(XEmacs
 ;; for the `preview-latex' package
 (setq TeX-shell "C:/Program Files/Emacs/emacs/bin/cmdproxy.exe"))


;;*** 41.2 Interactive Shell

;; for the interactive (sub)shell
(setq explicit-shell-file-name shell-file-name)

;; args passed to inferior shell by `M-x shell', if the shell is bash
(setq explicit-bash-args '("--noediting" "--login"))
;; FIXME This ensures that /etc/profile gets read (at least for Cygwin). Is
;; this good?


;;*** 41.3 Shell Mode

;; general command interpreter in a window stuff
(when (try-require 'comint)

  ;; `M-s'    `comint-next-matching-input'
  ;; `M-r'    `comint-previous-matching-input'
  ;; `M-n'    `comint-next-input'
  ;; `M-p'    `comint-previous-input'
  ;; `C-up'   `last command'

  ;; regexp to recognize prompts in the inferior process
  ;; (set it for Org-babel sh session to work!)
  (defun set-shell-prompt-regexp ()
    (setq comint-prompt-regexp "^[^#$%>\n]*[#$%>] *"))
  (add-hook 'shell-mode-hook 'set-shell-prompt-regexp)
  ;; FIXME See `shell-prompt-pattern'

  ;; don't add input matching the last on the input ring
  (setq-default comint-input-ignoredups t)

  ;; input to interpreter causes (only) the selected window to scroll
  (setq-default comint-scroll-to-bottom-on-input "this")

  ;; output to interpreter causes (only) the selected window to scroll
  (setq-default comint-scroll-to-bottom-on-output "this")

  ;; show the maximum output when the window is scrolled
  (setq-default comint-scroll-show-maximum-output t)

  ;; ignore short commands as well as duplicates
  (setq comint-min-history-size 5)
  (make-variable-buffer-local 'comint-min-history-size)
  (setq-default comint-input-filter
                (function
                 (lambda (str)
                   (and (not (string-match "\\`\\s *\\'" str))
                        (> (length str) comint-min-history-size)))))

  ;; functions to call after output is inserted into the buffer
  (setq-default comint-output-filter-functions
                ;; go to the end of buffer
                '(comint-postoutput-scroll-to-bottom))

  ;; get rid of the ^M characters
  (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

  ;; prompt in the minibuffer for password and send without echoing
  ;; (for example, with `su' command)
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)

  ;; use the `up' and `down' arrow keys to traverse through the previous
  ;; commands
  (defun my-shell-mode-hook ()
    "Customize my shell-mode."
    (local-set-key (kbd "<up>") 'comint-previous-input)
    (local-set-key (kbd "<down>") 'comint-next-input))
  (add-hook 'shell-mode-hook 'my-shell-mode-hook))


;;*** 41.4 Shell Prompts

;;*** 41.5 History

;;*** 41.6 Directory Tracking

;;*** 41.7 Options

;;*** 41.8 Terminal emulator

;;*** 41.9 Term Mode

;;*** 41.10 Paging in Term

;;*** 41.11 Remote Host

;;*** 41.12 Serial Terminal

;; In GNU Emacs 23, there's now support for serial port access. The new
;; command `serial-term' starts an interactive terminal on a serial port.




;; You're debugging bash code? I normally use `mode-compile.el' for
;; this. Basically, it runs bash with lots of debug output.

;; See `w32-settings.el' for more!

;; quote process arguments to ensure correct parsing on Windows
(setq w32-quote-process-args t)

;; switch used to have the shell execute its command line argument
;; (`/c' does not work with XEmacs)
(setq shell-command-switch "-c")

;; shell argument indicating that next argument is the command
(setq TeX-shell-command-option shell-command-switch)

;; regexp to match prompts in the inferior shell
(setq shell-prompt-pattern (concat "^" (system-name) " [^ ]+ \\[[0-9]+\\] "))

;; translate ANSI escape sequences into faces
(GNUEmacs
    (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on))


;; Emacs shell is a "dump" terminal which doesn't support (all) terminal
;; control codes. You need to use `M-x term' if you want a proper terminal
;; (but even that is not as good as the good old XTerm).
;;
;; (term "bash")
;; If you do `M-x term', `bash' is offered as the default as well.
;;
;; Need a good terminal emulation for applications that are interactive with
;; your terminal, in the same way `top' is working or `man' ... or `less' ...
;; or `unison'

;; "less" needs a _terminal_. Emacs's shell-mode is not a terminal so "less"
;; doesn't work. If the programs you use needs terminal features then you must
;; use a terminal, such as "M-x term".

;; This "page-at-a-time" feature comes with terminal emulation. There's no
;; need to make "M-x shell" a real terminal because there already is one: "M-x
;; term".

;; Try also to use `M-x ansi-term' that is much better than `term': with M-x
;; term, you can run only one shell; with `ansi-term' you can start more than
;; one (*ansi-term*<2>, *ansi-term*<3>).

;; One weird thing, is that 'M-x term' is a term emulation where less(1)
;; works and it has the 'C-c C-q' one-page-at-a-time thing as well...
;;
;; M-x shell, has neither.

;; >> Or are there some things `M-x term' can't do while `M-x shell' can?

;; > With M-x shell you're using emacs to construct the command to pass to
;; > the shell. This is an advantage if you shell doesn't keep a history or
;; > offers no completion mechanism. Also, you can use isearch to search
;; > through the command output, copy stuff into the kill ring or use the
;; > rectangle functions. Or you might just prefer the emacs keybindings
;; > over the one's your shell offers. 8-)

;; I'd like to point out that term-mode has also the so called "line mode"
;; (C-c C-j) in which user can wander around the buffer pretty much like
;; anywhere else in Emacs. Kill-ring commands, isearch etc. work. Then
;; there is "char mode" (C-c C-k), the default, which is like your normal
;; terminal emulator except the escape key.

;; Actually you can read this in the emacs info pages: "In line mode,
;; Term basically acts like Shell mode".

;; The problem here is the word 'basically' which probably means
;; 'almost'. So they differ in the details.

;; In my case, I started to use `M-x shell', and like it since I almost
;; never have to use applications that need a real term emulation. But if
;; you look at them (M-x term and M-x shell) carefully, then `M-x term'
;; has some weird behaviours when using the shell mode key bindings; try
;; for instance: `C-c C-e', `C-c C-a', `C-c C-o'... but as I said they're
;; details.


;; managing multiple terminal buffers in Emacs
;; (and fixing some troubles of `term-mode': key bindings, etc.)
(try-require 'multi-term)


;; run an inferior shell, with I/O through buffer `*shell*'
(global-set-key [(control !)]
                (cond (running-ms-windows 'shell)
                      (t 'term)))

;; you can switch between term modes. Using `C-c c-j' will put you
;; in line mode where term is behaves like a buffer , then u can use
;; `C-c c-k' to switch back to char mode.


;; run a telnet session from within an Emacs buffer
(when (try-require 'telnet)

  ;; program to run to open a telnet connection
  ;; simple public domain telnet client for Windows console
  ;; (from Igor Milavec)
  (setq telnet-program
        (cond (running-ms-windows
               (concat my-site-lisp-directory "../bin/telnet.exe"))
              (t
               "/usr/bin/telnet")))

  ;; open a network login connection to a host
  (defun telnet (host)
    "Open a network login connection to host named HOST (a string).
    Communication with HOST is recorded in a buffer `*telnet-HOST*'.
    Normally input is edited in Emacs and sent a line at a time."
    (interactive "sOpen telnet connection to host: ")
    (let* ((comint-delimiter-argument-list '(?\  ?\t))
           (name (concat "telnet-" (comint-arguments host 0 nil) ))
           (buffer (get-buffer (concat "*" name "*")))
           process)
      (cond ((string-equal system-type "windows-nt")
             (setq telnet-new-line "\n")))
      (if (and buffer (get-buffer-process buffer))
          (pop-to-buffer (concat "*" name "*"))
        (pop-to-buffer (make-comint name telnet-program nil host))
        (setq process (get-buffer-process (current-buffer)))
        (set-process-filter process 'telnet-initial-filter)
        (accept-process-output process)
        (telnet-mode)
        (setq comint-input-sender 'telnet-simple-send)
        (setq telnet-count telnet-initial-count)))))

(message "41 Running Shell Commands from Emacs... Done"))


;;** 42 Using (info "(emacs)Emacs Server")

(when section-emacs-server (message "42 Using Emacs as a Server...")

;; use Emacs as a server
;; On GNU/Linux, you should use the `server-start' function and the
;; `emacsclient' program (now as well available under EmacsW32). Emacs 23 has
;; a `--daemon' flag which makes this even more convenient.
(GNUEmacs
;;  (setq server-host "Rabbit")
;;  (setq server-use-tcp t)


;; * How can I use TRAMP to connect to a remote GNU Emacs session?
;;
;; You can configure Emacs Client doing this.  On the remote host,
;; you start the Emacs Server:
;;
;; (require 'server)
;; (setq server-host (system-name)
;;       server-use-tcp t)
;; (server-start)
;;
;; Make sure, that the result of `(system-name)' can be resolved on
;; your local host; otherwise you might use a hard coded IP address.
;;
;; The resulting file `~/.emacs.d/server/server' must be copied to
;; your local host, at the same location.  You can call then the
;; Emacs Client from the command line:
;;
;; emacsclient /ssh:user@host:/file/to/edit
;;
;; `user' and `host' shall be related to your local host.


    ;; start the Emacs server
    (server-start))


;; > I'd like to be able to reconnect to the running Emacs process and have
;; > it display on my X server at home.  Is this possible?
;;
;; In the X11 forwarded ssh shell:
;;
;; $ emacsclient -e "(make-frame-on-display \"$DISPLAY\")"
;; ; Fri Feb  1 13:06:41 2008 - sva   Replace `$DISPLAY' by `:0.0'
;; From VM:
;; > ssh 10.0.2.2 -f emacsclient --eval '"(make-frame-on-display \":0.0\")"'

;; rebind `C-x C-c' to `delete-frame'?

;; (GNUEmacs
;;     (defun my-done ()
;;       (interactive)
;;       (server-edit)
;;       (make-frame-invisible nil t))
;;     (global-set-key (kbd "C-x C-c") 'my-done))

(message "42 Using Emacs as a Server... Done"))


;;** 43 (info "(emacs)Printing") Hard Copies

(when section-printing (message "43 Printing Hard Copies...")

;; print Emacs buffer on line printer
;; for {lpr,print}-{buffer,region}
(when (try-require 'lpr)

    ;; name of program for printing a file
    (setq lpr-command "enscript")  ; TODO Install `enscript'

    ;; list of strings to pass as extra options for the printer program
    (setq lpr-switches (list "--font=Courier8"
                             "--header-font=Courier10"
                             (format "--header=%s" (buffer-name))))

    ;; name of a printer to which data is sent for printing
    (setq printer-name
          (cond (running-ms-windows
                 "//PRINT-SERVER/C510_APS")
                (t
                 t))))

;; 43 Printing Hard Copies...
;; Checking for library `lpr'... Found
;; Checking for library `ps-print'... Found [3 times]
;; Checking for library `filladapt'... Found
;; Tramp: Opening connection for fni@C using ssh...
;; Tramp: Waiting 60s for local shell to come up...
;; Tramp: Sending command `ssh C -l fni  -q -e none && exit || exit'
;; Tramp: Waiting for prompts from remote shell
;; File error: Process died




;; print text from the buffer as PostScript
(when (try-require 'ps-print-XXX)

    (let ((gsprint-program "C:/Program Files/Ghostgum/gsview/gsprint.exe"))
      (my-file-executable-p gsprint-program)

      (if (and gsprint-program
               (file-executable-p gsprint-program))
          (progn
            ;; name of a local printer for printing PostScript files
            ;; adjusted to run Ghostscript
            (setq ps-printer-name t)

            ;; name of program for printing a PostScript file
            ;; tell Emacs where ghostscript print utility is located
            (setq ps-lpr-command gsprint-program)

            ;; list of extra switches to pass to `ps-lpr-command'
            ;; tell Ghostscript to query which printer to use
            (setq ps-lpr-switches '("-query")))

        (progn
          (setq ps-printer-name "//PRINT-SERVER/LexmarkC510")
          (setq ps-lpr-command "")
          (setq ps-lpr-switches '("raw"))))

    ;; size of paper to format for
    (setq ps-paper-type 'a4)

    ;; print in portrait mode
    (setq ps-landscape-mode nil)

    ;; number of columns
    (setq ps-number-of-columns 1)))

;; generate and print a PostScript image of the buffer
(GNUEmacs
    (when running-ms-windows
      (w32-register-hot-key [snapshot]) ; override `Print Screen' globally
                                        ; used as a hotkey by Windows
      (global-set-key (kbd "<snapshot>") 'ps-print-buffer-with-faces)))
(XEmacs
    (setq toolbar-print-function 'ps-print-buffer-with-faces))

(global-set-key (kbd "M-p") 'ps-print-buffer-with-faces)

(message "43 Printing Hard Copies... Done"))


;;** 47 (info "(emacs)Sorting") Text

(when section-sorting (message "47 Sorting Text...")

;; key binding
(global-set-key (kbd "C-c ^") 'sort-lines)

(message "47 Sorting Text... Done"))


;;** 48 (info "(emacs)Narrowing")

(when section-narrowing (message "48 Narrowing...")

;; enable the use of the command `narrow-to-region' without confirmation
(put 'narrow-to-region 'disabled nil)

(message "48 Narrowing... Done"))


;;** 51 (info "(emacs)Saving Emacs Sessions")

(when section-saving-emacs-sessions (message "51 Saving Emacs Sessions...")

(try-idle-require 'saveplace)
(eval-after-load 'saveplace
  '(progn

    ;; automatically save place in each file
    (setq-default save-place t)  ;; default value for all buffers

    ;; name of the file that records `save-place-alist' value
    (setq save-place-file (convert-standard-filename "~/.emacs.d/places.txt"))

    ;; do not make backups of master save-place file
    (setq save-place-version-control "never")))

(message "51 Saving Emacs Sessions... Done"))


;;** 54 (info "(emacs)Hyperlinking") and Navigation Features ]

(when section-hyperlinking (message "54 Hyperlinking and Navigation Features...")

;; I use an excellent package called `webjump' to store my bookmarks. It
;; also has provisions for generating search strings for the search sites as
;; well.


;;*** HTML Tidy

(try-require 'tidy)

;; For other modes (like `html-helper-mode') simply change the variables
;; `html-mode-hook' and `html-mode-map' to whatever is appropriate e.g.

;; FIXME html-helper-mode should be in `auto-mode-alist' as well?
(defun my-html-helper-mode-hook ()
  "Customize my html-helper-mode."
  (tidy-build-menu html-helper-mode-map)
  (local-set-key (kbd "C-c C-c") 'tidy-buffer)
  (setq sgml-validate-command "tidy"))

(add-hook 'html-helper-mode-hook 'my-html-helper-mode-hook)


;;*** pass a URL to a WWW browser

;; display the current buffer in the default Windows WWW browser
(try-require 'browse-url)
(autoload 'browse-url-at-mouse "browse-url")

;; default browser started when you click on some URL in the buffer
(if window-system
    (if running-ms-windows
        (setq browse-url-browser-function 'browse-url-default-windows-browser)
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program (executable-find "firefox")))
  (setq browse-url-browser-function 'w3m-browse-url))

;; ;; shortcut to view the current file in browser
;; (define-key html-mode-map (kbd "C-c C-v") 'browse-url-of-buffer)

;; (setq browse-url-browser-function
;;       '(("file:///usr/share/doc/hyperspec/" . w3m-browse-url)
;;         ("emacswiki.org" . w3m-browse-url)
;;         ("lispdoc.com" . w3m-browse-url)
;;         ( "." . browse-url-firefox)))
;; that let me use w3m for EmacsWiki/Common Lisp documentation and
;; Firefox otherwise.

(defun rgr/browse (url)
  "If prefix is specified use the system default browser else use the
configured emacs one"
  (if current-prefix-arg
      (when url (browse-url-default-browser url))
    (if  url (browse-url url) (call-interactively 'browse-url))
    ))

(defun rgr/browse-url (&optional url)
  "browse the url passed in"
  (interactive)
  (setq url (or url
                (w3m-url-valid (w3m-anchor))
                (browse-url-url-at-point)
                (region-or-word-at-point)))
  (setq url (read-string (format "Url \"%s\" :" url) url nil url))
  (rgr/browse url))

(global-set-key (kbd "<f4>") 'rgr/browse-url)

;; Cursor on url. Hit f4 to open using your emacs browser (whatever
;; that is configured to) or C-u f4 to open in your desktop
;; browser (firefox here).
;; It also works in w3m buffers e.g in html rendered emails.
;; You might need to include thingatpt+.el


(defun get-tinyurl (long-url)
  "Gets URL and makes it short"
  (interactive "sLong URL: ")
  (let* ((tinyurl
          (save-excursion
            (with-temp-buffer
              (mm-url-insert
               (concat "http://tinyurl.com/api-create.php?url=" long-url))
              (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))


;;*** Emacs-w3m

;; Emacs/W3 is dead, long live Emacs-w3m
;; Emacs-w3m is a terrific text-based web and file browser

;; You can obtain a snapshot from
;; http://cvs.namazu.org/emacs-w3m.tar.gz?view=tar

;; `w3m-browse-url' asks Emacs-w3m to browse a URL.

;; When JavaScript is needed or the "design" is just too bad, use another
;; browser: you can open the page in your graphical browser (at your own
;; risk) by hitting `M' (`w3m-view-url-with-external-browser').
;; For what "risk" means, please see: (info "(emacs-w3m)Gnus")

;; (for Win32, use the Cygwin version of the executable)
(setq w3m-command (executable-find "w3m"))
(when (and w3m-command
           (file-executable-p w3m-command))
(try-idle-require 'w3m)  ; w3m slows down the startup process dramatically
(eval-after-load 'w3m
    '(progn

;;**** 3.1 Browsing Web Pages

    ;; add key binding
    (global-set-key (kbd "C-c w w") 'w3m)

    ;; go ahead, just try it
    (defun my-w3m-goto-url ()
      "Type in directly the URL I would like to visit (avoiding to hit `C-k')."
      (interactive)
      (let ((w3m-current-url ""))
        (call-interactively 'w3m-goto-url)))

    (define-key w3m-mode-map (kbd "U") 'my-w3m-goto-url)

    ;; fix inappropriate key bindings for moving from place to place in a
    ;; page
    (define-key w3m-mode-map (kbd "<up>") 'previous-line)
    (define-key w3m-mode-map (kbd "<down>") 'next-line)
    (define-key w3m-mode-map (kbd "<left>") 'backward-char)
    (define-key w3m-mode-map (kbd "<right>") 'forward-char)

    (define-key w3m-mode-map (kbd "<tab>") 'w3m-next-anchor)

    ;; moving from page to page
    (define-key w3m-mode-map (kbd "F") 'w3m-view-next-page)


;;**** 3.5 Using Tabs

    (define-key w3m-mode-map (kbd "<C-tab>") 'w3m-next-buffer)
    (define-key w3m-mode-map [(control shift iso-lefttab)] 'w3m-previous-buffer)

    (defun w3m-new-tab ()
      (interactive)
      (w3m-copy-buffer nil nil nil t))

    (define-key w3m-mode-map (kbd "C-t") 'w3m-new-tab)

    (define-key w3m-mode-map (kbd "C-w") 'w3m-delete-buffer)


;;**** 5.1 General Variables

    ;; send referers only when both the current page and the target page are
    ;; provided by the same server
    (setq w3m-add-referer 'lambda)

    ;; home page
    (setq w3m-home-page "http://www.emacswiki.org/")

    ;; number of steps in columns used when scrolling a window horizontally
    (setq w3m-horizontal-shift-columns 1)  ; 2

    ;; proxy settings
    (when (string= (upcase (system-name)) "PC3701")
      (eval-after-load "w3m"
        '(setq w3m-command-arguments
               (nconc w3m-command-arguments
                      '("-o" "http_proxy=http://proxy:8080"))))
                                        ; FIXME https_proxy for HTTPS support
      (setq w3m-no-proxy-domains '("local.com" "sysdoc")))


;;**** 5.2 Image Variables

    ;; always display images
    (setq w3m-default-display-inline-images t)

    ;; show favicon images if they are available
    (setq w3m-use-favicon t)


;;**** 5.4 Cookie Variables

    ;; functions for cookie processing
    (when (try-require 'w3m-cookie)
        ;; ask user whether accept bad cookies or not
        (setq w3m-cookie-accept-bad-cookies 'ask)

        ;; list of trusted domains
        (setq w3m-cookie-accept-domains
              '("google.com" "google.be"
                "yahoo.com" ".yahoo.com" "groups.yahoo.com"
                "www.dyndns.org")))

    ;; enable cookies (to use sites such as Gmail)
    (setq w3m-use-cookies t)


;;**** 5.14 Other Variables

    ;; functions convenient to access web search engines
    (when (try-require 'w3m-search)
      (global-set-key (kbd "C-c w s") 'w3m-search)
      (add-to-list 'w3m-search-engine-alist
                   '("teoma" "http://www.teoma.com/search.asp?t=%s" nil)))


    (defun google (what)
      "Use google to search for WHAT."
      (interactive "sSearch: ")
      (save-window-excursion
        (delete-other-windows)
        (let ((dir default-directory))
          (w3m-browse-url (concat "http://www.google.com/search?q="
                                  (w3m-url-encode-string what)))
          (cd dir)
          (recursive-edit))))
    (global-set-key (kbd "C-c g s") 'google)


    ;; list of content types, regexps (matching a url or a file name), commands
    ;; to view contents, and filters to override the content type specified at
    ;; first
    (setq w3m-content-type-alist
          (append '(("text/html" "\\.xhtml\\'" nil nil))
                  w3m-content-type-alist))

    ;; toggle a minor mode showing link numbers
    (when (try-require 'w3m-lnum)

      (defun my-w3m-go-to-linknum ()
        "Turn on link numbers and ask for one to go to."
        (interactive)
        (let ((active w3m-link-numbering-mode))
          (when (not active) (w3m-link-numbering-mode))
          (unwind-protect
              (w3m-move-numbered-anchor (read-number "Anchor number: "))
            (when (not active) (w3m-link-numbering-mode))
            (w3m-view-this-url))))

      (define-key w3m-mode-map (kbd "f") 'my-w3m-go-to-linknum)

      ;; enable link numbering mode by default
      (add-hook 'w3m-mode-hook 'w3m-link-numbering-mode)))))


;;**** 9.1 Turning Gnus into a web browser!

;; `nnshimbun' is a Gnus back end, but it is distributed with emacs-w3m, not
;; Gnus, exceptionally. `Nnshimbun' allows you to turn Gnus into an
;; exceptionally useful web browser. You can skim through the articles on a
;; newspaper's web server without having to see all the advertisement. You can
;; read articles in mailing list archives as if you were subscribed to the
;; list. You can also read submissions in bulletin boards, etc...


;;*** Web search

(when (and (try-require 'browse-url) t)
;;           (try-require 'url))

;; from Glenn Morris

    (defvar my-google-maxlen 200
      "Maximum string length of search term.")

    (defvar my-google-url "www.google.com"
      "Base URL for Google search.")

    (defvar my-google-groups-url "groups.google.com"
      "Base URL for groups Google search.")

    (defun my-google-search-region (prefix start end)
      "Create a search URL and send it to the web browser.
    With prefix argument, use groups URL."
      (interactive "P\nr")
      (if (> (- end start) my-google-maxlen)
          (message "Search string too long!")
        (let ((query (buffer-substring-no-properties start end)))
          (browse-url
           (concat "http://"
                   (if prefix (concat my-google-groups-url "/groups")
                     (concat my-google-url "/search"))
                   "?q=" (url-hexify-string query))))))

    (defvar my-url-maxlen 100
      "Maximum length of string to send to browser as URL.")

    ;; `find-file-at-point' does this, essentially
    (defun my-url-at-point (start end)
      "Send the highlighted URL to the web browser."
      (interactive "r")
      (if (> (- end start) my-url-maxlen)
          (message "URL too long!")
        (browse-url (buffer-substring-no-properties start end))))

;; (require 'url)
;;
;; (defvar google-search-maxlen 50
;;   "Maximum string length of search term.  This prevents you from accidentally
;; sending a five megabyte query string to Netscape.")
;;
;; (defun google-it (search-string)
;;   "Search for SEARCH-STRING on Google."
;;   (interactive "sSearch for: ")
;;   (browse-url (concat "http://www.google.com/search?client=xemacs&q="
;;                   (url-hexify-string
;;                     (encode-coding-string search-string 'utf-8)))))
;;
;; (defun google-search-selection ()
;;   "Create a Google search URL and send it to your web browser."
;;   (interactive)
;;   (let (start end term url)
;;     (if (or (not (fboundp 'region-exists-p)) (region-exists-p))
;;         (progn
;;           (setq start (region-beginning)
;;                 end   (region-end))
;;           (if (> (- start end) google-search-maxlen)
;;               (setq term (buffer-substring start (+ start google-search-maxlen)))
;;             (setq term (buffer-substring start end)))
;;           (google-it term))
;;       (beep)
;;       (message "Region not active"))))



    (defun my-google-search ()
      "Prompt for a query in the minibuffer, launch the web browser and query
    Google."
      (interactive)
      (let ((query (read-from-minibuffer "Google Search: ")))
        (browse-url (concat "http://" my-google-url "/search?q="
                            (url-hexify-string query)))))

    (defun my-google-search-word-at-point ()
      "Google the word at point."
      (interactive)
      (browse-url (concat "http://" my-google-url "/search?q="
                          (word-at-point))))

    (defun my-google-search-file (file)
      "Use Google to search for a file named FILE."
      (interactive "sSearch for file: ")
      (w3m-browse-url
       (concat "http://" my-google-url "/search?q="
               (w3m-url-encode-string
                (concat "+intitle:\"index+of\" "
                        "-inurl:htm -inurl:html -inurl:php "
                        file)))))

    (defvar my-google-prefix-map (make-sparse-keymap)
      "Keymap for my Google commands.")

;;;     (global-set-key [(meta s)] 'my-google-search-region)

    (global-set-key (kbd "C-c g") my-google-prefix-map)
    (define-key my-google-prefix-map "g" 'my-google-search)
    (define-key my-google-prefix-map (kbd "RET") 'my-google-search)
    (define-key my-google-prefix-map "w" 'my-google-search-word-at-point)
    (define-key my-google-prefix-map "r" 'my-google-search-region)
    (define-key my-google-prefix-map "u" 'my-url-at-point)


    (defun lookup-word-definition-in-w3m ()
      "Look up the word's definition in a emacs-w3m.\n
If a region is active (a phrase), lookup that phrase."
      (interactive)
      (let (myword
            myurl)
        (setq myword
              (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (thing-at-point 'symbol)))
        (setq myword (replace-regexp-in-string " " "%20" myword))
        (setq myurl (concat "http://www.answers.com/main/ntquery?s=" myword))
        (w3m-browse-url myurl)))
    (define-key my-google-prefix-map "a" 'lookup-word-definition-in-w3m)

    (defun lookup-wikipedia ()
      "Look up the word's in Wikipedia.\n
This command generates a url for Wikipedia.com and switches
you to browser.
If a region is active (a phrase), lookup that phrase."
      (interactive)
      (let (myword
            myurl)
        (setq myword
              (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (thing-at-point 'symbol)))
        (setq myword (replace-regexp-in-string " " "_" myword))
        (setq myurl (concat "http://en.wikipedia.org/wiki/" myword))
        (w3m-browse-url myurl))))


;;*** Babel

(GNUEmacs
    (let ((my/path-to-url (concat local-site-lisp-directory "url.el"))
          (my/path-to-babel (concat local-site-lisp-directory "babel.el")))
          ;; http://github.com/juergenhoetzel/babel/blob/master/babel.el
      (if (and (file-readable-p my/path-to-url)
               (file-readable-p my/path-to-babel))
          (progn
            ;; Uniform Resource Locator retrieval tool
            (require 'url my/path-to-url)

            ;; interface to web translation services such as Babelfish
            (require 'babel my/path-to-babel)))))


(when (try-require 'org-toodledo)
  ;; FIXME depends on url (problem = conflict with other url packages)
  (setq org-toodledo-userid "td4bf295821172f")
  (setq org-toodledo-password "default"))


;;*** (info "(emacs-goodies-el)htmlize")

;; HTML-ize font-lock buffers
(when (try-require 'htmlize)
    ;; For Emacs 23 users: in order to avoid "Invalid face" errors, you need
    ;; to use the version made available by Carsten in `org-mode/contrib/lisp'
    ;; directory

    ;; output type of generated HTML
    (setq htmlize-output-type 'css)

    ;; override output type `inline-css' used for htmlizing a region
    (defun htmlize-region-for-paste (beg end)
      "Htmlize the region and return just the HTML as a string.
This forces the `css' style and only returns the HTML body, but
without the BODY tag. This should make it useful for inserting
the text to another HTML buffer."
      (let* ((htmlize-output-type 'css)  ; was `inline-css'
             (htmlbuf (htmlize-region beg end)))
        (unwind-protect
            (with-current-buffer htmlbuf
              (buffer-substring (plist-get htmlize-buffer-places 'content-start)
                                (plist-get htmlize-buffer-places 'content-end)))
          (kill-buffer htmlbuf))))

    ;; charset declared by the resulting HTML documents
    (setq htmlize-html-charset "utf-8")

    ;; non-ASCII characters (codes in the 128-255 range) are copied to HTML
    ;; without modification -- if your HTML is in Unicode
    (setq htmlize-convert-nonascii-to-entities nil)

    ;; key binding
    (global-set-key (kbd "M-P") 'htmlize-buffer))

;; quick print preview (to Web browser) with `htmlize-view-buffer'
(GNUEmacs
    ;; view current buffer as html in web browser
    (when (try-require 'htmlize-view)

        ;; add "Quick Print" entry to file menu
        (htmlize-view-add-to-files-menu)))

        ;; Now, you can print from the browser in (complete) Unicode,
        ;; using your system's capabilities

(message "54 Hyperlinking and Navigation Features... Done"))


;;** 56 Other (info "(emacs)Amusements")

(when section-amusements (message "56 Other Amusements...")

(GNUEmacs
    ;; get rid of the Games in the Tools menu
    (define-key menu-bar-tools-menu [games] nil))

(message "56 Other Amusements... Done"))


;;** (info "(pgg)Top") (Emacs interface to GnuPG)

(when section-pgg (message "XX PGG...")

;; In Gnus 5.10 you don't need `mailcrypt' as it comes with a package called
;; "PGG" which does what mailcrypt does (and more because mailcrypt doesn't
;; do PGP/MIME).
;; Mailcrypt has some anonymous remailer stuff in it.  PGG doesn't,
;; but you can do most of that kind of stuff from within Gnus, I believe.

;; , (info "(pgg)Overview") ]
;; | PGG is an interface library between Emacs and various tools for secure
;; | communication.  Even though Mailcrypt has similar feature, it does not
;; | deal with detached PGP messages, normally used in PGP/MIME
;; | infrastructure.  This was the main reason why I wrote the new library.
;; `----

;;; Je chiffre avec easypg qui fait cela (entre autres choses). J'ai
;;; demandé à l'auteur que son outil propose encore plus de
;;; fonctionnalités pour être *le* vrai frontend GNU Emacs à GPG que
;;; je cherche depuis des années.
;;;
;;; Je me sers aussi de easypg en lieu et place de mailcrypt qui a
;;; l'air "mort" - i.e., plus maintenu.

;; 2008-03-03 -- No Gnus uses EasyPG instead of PGG when EasyPG is
;; installed. In Emacs CVS, EasyPG has been added recently.
;;
;; EasyPG is a GnuPG interface for Emacs. It has two aspects: convenient
;; tools which allow to use GnuPG from Emacs (EasyPG Assistant), and a fully
;; functional interface library to GnuPG (EasyPG Library.) It does not cache
;; passphrases, so gpg-agent (security/gnupg-devel) is recommended.

;; http://www.easypg.org/

;; glue for the various PGP implementations
(when (try-require 'pgg)

    ;; default PGP scheme
    (setq pgg-default-scheme 'gpg)

    ;; user ID of your default identity
    (setq pgg-default-user-id
          (concat user-full-name
                  " <" (rot13-string "sav") "@" "missioncriticalit.com>"))

    ;; do not encrypt all outgoing messages with user's public key
    (setq pgg-encrypt-for-me nil)

    ;; how many seconds the passphrase is cached
    (setq pgg-passphrase-cache-expiry 600)

    ;; current scheme of PGP implementation
    (setq pgg-scheme 'gpg)

    ;; verify signed parts
    (setq mm-verify-option 'always)

    ;; decrypt encrypted parts
    (setq mm-decrypt-option 'always)

    ;; display buttons for signed and encrypted messages
    (setq gnus-buttonized-mime-types
          '("multipart/signed" "multipart/encrypted"))

    ;; package used for PGP/MIME
    (setq mml2015-use 'pgg))

;; I'm not well acquainted with PGP/GnuPG too but you will be able
;; to fetch the key automatically if you set the `keyserver' entry
;; in the ~/.gnupg/gpg.conf file properly.

;; ;; Automagically sign all messages
;; ;(add-hook 'message-send-hook 'my-will-you-sign)
;; (defun my-will-you-sign ()
;;   (load-library "mc-toplev")
;;   (interactive)
;;   (if (y-or-n-p "Do you want to sign this message? ")
;;       (mc-sign-message)))

;; ;; Hide pgp cruft if any.
;; (setq gnus-treat-strip-pgp t)

;; ;; After hiding pgp, verify the message;
;; ;; only happens if pgp signature is found.
;; (add-hook 'gnus-article-hide-pgp-hook
;;           (lambda ()
;;             (save-excursion
;;               (set-buffer gnus-original-article-buffer)
;;               (mc-verify))))

;; create a before-save-hook for auto encryption functions
(defvar before-save-hook nil)
(make-local-hook 'before-save-hook)
(defadvice save-buffer (before crs-before-save-run-hooks)
  "Run before-save-hook before saving."
  (run-hooks 'before-save-hook))

(ad-activate 'save-buffer)

;; this is the auto-encryption function called at the bottom of important files
(defun crs-auto-encrypt ()
  (mc-decrypt)
  (auto-save-mode nil)
  (add-hook 'before-save-hook
            (lambda ()
              (mc-encrypt-region
               0 0 (save-excursion
                     (goto-char (point-max))
                     (re-search-backward "[L]ocal Variables:" nil t)
                     (beginning-of-line)
                     (point)))) nil t))

(define-minor-mode sensitive-mode
  "For sensitive files like password lists.
It disables backup creation and auto saving.

With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."

  ;; the initial value
  nil

  ;; the indicator for the mode line
  " Sensitive"

  ;; the minor mode bindings
  nil
  (if (symbol-value sensitive-mode)
      (progn
        ;; disable backups
        (set (make-local-variable 'backup-inhibited) t)
        ;; disable auto-save
        (if auto-save-default
    (auto-save-mode -1)))
    ;; resort to default value of backup-inhibited
    (kill-local-variable 'backup-inhibited)
    ;; resort to default auto save setting
    (if auto-save-default
        (auto-save-mode 1))))

;; Once the above snippet has been evaluated in Emacs, `M-x sensitive' will
;; disable backups and auto-save in the current buffer. All other buffers
;; will continue to have these features.

;; I usually set sensitive mode to turn on by default for files having the
;; gpg extension. The following code when put in your `.emacs' does exactly
;; that:

(setq auto-mode-alist
      (append '(("\\.gpg$" . sensitive-mode))
              auto-mode-alist))


;; Check out
;;   http://www.emacswiki.org/   -> Modes -> AutoEncryption
;;   http://www.emacswiki.org/cgi-bin/emacs-en/AutoEncryption
;; for more information. I'm using crypt++, starting off with a file,
;; file.txt say, I encrypt it manually the first time
;; $ gpg --symmetric --armor file.txt
;; Enter passphrase:
;; repeat passphrase:
;; $
;;
;; This creates a new file "file.txt.asc", which I open with emacs
;; (assuming crypt++ to be set up). This will ask for the passphrase,
;; then decrypt the file for me into the buffer. When saving the file, it
;; will be encrypted again. You can then remove the cleartext file.
;;
;; The use of "--armor" is disputable. While editing the file, there is
;; something like a autosave-file, the contents is clear in an emacs
;; buffer. This can be a problem on systems with more people logged in at
;; any time. So, I guess, this is not good for all possible scenarios.
;;
;; Hmm, you can try (info "(pgg)").  It uses GPG to provide functions for
;; encryption/decryption.
;; Note however, pgp is only in Emacs 22.


;; interface to `pwsafe'
(GNUEmacs
    (when (try-require 'pwsafe)

        ;; primary database used for `pwsafe'
        (setq pwsafe-primary-database "~/.hide/.pwsafe.dat")

        ;; lock the password database after the given number of seconds
        (setq pwsafe-keep-passwd 120)))

(message "XX PGG..."))


;;** 57 (info "(emacs)Customization")

(when section-customization (message "57 Customization...")

;; inhibit the initial startup message in the `*scratch*' buffer
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; ;; limit serving to catch infinite recursions for you before they
;; ;; cause actual stack overflow in C, which would be fatal for Emacs
;; (setq max-lisp-eval-depth (* 40 max-lisp-eval-depth))  ; 40 * 400 (default)

;; ;; limit on number of Lisp variable bindings & unwind-protects
;; (setq max-specpdl-size (* 40 max-specpdl-size))  ; 40 * 1 M (default)

;; ;; speed up things by preventing garbage collections
;; (setq gc-cons-threshold (* 40 gc-cons-threshold))  ; 40 * 400 KB (default)

;; make Gnus fast
(setq gc-cons-threshold 3500000)
    ; from http://www.emacswiki.org/emacs/GnusSpeed

;; don't display messages at start and end of garbage collection (as it hides
;; too many interesting messages)
(setq garbage-collection-messages nil)


;;*** 57.3 (info "(emacs)Variables")

;; file local variables specifications are obeyed, without query -- RISKY!
(setq enable-local-variables t)

;; obey `eval' variables -- RISKY!
(setq enable-local-eval t)

;; record safe values for some local variables
(setq safe-local-variable-values
      '((TeX-master . t)
        (balloon-help-mode . -1)
        (flyspell-mode . t)
        (flyspell-mode . -1)
        (ispell-local-dictionary . "en_US")
        (ispell-local-dictionary . "fr_FR")
        (ispell-mode . t)
        (nuweb-auto-index-scrap)
        (nuweb-source-mode . "mercury")
        (nuweb-source-mode . "sql")
        (org-export-latex-title-command . "\\maketitle[logo=Forem]")))

;; Have a look at (info "(emacs)Directory Variables")


;;*** 57.4 Customizing (info "(emacs)Key Bindings")

;; the keys `C-c LETTER' are reserved for user functions

;; print the key bindings in a tabular form
;; [from http://www-xray.ast.cam.ac.uk/~gmorris/dotemacs.html]
(GNUEmacs
    (defun my-keytable (arg)
      "Print the key bindings in a tabular form."
      (interactive "sEnter a modifier string:")
      (with-output-to-temp-buffer "*Key table*"
        (let* ((i 0)
               (keys (list "a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
                           "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
                           "<return>" "<down>" "<up>" "<right>" "<left>"
                           "<home>" "<end>" "<f1>" "<f2>" "<f3>" "<f4>" "<f5>"
                           "<f6>" "<f7>" "<f8>" "<f9>" "<f10>" "<f11>" "<f12>"
                           "1" "2" "3" "4" "5" "6" "7" "8" "9" "0"
                           "`" "~" "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "-"
                           "_" "=" "+" "\\" "|" "{" "[" "]" "}" ";" "'" ":"
                           "\"" "<" ">" "," "." "/" "?"))
               (n (length keys))
               (modifiers (list "" "S-" "C-" "M-" "M-C-"))
               (k))
          (or (string= arg "") (setq modifiers (list arg)))
          (setq k (length modifiers))
          (princ (format " %-10.10s |" "Key"))
          (let ((j 0))
            (while (< j k)
              (princ (format " %-28.28s |" (nth j modifiers)))
              (setq j (1+ j))))
          (princ "\n")
          (princ (format "_%-10.10s_|" "__________"))
          (let ((j 0))
            (while (< j k)
              (princ (format "_%-28.28s_|"
                             "_______________________________"))
              (setq j (1+ j))))
          (princ "\n")
          (while (< i n)
            (princ (format " %-10.10s |" (nth i keys)))
            (let ((j 0))
              (while (< j k)
                (let* ((binding
                        (key-binding (read-kbd-macro (concat (nth j modifiers)
                                                             (nth i keys)))))
                       (binding-string "_"))
                  (when binding
                    (if (eq binding 'self-insert-command)
                        (setq binding-string (concat "'" (nth i keys) "'"))
                      (setq binding-string (format "%s" binding))))
                  (setq binding-string
                        (substring binding-string 0 (min (length
                                                          binding-string) 28)))
                  (princ (format " %-28.28s |" binding-string))
                  (setq j (1+ j)))))
            (princ "\n")
            (setq i (1+ i)))
          (princ (format "_%-10.10s_|" "__________"))
          (let ((j 0))
            (while (< j k)
              (princ (format "_%-28.28s_|"
                             "_______________________________"))
              (setq j (1+ j))))))
      (delete-window)
      (hscroll-mode)
      (setq truncate-lines t)))


;; You can get a list of all the disabled functions by typing
;; `M-: (let(lst)(mapatoms(lambda(x)(if(get x 'disabled)(push x lst))))lst) RET'


(defmacro rloop (clauses &rest body)
  (if (null clauses)
      `(progn ,@body)
    `(loop ,@(car clauses) do (rloop ,(cdr clauses) ,@body))))

(defun all-bindings ()
  (interactive)
  (message "all-bindings: wait a few seconds please...")
  (let ((data
         (with-output-to-string
           (let ((bindings '()))
             (rloop ((for C in '("" "C-"))       ; Control
                     (for M in '("" "M-"))       ; Meta
                     (for A in '("" "A-"))       ; Alt
                     (for S in '("" "S-"))       ; Shift
                     (for H in '("" "H-"))       ; Hyper
                     (for s in '("" "s-"))       ; super
                     (for x from 32 to 127))
                    (let* ((k (format "%s%s%s%s%s%s%c" C M A S H s x))
                           (key (ignore-errors (read-kbd-macro k))))
                      (when key
                        (push
                         (list k
                               (format "%-12s  %-12s  %S\n" k key
                                       (or
                                        ;; (string-key-binding key)
                                        ;; What is this string-key-binding?
                                        (key-binding key))))
                         bindings))))
             (dolist (item
                      (sort bindings
                            (lambda (a b)
                              (or (< (length (first a))
                                     (length (first b)))
                                  (and (= (length (first a))
                                          (length (first b)))
                                       (string< (first a)
                                                (first b)))))))
               (princ (second item)))))))
    (switch-to-buffer (format "Keybindings in %s" (buffer-name)))
    (erase-buffer)
    (insert data)
    (goto-char (point-min))
    (values)))


;;*** 57.5 The (info "(emacs)Syntax") Table

;; The syntax table contains information that tells Emacs how to operate on
;; text, words, sentences etc. It will make Emacs know enough about all the
;; symbols in a buffer. Syntax table is used for example for word motion
;; (`M-f'), spell-checking of words, expansion commands of abbrevs, etc.

;; See `C-h f current-word' and whether characters such as `-' and `_' are
;; considered part of the word (depending on the current major mode).

;; fix completion syntax for `text-mode-syntax-table' (syntax table used for
;; editing text files)
(defun my-change-word-constituent ()
  (map nil
       (function
        (lambda (char)
          (modify-syntax-entry char "w" text-mode-syntax-table)))
       ;; include accented characters in completion syntax
       "áéíóúàèìòùâêîôûäëïöüñåç"))

(my-change-word-constituent)

;; now '-' is not considered a word-delimiter
;; (add-hook 'emacs-lisp-mode-hook
;;           (lambda ()
;;             (modify-syntax-entry ?- "w")))

(message "57 Customization... Done"))


;;** App G Emacs and (info "(emacs)MS-DOS")

(when section-ms-dos (message "Appendix G Emacs and MS-DOS...")

;; numeric keypad (needed in XEmacs for Windows)
(XEmacs
    ;; keys to the right of the regular keyboard
    (define-key key-translation-map [kp-divide]     [?/])
    (define-key key-translation-map [kp-multiply]   [?*])
    (define-key key-translation-map [kp-subtract]   [?-])
    (define-key key-translation-map [kp-add]        [?+])
    (define-key key-translation-map [kp-enter]     [?\r])
    (define-key key-translation-map [kp-decimal]    [?.])

    ;; keys with digits
    (define-key key-translation-map [kp-0]          [?0])
    (define-key key-translation-map [kp-1]          [?1])
    (define-key key-translation-map [kp-2]          [?2])
    (define-key key-translation-map [kp-3]          [?3])
    (define-key key-translation-map [kp-4]          [?4])
    (define-key key-translation-map [kp-5]          [?5])
    (define-key key-translation-map [kp-6]          [?6])
    (define-key key-translation-map [kp-7]          [?7])
    (define-key key-translation-map [kp-8]          [?8])
    (define-key key-translation-map [kp-9]          [?9])

    ;; additional keypad duplicates of keys ordinarily found elsewhere
    (define-key key-translation-map [kp-left]     [left])
    (define-key key-translation-map [kp-right]   [right])
    (define-key key-translation-map [kp-up]         [up])
    (define-key key-translation-map [kp-down]     [down])
    (define-key key-translation-map [kp-begin]   [begin])
    (define-key key-translation-map [kp-home]     [home])
    (define-key key-translation-map [kp-end]       [end])
    (define-key key-translation-map [kp-next]     [next])
    (define-key key-translation-map [kp-prior]   [prior])
    (define-key key-translation-map [kp-insert] [insert])
    (define-key key-translation-map [kp-delete] [delete]))

;; divide key (needed in GNU Emacs for Windows)
(GNUEmacs
    (global-set-key (kbd "<kp-divide>") (kbd "/")))

(message "Appendix G Emacs and MS-DOS... Done"))


;;** Emacs Display

(when section-emacs-display (message "XX Emacs Display...")

;;*** Faces

;; You can get text properties of any char by typing `C-u C-x ='

;; Under Windows, you can get the current font string by typing
;; `(insert (format "\n%S" (w32-select-font)))' followed by `C-x C-e'

;; You can find the current font by typing
;; `M-x ielm RET (frame-parameters) RET'
;; see the line `font'

;; To check if some font is available in Emacs do following:
;;    1.   Switch to the `*scratch*' buffer.
;;    2.   Type `(prin1-to-string (x-list-fonts "font-you-want-to-check or
;;         pattern"))'.
;;    3.   Place the cursor after the last closing paren and hit
;;         `C-j'. List of the names of available fonts matching given
;;         pattern will appear in the current buffer (`*scratch*').
;;    4.   For listing of all available fonts, use
;;         `(prin1-to-string (x-list-fonts "*"))' or
;;         `(dolist (i (x-list-fonts "*")) (princ i) (terpri))'
;;         for a better output.

;; Format: "-a-b-c-d-e-f-g-h-i-j-k-l-"
;; where
;;
;; a = foundry
;;
;; b = font family <<<
;;
;; c = weight
;;     Valid options: `bold', `demibold', `light', `medium', `normal'.
;;
;; d = slant
;;     Valid options: `i' for italic and `r' for roman.
;;
;; e = set width
;;     Ignored by NT-Emacs.
;;
;; f = pixels
;;     Nominal font height in pixels. (Eg. 13 pixels roughly corresponds to
;;     10 points (a point is 1/72 of an inch) on a 96dpi monitor, so the
;;     font spec above is selecting a 10 point bold Courier font)
;;
;; g = points in tenths of a point
;;     10 point is 100
;;
;; h = horiz resolution in dpi
;;     I think these numbers represent the "design resolution" of the font -
;;     on X, fonts are typically designed for 75dpi or 100dpi screens (under
;;     Windows,most monitors are assumed to be 96dpi I believe). NT-Emacs
;;     ignores these values.
;;
;; i = vertical resolution in dpi
;;     I think these numbers represent the "design resolution" of the font -
;;     on X, fonts are typically designed for 75dpi or 100dpi screens (under
;;     Windows,most monitors are assumed to be 96dpi I believe). NT-Emacs
;;     ignores these values.
;;
;; j = spacing
;;     Spacing as in mono-spaced or proportionally spaced.
;;     Values are `c' (constant) or `m' (monospace) to mean fixed-width or
;;     `p' for proportionally spaced.
;;
;; k = average width in tenths of a pixel
;;
;; l = character set
;;     NT-Emacs understands: ansi, oem, symbol to refer to the standard
;;     Windows character sets (the first two, at least, are locale
;;     dependant). "iso8859" and "iso8859-1" are accepted as synonyms for
;;     ansi.

;; Use `xfontsel' utility (or the command-line `xlsfonts') to try out
;; different fonts. After choosing a font, click the select button in
;; `xfontsel' window. This will copy font name you choose to copy & paste
;; buffer.
;; Edit your `~/.Xresources' file to have a line with "Emacs.font".
;; Then do a `xrdb -merge ~/.Xresources' or restart your X11 to validate the
;; modification. I let emacs do this for me:

(defun merge-x-resources ()
  (let ((file (file-name-nondirectory (buffer-file-name))))
    (when (or (string= file ".Xdefaults")
              (string= file ".Xresources"))
      (start-process "xrdb" nil "xrdb" "-merge" (buffer-file-name))
      (message (format "Merged %s into X resource database" file)))))

(add-hook 'after-save-hook 'merge-x-resources)

;; Now Emacs should start with that font.

;; For reasons unknown to me,'emacs' takes a long file to change fonts in an X
;; environment.
;;
;; Rather than using (set-default-font ...) in .emacs, stick the font
;; definition in your .Xresources file (key 'Emacs*font') and then use 'xrdb
;; -load' to activate it. You will find that startup time is greatly improved!

;; avoid Emacs hanging for a while changing default font
(modify-frame-parameters nil '((wait-for-wm . nil)))

;; the real color theme functions
(when (and window-system (try-require 'color-theme))

    ;; initialize the color theme package
    (if (fboundp 'color-theme-initialize)
        (color-theme-initialize))

    ;; color themes will be installed for all frames
    (setq color-theme-is-global t)

    ;; set my default color theme
    (when (try-require 'color-theme-cd)
      (color-theme-cd))
    (when (try-require 'color-theme-fni)
      (color-theme-fni)))  ; `color-theme-print' allows to keep what you see

;; allow any scalable font
(when running-ms-windows
    (setq scalable-fonts-allowed t))

(defun font-exists-p (font)
  "Test if FONT is available."
  (GNUEmacs23
   ;; FIXME list-fonts is void
   (if (null (list-fonts (font-spec :family font)))
              ;; 2008-02-26 function of the new font backend (Emacs 23),
              ;; instead of `x-list-fonts'
       nil
     t)))

;; set default font for all frames
(GNUEmacs
    (cond (running-ms-windows
           (if (font-exists-p "Consolas")
               (modify-all-frames-parameters
                '((font . "-outline-Consolas-normal-r-normal-normal-11-82-96-96-c-*-*-*")))
             ;; '((font . "-microsoft-consolas-medium-r-*-*-*-110-*-*-*-*-iso8859-1")))
             (modify-all-frames-parameters
              '((font . "-outline-Courier New-normal-r-normal-normal-12-90-96-96-c-*-iso8859-1")))))

          (running-gnu-linux
           (if (font-exists-p "Consolas")
               (modify-all-frames-parameters '((font . "Consolas-8")))
                                        ; short notation for Emacs23
             (modify-all-frames-parameters '((font . "DejaVu Sans Mono-9")))))))





;; Fonts that have a good UTF-8 coverage are:
;;
;;    + DejaVu Sans Mono
;;    + DejaVu Sans
;;    + FreeMono (FreeSans, FreeSerif)
;;    + Monospace
;;    - Arial Unicode (MS?)
;;    - Bitstream Vera Sans Mono
;;    - Lucida Sans (Typewriter?) Unicode
;;
;; None of them has all four variants, some have regular (medium) and bold
;; or light and regular, one regular and oblique.

;; To see if anti-aliasing is active, use `xmag' or any of the other
;; magnifier applications. The fonts should have gray edges.

(defvar font-cycle-index 0)
(defconst font-cycle-ring
  (if running-ms-windows
      '(
        "-*-Courier New-*-*-*-*-12-90-*-*-*-*-*-*"
        )
    ;; else
    '(
      "-Misc-Fixed-Medium-R-SemiCondensed-*-13-*-*-*-*-*-*-*"
      )
    ))

(defun font-cycle-next ()
  "Cycle between a list of fonts."
  (interactive)
  (let ((len (length font-cycle-ring))
        (next-index (+ font-cycle-index 1)))
    (if (= next-index len)
        (setq next-index 0))
    (setq font-cycle-index next-index)
    (message (concat "Setting default font to `"
                     (nth font-cycle-index font-cycle-ring) "'"))
    (set-default-font (nth font-cycle-index font-cycle-ring) t)
    (set-frame-font (nth font-cycle-index font-cycle-ring) t)))

(global-set-key (kbd "M-+") 'font-cycle-next)


(XEmacs
    (setq options-save-faces t))

;; convenience functions for face customization
(try-require 'face-list)

;; Cycle Font Sizes
;; Commands to zoom font size (like in Firefox)? In GNU Emacs 23, there are
;; `C-x C-+', `C-x C--' and `C-x C-0' (reset to defaults) for changing font
;; size.


;; Automatically switch to dark background after sunset
;; and to light background after sunrise.
;; See www.jurta.org/emacs/dotemacs.en.html

(message "XX Emacs Display..."))


;;* Recovery from Problems

;;** Debugging

(when section-debugging (message "99 Debugging...")

;; ;; get the backtrace when uncaught errors occur
;; (setq debug-on-error nil)  ; was set to `t' at beginning of buffer

;; warn that some packages were missing
(if missing-packages-list
    (progn (message "Packages not found: %S" missing-packages-list)))

(message "99 Debugging... Done"))



;; `C-x *' invokes the GNU Emacs Calculator
;;
;; Try the embedded mode of Calc: `C-x * E' (no need to mark the region) and
;; similar commands (`J' or `W' in place of `E'). You need to type `C-x * E'
;; again to exit the embedded mode.
;;
;; Start the Calc: `C-x * C'
;;
;; Run the Calculator in the minibuffer: `C-x * Q' (`M-x quick-calc')



(defun reverse-words (start end)
  (interactive "r")
  (let ((words (reverse (split-string (buffer-substring start end)))))
    (delete-region start end)
    (dolist (word words)
      (insert word " "))
    (backward-char 1)
    (delete-char 1)))


(defun reverse-region-by-line (beg end)
  (interactive "r")
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end) (re-search-forward "\\=.*$" end t))
      (replace-match (apply #'string
			    (nreverse (string-to-list (match-string 0)))))
      (forward-line))))


(defun shuffle-vector (vector)
  "Destructively shuffle the contents of VECTOR and return it."
  (loop
   for pos from (1- (length vector)) downto 1
   for swap = (random (1+ pos))
   unless (= pos swap)
   do (rotatef (aref vector pos)
               (aref vector swap)))
  vector)

(defun randomize-region (start end)
  "Randomly re-order the lines in the region."
  (interactive "r")
  (save-excursion
    (save-restriction
      ;; narrow to the region
      (narrow-to-region start end)
      (goto-char (point-min))
      (let* ((nlines (line-number-at-pos end))
             (lines (make-vector nlines nil)))
        ;;
        (while (not (eobp))
          (setf (aref lines (decf nlines)) ; if it's random backwards
                is fine
                (delete-and-extract-region (point)
                                           (progn (forward-visible-
                                                   line 1)
                                                  (point)))))
        ;;
        (let ((rlines (shuffle-vector lines)))
          (dotimes (linenum (length rlines))
            (insert (aref rlines linenum))))))))



(message "Emacs startup time: %d seconds."
         (time-to-seconds (time-since emacs-load-start-time)))
(sit-for 1.5)

;; after-save-hook: (lambda () (byte-compile-file (buffer-file-name)))


;; > move-to-window-line, M-r
;; > back-to-indentation, M-m



;; M-x rainbow-mode
(try-require 'rainbow-mode)


(GNUEmacs
    ;; should be part of Emacs 23.1
    ;; see `split-width-threshold' (minimum width for splitting windows
    ;; sensibly)

    (defun th-display-buffer (buffer force-other-window)
      "If BUFFER is visible, select it.

    If it's not visible and there's only one window, split the
    current window and select BUFFER in the new window. If the
    current window (before the split) is more than 160 columns wide,
    split horizontally, else split vertically.

    If the current buffer contains more than one window, select
    BUFFER in the least recently used window.

    This function returns the window which holds BUFFER.

    FORCE-OTHER-WINDOW is ignored."
      (or (get-buffer-window buffer)
          (if (one-window-p)
              (let ((new-win (if (> (window-width) 160)
                                 (split-window-horizontally)
                               (split-window-vertically))))
                (set-window-buffer new-win buffer)
                new-win)
            (let ((new-win (get-lru-window)))
              (set-window-buffer new-win buffer)
              new-win))))

    (setq display-buffer-function 'th-display-buffer))


;; Other IRC for Emacs: rcirc, circe, bitlbee,  liece, riece, zenirc, erc

;; Circe is advised by Tassilo (contributor)

(when (try-require 'circe)

  ;; This defines the password variables below
  (when (file-exists-p "~/.private.el")
    (load-file "~/.private.el"))
    (setq freenode-passwd "")

  (setq circe-default-nick "vauban")

  (setq circe-default-realname "vauban")

  (setq circe-highlight-nick-type 'all)

  (when (try-require 'circe-highlight-all-nicks)
    (enable-circe-highlight-all-nicks))

  ;; (setq circe-server-coding-system '(latin-1 . undecided))

  (setq circe-format-self-say "<{nick}> {body}")

  (setq circe-server-auto-join-channels
        '(("^freenode$"
           "#emacs"
           "#gnus"
           "#latex"
           "#ledger"
           "#org-mode"
           "#stumpwm"
           "#zsh"
           "#erc"
           "#xemacs")))

  (setq circe-nickserv-passwords
        `(("freenode" ,freenode-passwd)))

  (setq lui-flyspell-p t)

  (setq lui-flyspell-alist '(("." "en_US")))

  (setq lui-max-buffer-size 30000)

  ;; (setq lui-fill-column 80)
  (setq lui-highlight-keywords '("[^<]vauban" "org" "beamer" "tikz"))

  (eval-after-load "circe"
    '(progn

       ;; add IRC color support to LUI
       (require 'lui-irc-colors)

       (add-to-list 'lui-pre-output-hook 'lui-irc-colors)))

  (defun irc ()
    "Connect to IRC."
    (interactive)
    (circe "irc.freenode.net" "6667" "freenode")
    ;; (circe "localhost" "6668" "bitlbee")
    )

  ;; (global-set-key (kbd "<f9>")  'irc)

  ;; (irc)
  ;; /whois
  ;; /leave
  )



(when (require 'emms-setup nil t)
  (require 'emms-player-simple)
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (emms-standard)
  (emms-default-players)
  (defalias 'np 'emms-show))
;; and it works ok,  I've got mplayer in the PATH.


(when (try-require 'ido-XXX)
  (ido-mode 1)

  (ido-everywhere 1)
  (setq ido-confirm-unique-completion t)
  (setq ido-enable-flex-matching t)

  ;; will use ffap-guesser to determine whether file name is at point
  (setq ido-use-filename-at-point 'guess)

  (setq org-completion-use-ido t))


;; SQL-mode
(setq sql-sqlite-program "sqlite3")


(try-require 'ess-site)



;; Google maps
(try-idle-require 'google-maps-XXX)



;; make Emacs aware of this package
(when (try-require 'command-frequency)

  (command-frequency-table-load)

  ;; load the program
  (command-frequency-mode 1)

  (command-frequency-autosave-mode 1))



;; This is for the sake of Emacs.
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:size "8pt" :family "Consolas")))))
;; Local Variables:
;; coding: utf-8
;; mode: emacs-lisp
;; mode: outline-minor
;; mode: rainbow
;; ispell-local-dictionary: "en_US"
;; End:

;;; .emacs ends here
