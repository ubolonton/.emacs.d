;; (setq ido-decorations '( "(" ")" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(setq ido-decorations (quote ("\n=> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-use-virtual-buffers t
      ido-max-directory-size 100000)
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(ublt/set-up 'ido
  (ido-everywhere +1))

(ublt/set-up 'ido-other-window)

(ublt/set-up 'smex
  (setq smex-save-file "~/.emacs.d/.smex-items"))

;; ido on steroid `http://www.emacswiki.org/emacs/InteractivelyDoThing'
(defvar ido-enable-replace-completing-read nil
  "If t, use ido-completing-read instead of completing-read if possible.

    Set it to nil using let in around-advice for functions where the
    original completing-read is required.  For example, if a function
    foo absolutely must use the original completing-read, define some
    advice like this:

    (defadvice foo (around original-completing-read-only activate)
      (let (ido-enable-replace-completing-read) ad-do-it))")
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (and (boundp 'ido-cur-list) ido-cur-list)
                                        ;ido-cur-list ; Emacs 24
          )  ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read
                 prompt
                 allcomp
                 nil require-match initial-input hist def))
        ad-do-it))))

(ublt/set-up 'flx-ido
  (flx-ido-mode +1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil
        flx-ido-use-faces t))

(provide 'ublt-ido)
