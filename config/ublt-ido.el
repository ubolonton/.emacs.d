;; (setq ido-decorations '( "(" ")" " | " " | ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
(setq ido-decorations (quote ("\n=> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
      ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-use-virtual-buffers t
      ido-max-directory-size 100000)
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(ido-everywhere +1)

(ublt/set-up 'ido-other-window)

(ublt/set-up 'smex
  (setq smex-save-file "~/.emacs.d/.smex-items"))

(provide 'ublt-ido)
