(require 'yaml-mode)
(require 'mmm-auto)
(require 'mmm-jinja2)

(defgroup ubolonton nil
  "")

;;; Arimo file name conventions...

;;; Copied from salt-mode.el

(define-derived-mode ublt-conf-space-jinja-mode conf-space-mode "Conf-Space"
  "A major mode to edit jinja templates for conf-space files."
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'ublt-conf-space-jinja-mode "\\.conf.jin\\'" 'jinja2))

(add-to-list 'auto-mode-alist '("\\.conf.jin\\'" . ublt-conf-space-jinja-mode))


(define-derived-mode ublt-xml-jinja-mode nxml-mode "XML"
  "A major mode to edit jinja templates for xml files."
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'ublt-xml-jinja-mode "\\.xml.jin\\'" 'jinja2))

(add-to-list 'auto-mode-alist '("\\.xml.jin\\'" . ublt-xml-jinja-mode))


(define-derived-mode ublt-sh-jinja-mode sh-mode "Sh"
  "A major mode to edit jinja templates for sh files."
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'ublt-sh-jinja-mode "\\.sh.jin\\'" 'jinja2))

(add-to-list 'auto-mode-alist '("\\.sh.jin\\'" . ublt-sh-jinja-mode) t)


;;; XXX: Breaks sometimes, highlighting stuff as strings.
(define-derived-mode ublt-python-jinja-mode python-mode "Python"
  "A major mode to edit jinja templates for python files."
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'ublt-python-jinja-mode "\\.py.jin\\'" 'jinja2))

(add-to-list 'auto-mode-alist '("\\.py.jin\\'" . ublt-python-jinja-mode) t)


;;; XXX: Doesn't work. Loses Dockerfile colors.
(define-derived-mode ublt-dockerfile-jinja-mode dockerfile-mode "Dockerfile"
  "A major mode to edit jinja templates for dockerfile files."
  (setq mmm-global-mode 'maybe)
  (mmm-add-mode-ext-class 'ublt-dockerfile-jinja-mode "Dockerfile.j2.template\\'" 'jinja2))

(add-to-list 'auto-mode-alist '("Dockerfile.j2.template\\'" . ublt-dockerfile-jinja-mode) t)


(provide 'ublt-mix-modes)
