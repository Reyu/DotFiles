;; salt-mode configuration
;; https://github.com/glynnforrest/salt-mode
(use-package salt-mode
  :ensure t)

(use-package tex
  :ensure auctex
  :commands TeX-mode)

(use-package latex
  :ensure auctex
  :after tex)

(use-package auctex-latexmk
  :after tex)

(use-package company-auctex
  :after tex
  :config
  (company-auctex-init))

(provide 'lang-other)
