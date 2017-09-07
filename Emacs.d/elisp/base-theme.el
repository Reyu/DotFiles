(use-package solarized-theme
  :defer t
  :if window-system
  :init
  (load-theme 'solarized-dark t)
  :config
  (setq solarized-use-variable-pitch nil
	solarized-scale-org-headlines nil))

(provide 'base-theme)
