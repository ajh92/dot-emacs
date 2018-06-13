;;; ajh-global-packages.el --- Packages/Code used across modes
;;; Commentary:
;;; Code:

(semantic-mode 1)

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.6))



(use-package multiple-cursors
  :ensure t)

(use-package avy
  :ensure t
  :bind ("C-=" . avy-goto-char))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-up-to-char-dwim)
	 ("M-Z" . avy-zap-to-char-dwim))
  :config (setq avy-zap-dwim-prefer-avy nil))

(use-package magit
  :ensure t
  :bind(("C-c m" . magit-status))
  :config (setq magit-completing-read-function 'ivy-completing-read))

(use-package undo-tree
  :ensure t
  :bind ("C-M-/" . undo-tree-redo)
  :config (global-undo-tree-mode))

(use-package ispell
  :ensure t
  :config (progn
	    (setq ispell-program-name
		  (locate-file "hunspell" exec-path exec-suffixes 'file-executable-p))
	    (setq ispell-dictionary "american")))
(use-package company
  :ensure t
  :init (global-company-mode)
  :config (progn
	   (setq company-minimum-prefix-length 2)
	   (add-hook 'after-init-hook 'global-company-mode)
	   ))

(use-package company-edbi
  :ensure t
  :config (progn
	    (add-to-list 'company-backends 'company-edbi)
	    ))

(use-package company-quickhelp
  :ensure t
  :config (progn
	    (company-quickhelp-mode 1)
	    (setq company-quickhelp-delay 0.1)
	    ))

  (use-package ivy
    :ensure t
    :config (ivy-mode 1))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

(use-package aggressive-indent
  :ensure t)

(use-package popup
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package multiple-cursors
  :ensure t)


;;; Docker
(use-package docker
  :ensure t)

(use-package docker-api
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package docker-tramp
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package edbi
  :ensure t)

(use-package realgud
  :ensure t
  :init (load-library "realgud"))

(provide 'ajh-global-packages)
;;; ajh-global-packages.el ends here
