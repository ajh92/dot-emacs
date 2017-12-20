;;; ajh-interface-config.el -- Emacs UI Configuration
;;; Commentary:
; This package contains Emacs UI configuration options
;;; Code:

;;; Basic options
(setq inhibit-splash-screen t)

(display-time-mode 1)

(tool-bar-mode -1)
(menu-bar-mode -1)

(show-paren-mode 1)

(setq split-width-threshold nil)

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

(use-package nlinum
  :ensure t
  :config (global-nlinum-mode 1))

(use-package wc-mode
  :ensure t)
  

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(put 'narrow-to-region 'disabled nil)

;;; Keybindings

(global-set-key (kbd "C-+") 'expand-region)

(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(global-set-key (kbd "M-/") 'hippie-expand)


;;; Hippie Expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;; Themes
(use-package monokai-theme
  :ensure t
  :init (load-theme 'monokai t))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

(provide 'ajh-interface-config)
;;; ajh-interface-config.el ends here
