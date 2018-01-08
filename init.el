;;; init.el --- Emacs startup file
;;; Commentary:

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(defun ajh-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location."
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
  )

(load (ajh-get-fullpath "config/ajh-package-config.el"))
(load (ajh-get-fullpath "config/ajh-global-packages.el"))
(load (ajh-get-fullpath "config/ajh-interface-config.el"))
(load (ajh-get-fullpath "config/ajh-language-config.el"))
(load (ajh-get-fullpath "config/ajh-misc-config.el"))
(load (ajh-get-fullpath "config/ajh-platform-config.el"))
(load (ajh-get-fullpath "config/ajh-work-config.el"))
(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (js2-mode which-key wc-mode utop use-package undo-tree tuareg seeing-is-believing rvm ruby-electric robe rainbow-delimiters pythonic nlinum multiple-cursors monokai-theme merlin markdown-mode magit fsharp-mode flyspell-correct-ivy fish-mode fish-completion enh-ruby-mode elpy dockerfile-mode docker-compose-mode docker-api docker counsel company-edbi company-auctex cider avy-zap aggressive-indent))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
