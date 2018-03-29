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

(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tide company-auctex auctex rvm seeing-is-believing ruby-electric robe pythonic elpy utop merlin tuareg markdown-mode js2-mode fish-completion fish-mode fsharp-mode cider ng2-mode counsel monokai-theme wc-mode nlinum realgud dockerfile-mode docker-compose-mode docker-api docker popup aggressive-indent flyspell-correct-ivy flycheck rainbow-delimiters yasnippet ivy company-quickhelp company-edbi company undo-tree magit avy-zap avy multiple-cursors which-key use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
