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

;; You may delete these explanatory comments.
(load (ajh-get-fullpath "config/ajh-package-config"))
(load (ajh-get-fullpath "config/ajh-global-packages.el"))
(load (ajh-get-fullpath "config/ajh-interface-config.el"))
(load (ajh-get-fullpath "config/ajh-language-config.el"))
(load (ajh-get-fullpath "config/ajh-misc-config.el"))
(load (ajh-get-fullpath "config/ajh-platform-config"))

(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (avy-zap zop-up-to-char avy docker-compose docker docker-api docker-compose-mode docker-tramp dockerfile-mode which-key utop use-package undo-tree tuareg robe rainbow-delimiters pythonic multiple-cursors monokai-theme merlin magit fsharp-mode flyspell-correct-ivy epc elpy counsel company-auctex cider aggressive-indent ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
