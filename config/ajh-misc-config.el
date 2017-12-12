;;; ajh-misc-config.el --- Miscellaneous Configuration
;;; Commentary:
;;; Code:

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-default 'tramp-auto-save-directory temporary-file-directory)

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(provide 'ajh-misc-config)
;;; ajh-misc-config.el ends here
