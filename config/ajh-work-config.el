;;; ajh-work-config.el --- Configuration specific to company machine
;;; Commentary:
;;; Code:
(setenv "PATH" (concat "c:/Users/ajhill/Apps/GetGnuWin32/gnuwin32/bin/" ";" (getenv "PATH")))
(setq exec-path (append exec-path '("C:/Users/ajhill/Apps/GetGnuWin32/gnuwin32/bin/")))




;;sql stuff
(require 'sql)
(sql-set-product "oracle")
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
(provide 'ajh-work-config)
;;; ajh-work-config.el ends here
