;;; ajh-work-config.el --- Configuration specific to company machine
;;; Commentary:
;;; Code:
(setenv "PATH"
	(concat "c:/Users/ajhill/Apps/GetGnuWin32/gnuwin32/bin/"
		";"
		"C:/Users/ajhill/AppData/Local/Continuum/anaconda3"
		";"
		"C:/Users/ajhill/AppData/Local/Continuum/anaconda3/Scripts"
		";"
		(getenv "PATH")))
(setq exec-path
      (append exec-path
	      '("C:/Users/ajhill/Apps/GetGnuWin32/gnuwin32/bin/")
	      '("C:/Users/ajhill/AppData/Local/Continuum/anaconda3")
	      '("C:/Users/ajhill/AppData/Local/Continuum/anaconda3/Scripts")))


(setq python-shell-interpreter "ipython")
(elpy-use-ipython "ipython")


;;sql stuff
(require 'sql)
(sql-set-product "oracle")
(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))
(provide 'ajh-work-config)
;;; ajh-work-config.el ends here
