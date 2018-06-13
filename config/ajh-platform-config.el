;;; ajh-platform-config.el --- Platform-specific configurations
;;; Commentary:

;;; Code:


;;; macOS
(if (string-equal system-type "darwin")
    (setq ns-command-modifier 'meta)) ; set command key to be meta instead of option

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


;;; Windows NT
(when (string-equal system-type "windows-nt")
  (defun my-semantic-hook ()
    (semantic-add-system-include "C:/tools/mingw64/x86_64-w64-mingw32/include/" 'c-mode)
    (semantic-add-system-include "C:/tools/mingw64/x86_64-w64-mingw32/include/" 'c++-mode))
  (add-hook 'semantic-init-hooks 'my-semantic-hook)
  (set-face-attribute 'default nil
		      :family "Consolas"
		      :height 130
		      :weight 'normal
		      :width 'normal)
  (require 'tramp)
  (set-default 'tramp-default-method "plink")
  (add-to-list 'exec-path "C:/hunspell/bin")

  (setq python-environment-default-root-name "windows")

  (add-to-list 'TeX-view-program-list '("Sumatra PDF" ("sumatrapdf -reuse-instance" (mode-io-correlate " -forward-search %b %n -inverse-search \"emacsclientw --no-wait +%%l \\\"%%f\\\"\" ") " %o")))

  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))
  (setq python-shell-interpreter "jupyter"
	python-shell-interpreter-args "console --simple-prompt"))

;;; Linux
(if (string-equal system-type "gnu/linux")
    (progn
      (set-face-attribute 'default nil
			  :family "Inconsolata"
			  :height 100
			  :weight 'normal
			  :width 'normal)
      (setq elpy-rpc-python-command "python3")
      (setq python-shell-interpreter "ipython3"
	    python-shell-interpreter-args "-i --simple-prompt")))

      (provide 'ajh-platform-config)
;;; ajh-platform-config.el ends here
