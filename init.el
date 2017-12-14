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

(load (ajh-get-fullpath "config/ajh-package-config"))
(load (ajh-get-fullpath "config/ajh-global-packages.el"))
(load (ajh-get-fullpath "config/ajh-interface-config.el"))
(load (ajh-get-fullpath "config/ajh-language-config.el"))
(load (ajh-get-fullpath "config/ajh-misc-config.el"))
(load (ajh-get-fullpath "config/ajh-platform-config.el"))
(load (ajh-get-fullpath "config/ajh-work-config.el"))

(provide 'init)
;;; init.el ends here
