(setq inhibit-splash-screen t)
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/yasnippet"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

(setq tool-bar-mode nil)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("marmalade" . "https://marmalade-repo.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")))

(package-initialize)

(defvar my/packages '(magit magit-popup ace-jump-mode aggressive-indent cider clojure-mode elpy auctex company-quickhelp company edbi epc ctable concurrent deferred git-commit minimap monokai-theme pkg-info epl popup pos-tip powershell pythonic rainbow-delimiters robe inf-ruby powerline swiper wc-mode sqlup-mode multiple-cursors undo-tree company-auctex monky flycheck utop tuareg fsharp-mode which-key))

(require 'cl-lib)

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(my/install-packages)

(setq split-width-threshold nil)

(which-key-mode)
(setq which-key-idle-delay 0.6)

(load-theme 'monokai t)

(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

; set command key to be meta instead of option
(if (string-equal system-type "darwin")
    (setq ns-command-modifier 'meta))

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'powerline)
(powerline-default-theme)

(global-linum-mode 1)

(require 'multiple-cursors)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-set-key (kbd "M-Z") 'zap-to-char)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-=") 'ace-jump-mode)


(global-set-key (kbd "C-+") 'expand-region)
(global-set-key (kbd "C-c m") 'magit-status)
(setq magit-last-seen-setup-instructions "1.4.0")

;;ivy
(ivy-mode 1)
(setq magit-completing-read-function 'ivy-completing-read)

(global-undo-tree-mode)
(global-set-key (kbd "C-M-/") 'undo-tree-redo)

;;spelling
(require 'ispell)
(setq ispell-program-name "aspell")
(setq ispell-dictionary "american")


(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;;company mode
(require 'company)
(setq company-idle-delay .01)
(setq company-minimum-prefix-length 2)
(add-hook 'after-init-hook 'global-company-mode)
(add-to-list 'company-backends 'company-edbi)
(company-quickhelp-mode 1)

;;hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;;yasnippet
(require 'yasnippet)
(yas-global-mode 1)

(elpy-enable)
(setq elpy-rpc-python-command "/usr/local/bin/python3")
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(elpy-use-ipython)

;;ruby
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

					;flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;;; AUCTeX
;; Customary Customization, p. 1 and 16 in the manual, and http://www.emacswiki.org/emacs/AUCTeX#toc2
(require 'tex)
(require 'company-auctex)
(company-auctex-init)
(setq TeX-parse-self t); Enable parse on load.
(setq TeX-auto-save t); Enable parse on save.

(setq TeX-PDF-mode t); PDF mode (rather than DVI-mode)

;;auto-indent
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)

;;SyncTeX
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)


(add-hook 'TeX-mode-hook 'visual-line-mode)
(add-hook 'TeX-mode-hook 'flyspell-mode); Enable Flyspell mode for TeX modes such as AUCTeX. Highlights all misspelled words.
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode); Enable Flyspell program mode for emacs lisp mode, which highlights all misspelled words in comments and strings.
(add-hook 'TeX-mode-hook
          (lambda () (TeX-fold-mode 1))); Automatically activate TeX-fold-mode.
(setq LaTeX-babel-hyphen nil); Disable language-specific hyphen insertion.

;; " expands into csquotes macros (for this to work babel must be loaded after csquotes).
(setq LaTeX-csquotes-close-quote "}"
      LaTeX-csquotes-open-quote "\\enquote{")

;; LaTeX-math-mode http://www.gnu.org/s/auctex/manual/auctex/Mathematics.html
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)

;;; RefTeX
;; Turn on RefTeX for AUCTeX http://www.gnu.org/s/auctex/manual/reftex/reftex_5.html
(add-hook 'TeX-mode-hook 'turn-on-reftex)

(eval-after-load 'reftex-vars; Is this construct really needed?
  '(progn
     (setq reftex-cite-prompt-optional-args t); Prompt for empty optional arguments in cite macros.
     ;; Make RefTeX interact with AUCTeX, http://www.gnu.org/s/auctex/manual/reftex/AUCTeX_002dRefTeX-Interface.html
     (setq reftex-plug-into-AUCTeX t)
     ;; So that RefTeX also recognizes \addbibresource. Note that you
     ;; can't use $HOME in path for \addbibresource but that "~"
     ;; works.
     (setq reftex-bibliography-commands '("bibliography" "nobibliography" "addbibresource"))
					;     (setq reftex-default-bibliography '("UNCOMMENT LINE AND INSERT PATH TO YOUR BIBLIOGRAPHY HERE")); So that RefTeX in Org-mode knows bibliography
     (setcdr (assoc 'caption reftex-default-context-regexps) "\\\\\\(rot\\|sub\\)?caption\\*?[[{]"); Recognize \subcaptions, e.g. reftex-citation
     (setq reftex-cite-format; Get ReTeX with biblatex, see http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
           '((?t . "\\textcite[]{%l}")
             (?a . "\\autocite[]{%l}")
             (?c . "\\cite[]{%l}")
             (?s . "\\smartcite[]{%l}")
             (?f . "\\footcite[]{%l}")
             (?n . "\\nocite{%l}")
             (?b . "\\blockcquote[]{%l}{}")))))

;; Fontification (remove unnecessary entries as you notice them) http://lists.gnnu.org/archive/html/emacs-orgmode/2009-05/msg00236.html http://www.gnu.org/software/auctex/manual/auctex/Fontification-of-macros.html
(setq font-latex-match-reference-keywords
      '(
        ;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ;; ("cite" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
					; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")))

(setq font-latex-match-textual-keywords
      '(
        ;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; supcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(
        ;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")))

;;cedet
(semantic-mode 1)

(require 'wc-mode)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

					;Windows specific stuff			; ; ; ;
(when (string-equal system-type "windows-nt")

  (defun my-semantic-hook ()
    (semantic-add-system-include "C:/tools/mingw64/x86_64-w64-mingw32/include/" 'c-mode)
    (semantic-add-system-include "C:/tools/mingw64/x86_64-w64-mingw32/include/" 'c++-mode))
  (add-hook 'semantic-init-hooks 'my-semantic-hook)


					;Windows Tramp
  (require 'tramp)
  (set-default 'tramp-auto-save-directory "C:/Users/Andrew/AppData/Local/Temp")
  (set-default 'tramp-default-method "plink")


  (setq python-environment-default-root-name "windows")

  ;; Enables forward search and inverse search.
  (add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
  ;; Make default output format to be PDF (rather than DVI)
  (add-hook 'TeX-mode-hook 'TeX-PDF-mode)
  (add-to-list 'TeX-view-program-list '("Sumatra PDF" ("sumatrapdf -reuse-instance" (mode-io-correlate " -forward-search %b %n -inverse-search \"emacsclientw --no-wait +%%l \\\"%%f\\\"\" ") " %o")))

  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))

  );end windows config


					;Linux specific config
;; (when (not (string-equal system-type "windows-nt"))

;;   )
(put 'narrow-to-region 'disabled nil)


(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;bibtex from doi
(defun get-bibtex-from-doi (doi)
  "Get a BibTeX entry from the DOI"
  (interactive "MDOI: ")
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
	(url-retrieve-synchronously
	 (format "http://dx.doi.org/%s"
		 (replace-regexp-in-string "http://dx.doi.org/" "" doi)))
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (setq bibtex-entry
	    (buffer-substring
	     (string-match "@" (buffer-string))
	     (point)))
      (kill-buffer (current-buffer))))
  (insert (decode-coding-string bibtex-entry 'utf-8))
  (bibtex-fill-entry))


;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
;; turn on abbrev mode globally
(setq-default abbrev-mode t)

;;window resizing
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)


;;OCaml
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)
; Make company aware of merlin
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)

;;F#
(setq inferior-fsharp-program "fsharpi --readline-")
(setq fsharp-compiler "fsharpc")

