;;; init.el --- Emacs startup file
;;; Commentary:

;;; Code:

;;; Package Setup
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/yasnippet"))
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes/"))

(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("elpy" . "https://jorgenschaefer.github.io/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


(defun ajh-get-fullpath (@file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location."
  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
  )


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)

(use-package paredit
  :ensure t)

(use-package highlight-indentation
  :ensure t
  :config (progn (set-face-background 'highlight-indentation-face "#e3e3d3")
                 (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")))
  
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config (setq which-key-idle-delay 0.6))

(use-package multiple-cursors
  :ensure t)


(use-package reformatter
  :ensure t
  :config
                                        ; Adds a reformatter configuration called "+elixir-format"
                                        ; This uses "mix format -"
  (reformatter-define +elixir-format
                      :program "mix"
                      :args '("format" "-"))
                                        ; defines a function that looks for the .formatter.exs file used by mix format
  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
                                        ; adds an advice to the generated function +elxir-format-region that sets the proper root dir
                                        ; mix format needs to be run from the root directory otherwise it wont use the formatter configuration
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)
                                        ; Adds a hook to the major-mode that will add the generated function +elixir-format-on-save-mode
                                        ; So, every time we save an elixir file it will try to find a .formatter.exs and then run mix format from
                                        ; that file's directory
  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))

(use-package avy
  :ensure t
  :bind ("C-'" . avy-goto-char))

(use-package avy-zap
  :ensure t
  :bind (("M-z" . avy-zap-up-to-char-dwim)
	 ("M-Z" . avy-zap-to-char-dwim))
  :config (setq avy-zap-dwim-prefer-avy nil))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window)
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package golden-ratio
  :ensure t
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1)
  :config (progn (setq golden-ratio-auto-scale t)
		 (add-to-list 'golden-ratio-extra-commands 'ace-window)))

(use-package magit
  :ensure t
  :bind(("C-c m" . magit-status))
  :config (progn
	    (setq magit-completing-read-function 'ivy-completing-read)
	    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)
            (transient-replace-suffix 'magit-branch 'magit-checkout
              '("b" "dwim" magit-branch-or-checkout))
	    (setq magit-clone-set-remote.pushDefault t)))

(use-package undo-tree
  :ensure t
  :bind ("C-M-/" . undo-tree-redo)
  :config (global-undo-tree-mode))

(use-package ispell
  :ensure t
  :config (progn
	    (setq ispell-program-name
		  (locate-file "aspell" exec-path exec-suffixes 'file-executable-p))
	    (setq ispell-dictionary "en_US")
            (setq-default ispell-extra-args '("--sug-mode=ultra"
                                              "--camel-case"))))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config (progn
	    (setq company-minimum-prefix-length 2)
	    (add-hook 'after-init-hook 'global-company-mode)
	    (define-key company-active-map (kbd "M-n") nil)
	    (define-key company-active-map (kbd "M-p") nil)
	    (define-key company-active-map (kbd "C-n") #'company-select-next)
	    (define-key company-active-map (kbd "C-p") #'company-select-previous)
	    ))

(use-package company-quickhelp
  :ensure t
  :config (progn
	    (company-quickhelp-mode 1)
	    (setq company-quickhelp-delay 0.1)
	    ))

(use-package company-restclient
  :ensure t
  :config (add-to-list 'company-backend 'company-restclient))

(use-package lsp-mode
  :ensure t
  :commands lsp
  :diminish lsp-mode
  :config (setq lsp-prefer-flymake nil)
  :hook
  (elixir-mode . lsp)
  :init
  (add-to-list 'exec-path "/Users/andrew/.elixir-ls/")
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config (progn
	    (setq lsp-ui-doc-position 'at-point)
	    (setq lsp-ui-flycheck-enable t)
	    (setq lsp-ui-doc-use-childframe nil)
	    ))


(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config (push 'company-lsp company-backends))

(use-package project
 :ensure t)

(use-package eglot
  :ensure t)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :ensure t)

(use-package ivy
  :ensure t
  :config (ivy-mode 1))

(use-package yasnippet
  :ensure t
  :config (yas-global-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flyspell-correct-ivy
  :ensure t
  :after flyspell)

(use-package aggressive-indent
  :ensure t)

(use-package popup
  :ensure t)

(use-package pos-tip
  :ensure t)

(use-package multiple-cursors
  :ensure t)


;;; Docker
(use-package docker
  :ensure t)

(use-package docker-api
  :ensure t)

(use-package docker-compose-mode
  :ensure t)

(use-package docker-tramp
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package edbi
  :ensure t)

(use-package realgud
  :ensure t
  :init (load-library "realgud"))



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

(add-to-list 'auto-mode-alist '("\\.nuspec\\'" . nxml-mode))

(add-hook 'mmm-mode-hook
          (lambda ()
            (set-face-background 'mmm-default-submode-face nil)))

;;; Keybindings

(global-set-key (kbd "C-+") 'expand-region)

(global-set-key (kbd "C-s-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-s-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-s-<down>") 'shrink-window)
(global-set-key (kbd "C-s-<up>") 'enlarge-window)

(global-set-key (kbd "M-/") 'hippie-expand)


;;; Hippie Expand
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; ;;; Themes
(use-package flatui-theme
  :ensure t
  :init (load-theme 'flatui t))

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



;;; Angular
(use-package ng2-mode
  :ensure t)


;;; Clojure
(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)


(use-package elixir-mode
  :ensure t)


;;; F#
(use-package fsharp-mode
  :defer t
  :ensure t
  :config (require 'eglot-fsharp))


;;; Fish
(use-package fish-mode
  :ensure t)

(use-package fish-completion
  :ensure t)


;;; Javascript
(use-package js2-mode
  :ensure t
  :config (progn
	    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
	    (add-hook 'js2-mode-hook (lambda () (tern-mode) (company-mode)))
	    )
  )

(use-package npm-mode
  :ensure t)

(setq js2-strict-missing-semi-warning nil)
(setq js2-missing-semi-one-line-override nil)
(setq js-indent-level 2)


;;; Markdown
(use-package markdown-mode
  :ensure t
  :config(progn (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
		(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))


;;; Ocaml
(use-package tuareg
  :ensure t)

(use-package merlin
  :ensure t
  :init (progn
	  (add-hook 'tuareg-mode-hook 'merlin-mode)
	  (add-hook 'caml-mode-hook 'merlin-mode)))

					; Make company aware of merlin
(with-eval-after-load 'company
  (add-to-list 'company-backends 'merlin-company-backend))

					; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)

(use-package utop
  :ensure t)


;;; Powershell
(use-package powershell
  :ensure t)


;;; Python
(use-package elpy
  :ensure t
  :config (progn
	    (elpy-enable)
            (setq python-indent-offset 4)
	    (setq elpy-rpc-backend "jedi")))

(defadvice realgud:pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline (make-symbol "python -m pdb")
   	 		    (file-name-nondirectory buffer-file-name)))))

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline (make-symbol "python -m pdb")
   	 		    (file-name-nondirectory buffer-file-name)))))

(use-package epc
  :ensure t)

(use-package pythonic
  :ensure t)


;;; Racket
(use-package racket-mode
  :ensure t)

(use-package geiser
  :ensure t)


;;; Ruby
(use-package robe
  :ensure t)

(use-package inf-ruby
  :ensure t)

(use-package ruby-electric
  :ensure t)

(use-package seeing-is-believing
  :ensure t)

(use-package rvm
  :ensure t)

(rvm-use-default)

(add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . ruby-mode))

(add-hook 'ruby-mode-hook 'robe-mode)
(push 'company-robe company-backends)

(add-hook 'ruby-mode-hook 'highlight-indentation-mode)


;;; TeX

(use-package tex
  :ensure auctex
  :init (progn
	  (add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
	  (add-hook 'TeX-mode-hook
		    (lambda () (TeX-fold-mode 1)))
	  (add-hook 'TeX-mode-hook 'LaTeX-math-mode)
	  (add-hook 'TeX-mode-hook 'turn-on-reftex)
	  (add-hook 'TeX-mode-hook 'TeX-source-correlate-mode)
	  (add-hook 'TeX-mode-hook 'TeX-PDF-mode))
  :config (progn
	    (setq TeX-parse-self t)
	    (setq TeX-auto-save t)
	    (setq TeX-PDF-mode t)
	    (setq TeX-source-correlate-mode t)
	    (setq TeX-source-correlate-method 'synctex)
	    (setq LaTeX-babel-hyphen nil)
	    (setq LaTeX-csquotes-close-quote "}"
		  LaTeX-csquotes-open-quote "\\enquote{")))

(use-package company-auctex
  :ensure t
  :init (company-auctex-init))

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


;;; Typescript
(use-package tide
  :ensure t
  :config (progn
	    (defun setup-tide-mode ()
	      (interactive)
	      (tide-setup)
	      (flycheck-mode +1)
	      (setq flycheck-check-syntax-automatically '(save mode-enabled))
	      (eldoc-mode +1)
	      (tide-hl-identifier-mode +1)
	      ;; company is an optional dependency. You have to
	      ;; install it separately via package-install
	      ;; `M-x package-install [ret] company`
	      (company-mode +1))

	    ;; aligns annotation to the right hand side
	    (setq company-tooltip-align-annotations t)

	    ;; formats the buffer before saving
	    (add-hook 'before-save-hook 'tide-format-before-save)

	    (add-hook 'typescript-mode-hook #'setup-tide-mode)
	    (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
	    (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
	    )
  )


;;; Vue
(use-package vue-mode
  :ensure t)
(use-package vue-html-mode
  :ensure t)

(add-hook 'vue-mode-hook #'lsp)


;;; YAML
(add-hook 'yaml-mode-hook 'highlight-indentation-mode)

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

(setq backup-by-copying-when-linked t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq vc-make-backup-files t)

(setq create-lockfiles nil)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-aggressive-indent-mode 1)


;;; macOS
(when (eq system-type 'darwin) ;; mac specific setting
  (use-package exec-path-from-shell
    :ensure t
    :config (exec-path-from-shell-initialize))
  (use-package mac-pseudo-daemon
    :ensure t)
  (setq mac-option-modifier 'meta)
  (setq insert-directory-program (executable-find "gls")) ;; use gnu ls (better dired support)
  (setq elpy-rpc-python-command "python3")
  (setq elpy-shell-echo-output nil)
  (setq python-shell-interpreter "ipython3"
        python-shell-interpreter-args "--simple-prompt -c exec('__import__(\\'readline\\')') -i")
  (server-start)
  (mac-pseudo-daemon-mode)
  )


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

  ;; Improve magit performance
  (progn
    (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
    (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH"))))

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
			  :family "Ubuntu Mono"
			  :height 120
			  :weight 'normal
			  :width 'normal)
      (setq elpy-rpc-python-command "python3")
      (setq python-shell-interpreter "ipython3"
	    python-shell-interpreter-args "-i --simple-prompt")))

(provide 'init)
