;;; ajh-language-config.el --- Language-specific configurations
;;; Commentary:
;;; Code


;;; Angular (Framework -- not really a language)
(use-package ng2-mode
  :ensure t)


;;; Clojure
(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t)


;;; F#
(use-package fsharp-mode
  :ensure t
  :config (progn
	    (setq inferior-fsharp-program "fsharpi --readline-")
	    (setq fsharp-compiler "fsharpc")))


;;; Fish
(use-package fish-mode
  :ensure t)

(use-package fish-completion
  :ensure t)


;;; Javascript
(use-package js2-mode
  :ensure t
  :config (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))


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


;;; Python
(use-package elpy
  :ensure t
  :config (progn
	    (elpy-enable)
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

(provide 'ajh-language-config)
;;; ajh-language-config.el ends here
