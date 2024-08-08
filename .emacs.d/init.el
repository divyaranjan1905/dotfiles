;; init.el --- Divya's Emacs Config --- -*- lexical-binding: t -*-

;;; Commentary:

;; My main Emacs initialization file.

;;; Code:

;;; Native compilation
(setq native-comp-speed 3)

;;; Less warnings

(setq warning-minimum-level :emergency)

;; Follow symlinks
(setq vc-follow-symlinks t)

;;; Directory Setup

;;; Setting up the UI

(load (concat user-emacs-directory
	      "config/ui.el"))

;;; Key bindings
(load (concat user-emacs-directory
	      "config/keys.el"))
;;; Buffer bindings

(load (concat user-emacs-directory
	      "config/buffers.el"))

;;; Window management in Emacs
(load (concat user-emacs-directory
	      "config/wm.el"))

;;; Org
(load (concat user-emacs-directory
	      "config/org.el"))

;;; Org-latex-preview
;; (load (concat user-emacs-directory
;; 	      "config/org-preview.el"))

;;; LaTeX in Emacs
(load (concat user-emacs-directory
	      "config/tex.el"))

;;; Mail in Emacs
(load (concat user-emacs-directory
	      "config/mail.el"))

;;; Dired
(load (concat user-emacs-directory
	      "config/dired.el"))

;;; Web browsing in Emacs
(load (concat user-emacs-directory
	      "config/eww.el"))

;;; Reading Books
(load (concat user-emacs-directory
	      "config/books.el"))

;;; Bookmarks
(load (concat user-emacs-directory
	      "config/bookmarks.el"))

;;; Feeds
(load (concat user-emacs-directory
	      "config/feed.el"))

;;; Anki
(load (concat user-emacs-directory
	      "config/anki.el"))

;;; IRC

;; (load (concat user-emacs-directory
;; 	      "config/irc.el"))

;;; Calendar
(load (concat user-emacs-directory
	      "config/cal.el"))

;;; Extras
(load (concat user-emacs-directory
	      "config/extras.el"))


;; So that Emacs doesn't produce ~ files
(setq make-backup-files nil)

;;;; Languages

;; Highlight symbols
(use-package symbol-overlay
  :straight t
  :hook ((rust-mode . symbol-overlay-mode)
	  (haskell-mode . symbol-overlay-mode)
	  (lisp-mode . symbol-overlay-mode)
	  (emacs-lisp-mode . symbol-overlay-mode)
	  (scheme-mode . symbol-overlay-mode)
	  (r-mode . symbol-overlay-mode)
	  (go-mode . symbol-overlay-mode)
	  (tex-mode . symbol-overlay-mode)
	  (racket-mode . symbol-overlay-mode)
	  (forth-mode . symbol-overlay-mode)))

;; Semantic Color Highlighting
(use-package color-identifiers-mode
  :straight t
  :init (global-color-identifiers-mode))

;; Compiler Explorer
(use-package rmsbolt
  :straight t)

;;; LSP Support in Emacs
(require 'eglot)

;; Extending Eglot
(use-package eglot-x
  :straight (eglot-x
	     :type git
	     :host github
	     :repo "nemethf/eglot-x"
	     :files ("*.el"))
  :init (eglot-x-setup))


;;; ElDoc Boxes
(require 'eldoc)
(global-eldoc-mode 1)

(use-package eldoc-box
  :straight t
  :hook  ((rust-mode . eldoc-box-hover-at-point-mode)
	  (haskell-mode . eldoc-box-hover-at-point-mode)
	  (go-mode . eldoc-box-hover-at-point-mode)
	  (r-mode . eldoc-box-hover-at-point-mode)
	  (racket-mode . eldoc-box-hover-at-point-mode)
	  (forth-mode . eldoc-box-hover-at-point-mode))
  :config
  (eldoc-box-only-multi-line t)
  :custom
  (set-face-font eldoc-box-body "Spline Sans Mono-11"))

;;; Golang
(use-package go-mode
  :straight t
  :hook (go-mode-hook . eglot-ensure))

;;; Haskell
(require 'ob-haskell)
(use-package haskell-mode
  :straight t
  :hook (haskell-mode . eglot-ensure)
  :bind
  (("C-c h" . haskell-hoogle))
  :config

  (let ((my-ghcup-path (expand-file-name "/home/divya/.ghcup/bin")))
    (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-ghcup-path)))

;; Hlint
(require 'hs-lint)
(defun my-haskell-mode-hook ()
    (local-set-key "\C-cl" 'hs-lint))
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; OCaml
;;(use-package ocaml-ts-mode)

(let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)
    ;; To easily change opam switches within a given Emacs session, you can
    ;; install the minor mode https://github.com/ProofGeneral/opam-switch-mode
    ;; and use one of its "OPSW" menus.
    ))


;; Rust
(use-package rust-mode
  ;; :straight (:build (:not compile autoloads))
  :straight t
  :hook (rust-mode . eglot-ensure)
  :hook (rust-mode . subword-mode))

;; (use-package rust-ts-mode
;;   :straight t
;;   :hook (rust-ts-mode . eglot-ensure)
;;   :hook (rust-ts-mode . subword-mode))

;;; Forth
(use-package forth-mode
  :straight t)

;;; Scheme
(use-package geiser
  :straight t)

;;; MIT/GNU Scheme
(use-package geiser-mit
  :straight t)

;;; Chez Scheme
(use-package geiser-chez
  :straight t)

;;; Guile
(use-package geiser-guile
  :straight t)

;;; Racket
(use-package racket-mode
  :straight t)

;;; Pollen
(use-package pollen-mode
  :straight t)

;;; Geiser Racket
(use-package geiser-racket
  :straight t)

;;; For Lisps in general
(use-package paredit
  :straight t
  :hook ((lisp-mode . paredit-mode)
	 (racket-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

;;; SLIME For Superior Lisp Editing
(use-package slime
  :straight t
  :hook (slime-repl-mode . paredit-mode)
  :config
  (setq inferior-lisp-program "sbcl"))


;;; Lean4
(use-package lean4-mode
  :straight (lean4-mode
	     :type git
	     :host github
	     :repo "leanprover/lean4-mode"
	     :files ("*.el" "data"))
  ;; to defer loading the package until required
  :commands (lean4-mode))

;;; Emacs Lisp

;; (add-hook 'emacs-lisp-mode-hook #'flycheck-mode)

;; (use-package elisp-refs
;;   :demand t
;;   :straight
;;   (elisp-refs :type git :host github :repo "Wilfred/elisp-refs"
;;                    ;; Skip the autoloads phase because straight.el can't seem to get it right.
;;                    :build (:not autoloads)))

(use-package helpful
  :straight t
  :bind
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Markdown

(use-package markdown-mode
  :straight t
  :mode "\\.md\\'"
  :hook
  (markdown-mode . writeroom-mode)
  :config
  (setq markdown-command "marked")
  (defun divya/set-markdown-header-font-sizes ()
    (dolist (face '((markdown-header-face-1 . 1.2)
		    (markdown-header-face-2 . 1.1)
		    (markdown-header-face-3 . 1.0)
		    (markdown-header-face-4 . 1.0)
		    (markdown-header-face-5 . 1.0)))
      (set-face-attribute (car face) nil :weight 'normal :height (cdr face)))

    (set-face-attribute 'variable-pitch nil :font "ET Bembo"))

  (defun divya/markdown-mode-hook ()
    (divya/set-markdown-header-font-sizes))

  (add-hook 'markdown-mode-hook 'divya/markdown-mode-hook))

;; HTML

(use-package web-mode
  :mode "(\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'"
  :config
  (setq-default web-mode-code-indent-offset 2)
  (setq-default web-mode-markup-indent-offset 2)
  (setq-default web-mode-attribute-indent-offset 2))

;; ;; 1. Start the server with `httpd-start'
;; ;; 2. Use `impatient-mode' on any buffer
(use-package impatient-mode
  :straight t)

(use-package skewer-mode
  :straight t)

;; Compilation

(use-package compile
  :straight t
  :custom
  (compilation-scroll-output t))

(defun auto-recompile-buffer ()
  (interactive)
  (if (member #'recompile after-save-hook)
      (remove-hook 'after-save-hook #'recompile t)
    (add-hook 'after-save-hook #'recompile nil t)))

;; Syntax checking

;; (use-package flycheck
;;   :defer t
;;   :hook (prog-mode . flycheck-mode))

;;; Project Management
;; We don't need projectile, Emacs <27.1 has an inbuilt package `project.el` that does most of what I'd need. I am keeping the projectile config here for just in case.
;; Project.el already comes binded to `C-x p`

(require 'project)

(setq project--list '(("/home/divya/.dotfiles/")
		      ("/home/divya/src/")
		      ("/home/divya/projects/")
		      ("/mnt/LDisk-D/big-src/")
		      ("/mnt/LDisk-E/Albert Einstein/Books & Resources/Sociology/Gender Mainstreaming/Megha/")))

(define-key project-prefix-map (kbd "b") 'consult-project-buffer)

;; Magit :  The best git management there is!

(use-package magit
  :straight t
  :bind-keymap
  ("C-M-;" . magit-status)
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;;; Code Completion
;; Autocomplete
;; (use-package auto-complete
;;   :straight t
;;   :config
;;   (ac-config-default))

;; Corfu

(use-package corfu
  :straight t
  :hook (prog-mode . corfu-mode)
  :custom
  (corfu-auto t)          ;; Enable auto completion
  ;; (corfu-separator ?_) ;; Set to orderless separator, if not using space
  :bind
  ;; Another key binding can be used, such as S-SPC.
  (:map corfu-map ("M-SPC" . corfu-insert-separator))
  :init
  ;(global-corfu-mode)
  )

;; using corfu in terminal

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))


;; (use-package cape
;;   ;; Bind dedicated completion commands
;;   ;; Alternative prefix keys: C-c p, M-p, M-+, ...
;;   :bind (("C-c p p" . completion-at-point) ;; capf
;;          ("C-c p t" . complete-tag)        ;; etags
;;          ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;          ("C-c p h" . cape-history)
;;          ("C-c p f" . cape-file)
;;          ("C-c p k" . cape-keyword)
;;          ("C-c p s" . cape-elisp-symbol)
;;          ("C-c p e" . cape-elisp-block)
;;          ("C-c p a" . cape-abbrev)
;;          ("C-c p l" . cape-line)
;;          ("C-c p w" . cape-dict)
;;          ("C-c p :" . cape-emoji)
;;          ("C-c p \\" . cape-tex)
;;          ("C-c p _" . cape-tex)
;;          ("C-c p ^" . cape-tex)
;;          ("C-c p &" . cape-sgml)
;;          ("C-c p r" . cape-rfc1345))
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-file)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-block)
;;   (add-to-list 'completion-at-point-functions #'cape-history)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   (add-to-list 'completion-at-point-functions #'cape-tex)
;;   (add-to-list 'completion-at-point-functions #'cape-sgml)
;;   (add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-dict)
;;   (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
;;   (add-to-list 'completion-at-point-functions #'cape-line)
;; )

;; Telegram in Emacs
;; (use-package telega
;;   :straight t
;;   :config
;;   (setq telega-use-images t
;;    telega-server-libs-prefix "/usr"
;;    telega-autoplay-mode t))

;;; Shell in Emacs

;; A pop up shell
(use-package shell-pop
  :straight t
  ;;  :defer t
  :bind
  (("C-x /" . 'shell-pop))
  :custom
  (shell-pop-default-directory nil)
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda () (eshell shell-pop-term-shell)))))
  (shell-pop-window-size 30)
  (shell-pop-full-span nil)
  (shell-pop-window-position "bottom")
  (shell-pop-autocd-to-working-dir t)
  (shell-pop-restore-window-configuration t)
  (shell-pop-cleanup-buffer-at-process-exit t))

;; Main Eshell config

(defun divya/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer))

  ;; Bind some useful keys for evil-mode
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  ;; (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  ;; (evil-normalize-keymaps)

  ;; (setq eshell-history-size         10000
  ;; 	eshell-buffer-maximum-lines 10000
  ;; 	eshell-hist-ignoredups t
  ;; 	eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :straight t)

(use-package eshell
  :straight t
  :hook (eshell-first-time-mode . divya/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))

  (eshell-git-prompt-use-theme 'powerline))


;; Searching and navigation
;; (use-package ctrlf
;;   :straight t)


;; Start Emacs as a server!
;;(server-mode)

;; Hooks
;; (add-hook 'eww-mode-hook #'divya/enable-focus)


;;; For restarting emacs
(define-key global-map (kbd "s-r") 'restart-emacs)

;;; Garbage collection in Emacs

(use-package gcmh
  :straight t
  :init
  (gcmh-mode 1))

;; Load the appropriate theme
;; (circadian-setup)

;;; Report how long packages take to load
(setq use-package-verbose t)

;;; Tree-sitter
(setq treesit-extra-load-path  '("~/.emacs.d/tree-sitter/"))

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(go "https://github.com/tree-sitter/tree-sitter-go")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(rust "https://github.com/tree-sitter/tree-sitter-rust")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(typescript "https://github.com/tree-sitter/tree-sitte" "master" "typescript/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; Quickly open the init

(define-key global-map (kbd "C-c i") 'crux-find-user-init-file)




(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(gameoflife-screensaver-mode t)
 '(org-agenda-files
   '("/home/divya/notes/org/org-agenda/tasks.org"
     "/home/divya/notes/org/org-agenda/habits.org"
     "/home/divya/notes/org/org-roam/projects/on_hermeneutic_temptation.org"
     "/home/divya/notes/org/org-roam/ref/kaggle_introduction_to_machine_learning.org"
     "/home/divya/notes/org/org-roam/ref/kaggle_intro_to_deep_learning.org"
     "/home/divya/notes/org/org-roam/main/category_theory.org"
     "/home/divya/notes/org/org-roam/main/kaggle.org"
     "/home/divya/notes/org/org-roam/main/foundations_of_machine_learning.org"
     "/home/divya/notes/org/org-roam/main/intuitionism.org"
     "/home/divya/notes/org/org-roam/main/computer_vision.org"
     "/home/divya/notes/org/org-roam/projects/org_mobile.org"
     "/home/divya/notes/org/org-roam/projects/digit_recognizer.org"
     "/home/divya/notes/org/org-roam/projects/biblio_rogue.org"
     "/home/divya/notes/org/org-roam/projects/vesuvius_challenge.org"
     "/home/divya/notes/org/org-roam/projects/retracing_freud_s_oeuvre.org"
     "/home/divya/notes/org/org-roam/projects/bibliophile_el.org"
     "/home/divya/notes/org/org-roam/ref/in_search_of_lost_time.org"
     "/home/divya/notes/org/org-roam/ref/stanford_231n.org"
     "/home/divya/notes/org/org-roam/projects/reader_el.org"
     "/home/divya/notes/org/org-roam/main/mathematical_logic.org"
     "/home/divya/notes/org/org-roam/ref/18_905_algebraic_topology_i.org"
     "/home/divya/notes/org/org-roam/projects/thesis_gender_mainstreaming_in_urban_governance_a_study_of_women_councillors_of_ajmer_division_in_rajasthan.org"
     "/home/divya/notes/org/org-roam/projects/bibliotheca_aeterna.org"
     "/home/divya/notes/org/journal/20240805.org.gpg")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
