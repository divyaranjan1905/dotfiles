;;; dired.el --- Emacs Dired Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's Dired congifuration

;;; Code:

;;; Dired

(use-package dired
  :straight (:type built-in)
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (define-key dired-mode-map (kbd "h") 'diredp-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-find-file)
  (setq dired-dwim-target t))

(require 'wdired)

(use-package fd-dired
  :straight t)

;;; Dirvish (not entirely sure)

(use-package dirvish
  :straight t
  :init
  (dirvish-override-dired-mode)
  (dirvish-side-follow-mode)
  (dirvish-peek-mode)
  :hook
  (dired-mode . dirvish-side-follow-mode)
  (dired-mode . dirvish-peek-mode)
  :custom
  (dirvish-quick-access-entries
   '(("h" "~/"     "Home")
     ("e" "/mnt/LDisk-E/Albert Einstein/"   "Albert Einstein")
     ("t" "~/.local/share/Trash/files/"     "TrashCan")
     ("b" "~/books/"    "Books")
     ("n" "~/notes/"    "Notes")
     ("m" "~/math/"     "Math")
     ("B" "~/bib/"      "Bibliography")
     ("M" "/mnt/LDisk-E/Albert Einstein/Movies & Series" "Movies")
     ("p" "~/philosophy/" "Philosophy")
     ("P" "~/psychoanalysis/" "Psychoanalysis")
     ("c" "~/cs/"       "Computer Science")
     ("l" "~/literature/" "Literature")
     ("o" "~/notes/org/org-roam")
     ("C-p" "~/physics/"  "Physics")
     ("L" "~/life/"      "Life")
     ("s" "~/sociology"  "Sociology")
     ("S" "~/Sync/"      "Sync")
     ("d" "~/.dotfiles"   "Dotfiles")
     ("C-c" "~/.config"  "Config")))
  :config
;  (setq dirvish-default-layout '(1 1 1))
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq delete-by-moving-to-trash t)
  (setq dirvish-use-mode-line nil)
;;  (setq dirvish-preview-dispatchers
;;	(cl-substitute 'pdf-preface 'pdf dirvish-preview-dispatchers))

  ;; :general
  ;; (divya/evil
  ;;   :keymaps 'dirvish-mode-map
  ;;   :packages '(dired dirvish)
  ;;   "q" #'dirvish-quit
  ;;   "TAB" #'dirvish-subtree-toggle)

  :bind ; Bind `dirvish|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish-fd)
   :map dirvish-mode-map ; Dirvish inherits `dired-mode-map'
   ("C-a"   . dirvish-quick-access)
   ("f"   . dirvish-file-info-menu)
   ("y"   . dirvish-yank-menu)
   ("H"	  . dired-hide-dotfiles-mode)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("C-z"   . dirvish-quicksort)    ; remapped `dired-sort-toggle-or-edit'
   ("v"   . dirvish-vc-menu)      ; remapped `dired-view-file'
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-l" . dirvish-ls-switches-menu)
   ("M-m" . dirvish-mark-menu)
   ("M-t" . dirvish-layout-toggle)
   ("M-s" . dirvish-setup-menu)
   ("M-e" . dirvish-emerge-menu)
   ("M-j" . dirvish-fd-jump)))


(use-package dired-rsync
  :straight t
  :if (executable-find "rsync")
  :defer t
  :config
  (define-key dired-mode-map (kbd "C-r") 'dired-rsync))
   ;; :general
  ;; (divya/evil
  ;;   :keymaps 'dired-mode-map
  ;;   :packages 'dired-rsync
  ;;   "C-r" #'dired-rsync))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :straight t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hoo
	    (lambda ()
	      (unless (file-remote-p default-directory)
		(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-use-term-integration t))


;;; Managing hidden dotfiles

(use-package dired-hide-dotfiles
  :straight t
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (define-key dired-moe-map (kbd "H") 'dired-hide-dotfiels-mode))

;;; Dired+
(require 'dired+)

(provide 'dired.el)
;;; dired.el ends here
