;;; keys.el --- Keybinding Configuration File --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Divya's keybindings for GNU Emacs

;;; Code:

;;; Vim Bindings
;; (use-package evil
;;              :straight t
;;              :bind (("<escape>" . keyboard-escape-quit))
;;              :init
;;              ;; allows for using cgn
;;              ;; (setq evil-search-module 'evil-search)
;;              (setq evil-want-keybinding nil)
;;              ;; no vim insert bindings
;;              (setq evil-undo-system 'undo-fu)
;;              (setq evil-want-C-u-scroll t)
;;              :config
;;              (evil-mode 1))


(use-package undo-fu
  :straight t)

(use-package undo-tree
  :straight t)

;;; Vim Bindings Everywhere else

;; (use-package evil-collection
;;              :straight t
;;              :after evil
;;              :config
;;              (setq evil-want-integration t)
;;              (evil-collection-init))

;; Vim like scrolling
(setq scroll-step            1
      scroll-conservatively  10000
      next-screen-context-lines 5
      ;; move by logical lines rather than visual lines (better for macros)
      line-move-visual nil)

;; Setting the leader key
;; (use-package general
;; 	     :straight t
;;              :init (general-auto-unbind-keys)
;;              :config
;;              (general-create-definer divya/evil
;;                                      :states '(normal))
;;              (general-create-definer divya/leader-keys
;;                                      :keymaps '(normal insert visual emacs)
;;                                      :prefix "spc"
;;                                      :global-prefix "c-spc")

;;              (divya/leader-keys
;;                "t" '(:ignore t :which-key "toggles")
;;                "." '(find-file :which-key "find file:") ;; remapping the default `c-x c-f`
;;                "tt" '(consult-theme :which-key "choose theme")))

;;;; Custom Keybindings

;; A macro to make life easier (From http://dotshare.it/dots/8464/)
(defmacro bind-all (&rest pairs)
  `(progn
     ,@(mapcar
         (lambda (p)
           (let ((key (car p))
                 (val (cdr p)))
             (if (consp (car val))
                 `(define-key keymap (kbd ,key) (lambda () (interactive) ,@val))
                 `(define-key keymap (kbd ,key) (quote ,(car val))))))
         pairs)))

;; Go to a particular directory and open dired
(defun divya/go-to-dired (s)
  (interactive)
  (find-file s))




;; (divya/leader-keys
;;   "d" directories-map :which-key "Directories")

;; Your directory navigation function
(defun divya/go-to-dired (s)
  (interactive)
  (find-file s))

;; Define a keymap for C-x d prefix
(defvar directories-map (make-sparse-keymap)
  "Keymap for navigating to directories")

(setq directories-map
      (let ((keymap (make-sparse-keymap)))
        (bind-all
          ("e"   (divya/go-to-dired "/mnt/LDisk-E/Albert Einstein/"))
          ("M"   (divya/go-to-dired "/mnt/LDisk-E/Albert Einstein/Movies & Series/"))
          ("b"   (divya/go-to-dired "~/books/"))
          ("B"   (divya/go-to-dired "~/bib/"))
          ("m"   (divya/go-to-dired "~/math/"))
          ("p"   (divya/go-to-dired "~/philosophy/"))
          ("w"   (divya/go-to-dired "~/writing/"))
          ("P"   (divya/go-to-dired "~/psychoanalysis"))
          ("l"   (divya/go-to-dired "~/literature/"))
          ("c"   (divya/go-to-dired "~/cs/"))
          ("C-p" (divya/go-to-dired "~/physics/"))
          ("s"   (divya/go-to-dired "~/sociology/"))
          ("N"   (divya/go-to-dired "~/neuroscience/"))

          ("n"   (divya/go-to-dired "~/notes/"))
          ("o"   (divya/go-to-dired "~/notes/org/org-roam/"))
          ("M-p"  (divya/go-to-dired "~/projects/"))
          ("L"   (divya/go-to-dired "~/life/"))
          ("S"   (divya/go-to-dired "~/Sync/"))

          ("d"   (divya/go-to-dired "~/.dotfiles/"))
          ("C-c" (divya/go-to-dired "~/.config/")))

        keymap))

;; Bind the C-x d prefix to the directories-map keymap
(global-set-key (kbd "C-x d") directories-map)



;;; Keybindings for window management

;; (divya/leader-keys
;;   "wq" 'evil-window-delete
;;   "wn" 'next-window
;;   "wp" 'previous-window
;;   "wh" 'evil-window-left
;;   "wj" 'evil-window-down
;;   "wk" 'evil-window-up
;;   "wl" 'evil-window-right
;;   "wH"  'evil-window-move-far-left
;;   "wJ"  'evil-window-move-very-top
;;   "wK" 'evil-window-move-very-bottom
;;   "wL"  'evil-window-move-far-right
;;   "wb" 'balance-windows
;;   "ws"  'evil-window-split
;;   "wv"  'evil-window-vsplit
;;   "wx"  'evil-window-exchange)

;; (divya/leader-keys
;;   "e" '(:ignore t :which-key "eval")
;;   "eb" '(eval-buffer :which-key "eval buffer")
;;   "uc" 'comment-or-uncomment-region :which "(Un)Commenting")

;; (divya/leader-keys
;;   :keymaps '(visual)
;;   "er" '(eval-region :which-key "eval region"))

;;; Meow

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("j" . dired-jump)
   ;; '("P" . (lambda () (interactive) (dired "~/psychoanalysis")))
   ;; '("p" . (lambda () (interactive) (dired "~/philosophy")))
   ;; '("b" . (lambda () (interactive) (dired "~/books")))
   ;; '("C" . (lambda () (interactive) (dired "~/cs")))
   ;; '("e" . (lambda () (interactive) (dired "~/physics")))
   ;; '("w" . (lambda () (interactive) (dired "~/writing")))
   ;; '("s" . (lambda () (interactive) (dired "~/sociology")))
   ;; '("M" . (lambda () (interactive) (dired "~/math")))
   ;; '("l" . (lambda () (interactive) (dired "~/literature")))
   '("?" . meow-cheatsheet))
 
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
 '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :straight t
  :config
  (meow-setup)
  (meow-setup-indicator)
  (meow-global-mode 1))

;; Some more vim stuff

;; (use-package evil-surround
;;   :straight t)

;; Expand-Region
(use-package expand-region
  :bind (("C-;" . er/expand-region)
	 ("C-(" . er/mark-outside-pairs)))

;;; Embark

(use-package embark
  :straight t

  :bind
  (("C-." . embark-act)
   ("C-'" . embark-dwim)
   ("C-h B" . embark-bindings)))

(use-package embark-consult
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'keys.el)

;;; keys.el ends here
