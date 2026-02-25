(setq-default make-backup-files nil
      truncate-lines t
      create-lockfiles nil
      auto-save-default nil
      save-interprogram-paste-before-kill t
      async-shell-command-buffer 'new-buffer
      help-window-select t
      history-length 25
      use-dialog-box nil
      dired-dwim-target t
      electric-indent-inhibit t
      backward-delete-char-untabify-method 'hungry
      display-line-numbers-type 'visual
      indent-tabs-mode nil
      custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory)
      dired-kill-when-opening-new-dired-buffer t
      remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t
      split-width-threshold 1 ;; Prever side by side splits
      next-screen-context-lines 2)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
;; Terminal setup for tmux terms (truecolor + italics support)
(add-to-list 'term-file-aliases '("tmux-direct" . "xterm-direct"))
(add-to-list 'term-file-aliases '("tmux-256color" . "xterm-direct"))
(setq xterm-extra-capabilities '(invisible-text))

;; Don't override TERM - use what tmux sets
(setq frame-background-mode 'dark)
(set-terminal-parameter nil 'background-mode 'dark)
(toggle-enable-multibyte-characters t)

(blink-cursor-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode 1)
(electric-indent-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(put 'dired-find-alternate-file 'disabled nil)

;; Builtin packages setup
(use-package which-key
  :disabled
  :diminish which-key-mode
  :config (which-key-mode t))

(use-package ansi-color ;; Properly handle colors in compilation buffers
  :config
  (defun user/ansi-colorize-buffer ()
    (let ((buffer-read-only nil))
      (ansi-color-apply-on-region (point-min) (point-max))))
  :hook
  (compilation-filter . user/ansi-colorize-buffer))

(use-package compile
  :custom
  (compilation-max-output-line-length 5000)
  (compilation-scroll-output t)
  (compilation-buffer-name-function (lambda (_) (concat "*" compile-command "*")))
  :bind
  ("<f8>" . user/recompile)
  ("<f9>" . user/compile)
  :config
  (defun user/compile () (interactive) (if (project-current) (project-compile) (compile)))
  (defun user/recompile () (interactive) (if (project-current) (project-recompile) (recompile)))
  (dolist (regex '('(biome-lint "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s.*\s━+$" 1 2 3 2 1)
                 '(tsc "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s-\serror\s.*$" 1 2 3 2 1)
                 '(ruff "^ *--> \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3)))
  (add-to-list 'compilation-error-regexp-alist-alist regex)
  (add-to-list 'compilation-error-regexp-alist-alist (car regex))))

(use-package eldoc ;; There is no place for the annoying documentation
  :config
  (global-eldoc-mode -1))

(use-package ls-lisp ;; Sort directories first in dired
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

;; Custom built-in binds
(use-package emacs
  :bind
  ("C-x ;" . comment-line)
  ("C-x C-b" . ibuffer)
  ("C-x k" . kill-current-buffer)
  ("C-x K" . kill-buffer)
  ("C-x /" . comment-or-uncomment-region)
  ("C-c d" . duplicate-line)
  ("C-c r" . replace-regexp)
  ("C-c R" . replace-string))

;; Custom simple binds
(defun user/smart-kill-back()
  "Kill word back if region is not selected else kill region."
  (interactive)
  (call-interactively
   (if (region-active-p)
       'kill-region

       'backward-kill-word)))

(use-package emacs
  :bind
  ("C-v" . scroll-up)
  ("M-v" . scroll-down)
  ("C-w" . user/smart-kill-back)
  ("C-M-p" . previous-buffer)
  ("C-M-n" . next-buffer))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(require 'package)
(require 'use-package)

(use-package diminish :ensure t)
(use-package f :ensure t)
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))
(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode 1))

(use-package expand-region
  :defer 1
  :ensure t
  :bind
  ("M-;" . er/expand-region))

(use-package magit
  :defer 1
  :ensure t
  :custom
  (magit-status-buffer-switch-function 'switch-to-buffer)
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-x g" . magit)
  (:map magit-status-mode-map ("<backtab>" . magit-section-cycle-diffs)))

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

;; Go lang
(use-package go-mode
  :ensure t
  :config
  (defun user/go-mode-hook ()
    (setq tab-width 8
	  standard-indent 8
	  indent-tabs-mode nil))
  (defalias 'user/go-insert-err-check
    (kmacro "C-e RET i f SPC e r r SPC ! = SPC n i l SPC { RET r e t u r n SPC e r r"))
  :hook
  (go-mode . user/go-mode-hook)
  :bind (:map go-mode-map
	      ("C-c C-e" . user/go-insert-err-check)))

(use-package markdown-mode
  :ensure t
  :config
  (defun user/markdown-anchor-link ()
    "Replaces selected region with span with generated id.
Stores markdown link to it in kill ring."
    (interactive)
    (let* ((contents (buffer-substring-no-properties (region-beginning) (region-end)))
           (kebab-case (replace-regexp-in-string
                        " " "-"
                        (string-trim (replace-regexp-in-string
                                      "[^a-z0-9]+" " "
                                      (downcase contents))))))
      (delete-region (region-beginning) (region-end))
      (insert (format "<span id=\"#%s\">%s</span>" kebab-case contents))
      (kill-new (format "[%s](#%s)" contents kebab-case))
      (message "Link saved to kill ring"))))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package gruber-darker-theme
  :ensure t
  :config
  (load-theme 'gruber-darker t))

(load "term/xterm")

;; Enable 24-bit color for tmux-direct
(defun terminal-init-tmux-direct ()
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))

;; Most tmux setups expose TERM=tmux-256color.
(defun terminal-init-tmux-256color ()
  (xterm-register-default-colors)
  (tty-set-up-initial-frame-faces))
