;; -*- lexical-binding: t; -*-
(setq-default make-backup-files nil
      create-lockfiles nil
      auto-save-default nil
      help-window-select t
      history-length 25
      use-dialog-box nil
      electric-indent-inhibit t
      backward-delete-char-untabify-method 'hungry
      indent-tabs-mode nil
      custom-file (expand-file-name ".emacs.custom.el" user-emacs-directory)
      remote-file-name-inhibit-locks t
      remote-file-name-inhibit-auto-save-visited t
      tramp-use-scp-direct-remote-copying t
      split-width-threshold 1 ;; Prever side by side splits
      compile-command "")

(put 'upcase-region 'disabled nil)

(blink-cursor-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(winner-mode 1)
(menu-bar-mode -1)

;; Preserve cursor position when reverting buffers
(defun user/revert-buffer-preserve-point (&rest _)
  "Preserve cursor position after revert-buffer."
  (let ((point (point))
        (window-start (window-start)))
    (run-at-time 0 nil
                 (lambda ()
                   (goto-char point)
                   (set-window-start (selected-window) window-start)))))
(advice-add 'revert-buffer :after #'user/revert-buffer-preserve-point)

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
  :config
  (dolist (regex '((biome-lint "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s.*\s━+$" 1 2 3 2 1)
                   (tsc "^\\(.*\\):\\([0-9]+\\):\\([0-9]+\\)\s-\serror\s.*$" 1 2 3 2 1)
                   (ruff "^ *--> \\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\)$" 1 2 3)))
  (add-to-list 'compilation-error-regexp-alist-alist regex)
  (add-to-list 'compilation-error-regexp-alist-alist (car regex))))

;; Noisy and seem to add latency
(use-package eldoc
  :config
  (global-eldoc-mode -1))

(use-package ls-lisp ;; Sort directories first in dired
  :custom
  (ls-lisp-dirs-first t)
  (ls-lisp-use-insert-directory-program nil))

(use-package dired
  :custom
  (dired-dwim-target t)
  (dired-kill-when-opening-new-dired-buffer t)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :hook
  (dired-mode . (lambda () (dired-hide-details-mode 1))))

(use-package whitespace
  :config
  (add-hook 'before-save-hook  'whitespace-cleanup))

;; Custom built-in binds
(use-package emacs
  :bind
  ("C-c d" . duplicate-line)
  ("C-," . previous-buffer)
  ("C-." . next-buffer))

(use-package org
  :config
  (defun user/org-capture-tasks-note()
    "Runs org-capture with agent task template."
    (interactive)
    (if (project-current)
        (org-capture nil "a")
      (message "project-current is not set")))
  :bind
  ("C-x C-a" . user/org-capture-tasks-note)
  :custom
  (org-capture-templates
   '(("a" "Agent Task" entry
      (file+headline
       (lambda ()
         (expand-file-name ".tasks/notes.org" (project-root (project-current))))
       "Tasks")
      "* TODO %?\n  %u\n  %a"))))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(require 'package)
(package-initialize)
(require 'use-package)

(use-package diminish :ensure t)

(use-package f :ensure t)

(use-package keycast
  :ensure t
  :commands keycast-mode-line-mode)

(use-package sudo-edit
  :ensure t
  :commands sudo-edit)

(use-package golden-ratio
  :ensure t
  :diminish 'golden-ratio-mode
  :config
  (golden-ratio-mode 1))

(use-package change-inner
  :ensure t
  :bind
  ("M-i" . change-inner)
  ("M-o" . change-outer))

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

;; Start region -- Major modes

(use-package go-mode
  :ensure t
  :config
  (defun user/go-mode-hook ()
    (setq tab-width 8
          standard-indent 8
          indent-tabs-mode t))
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
Stores markdown link to it in the kill ring."
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

(use-package web-mode
  :ensure t
  :config
  (defun user/web-mode-hook ()
    (setq web-mode-markup-indent-offset 2))
  (add-hook 'web-mode-hook 'user/web-mode-hook)
  :hook
  (html-mode . web-mode))

;; End region -- Major modes

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t))

(define-derived-mode yao-mode comint-mode "YAO"
  "Major mode for YAO shell."
  (setq-local comint-prompt-regexp "^λ "))

(defun yao ()
  "Toggle YAO shell with comint-mode.
If region is active, inserts snippet at cursor position."
  (interactive)
  (let* ((default-directory (or (when-let ((p (project-current)))
                                  (project-root p))
                                default-directory
                                user-emacs-directory))
         (project-name (file-name-nondirectory
                        (directory-file-name default-directory)))
         (buffer-name (format "*%s-yao*" project-name))
         (yao-binary "yao")
         (snippet (when (use-region-p)
                    (prog1 (format "\n%s:%s-%s\n```\n%s```"
                                   (buffer-file-name)
                                   (line-number-at-pos (region-beginning))
                                   (line-number-at-pos (region-end))
                                   (buffer-substring (region-beginning) (region-end)))
                      (deactivate-mark)))))
    (message "[yao] opening yao shell at %s" default-directory)
    (unless (comint-check-proc buffer-name)
      (make-comint-in-buffer "yao" buffer-name yao-binary)
      (with-current-buffer buffer-name
        (yao-mode)))
    (pop-to-buffer buffer-name)
    (when snippet
      (goto-char (process-mark (get-buffer-process buffer-name)))
      (save-excursion
        (insert snippet)))))

(defun ntfy ()
  "Send latest kill ring entry via curl POST."
  (interactive)
  (let* ((payload (current-kill 0 t))  ;; get latest kill
         (url "https://ntfy.naqerl.com/clip")
         (cmd (format "curl -s -d %s -L %s"
                      (shell-quote-argument payload)
                      url))
         (response (shell-command-to-string cmd)))
    (message "Sent kill-ring content to %s" url)))
