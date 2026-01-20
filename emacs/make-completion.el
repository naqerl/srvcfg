;;; make-completion.el --- Run Makefile commands with ease -*- lexical-binding: t;-*-

;; Copyright (C) 2024 SciPunch <scipunch@gmail.com>

;; Author: SciPunch <scipunch@gmail.com>
;; Maintainer: SciPunch <scipunch@gmail.com>
;; URL: https://github.com/scipunch/make-completion
;; Version: 0.1
;; Keywords: project compile make makefile

;; This is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; GNU make is a powerful tool as GNU Emacs compile mode.
;; This package helps to merge them into a powerful interface
;;

;;; Code:

(require 'project)

(defvar make-completion-column-margin 4
  "Amount of spaces during `completing-read' between Makefile target info.")

;;;###autoload
(defvar make-completion-compilation-buffer-name "*compile*"
  "Name of the last make compilation command.")

(defvar make-completion--target-column-width)
(defvar make-completion--prerequisites-column-width)

;;;###autoload
(defun make (prefix)
  "Interactively select a Makefile target from the project and run it.

With PREFIX argument (e.g. \[universal-argument]), prefill the compile
command instead of running it immediately."
  (interactive "P")
  (let* ((project (project-current))
         (default-directory (project-root project))
         (makefiles (make-completion--find-makefiles))
         (targets-alist
          (apply #'append
                 (mapcar (lambda (makefile)
                           (let ((dir (file-name-directory makefile)))
                             (mapcar (lambda (target)
                                       (cons (make-completion--format-make-target makefile target)
                                             (cons dir target)))
                                     (my-make-targets dir))))
                         makefiles)))
         (selected (completing-read (concat "[" (project-name project) "] Makefile target: ") targets-alist)))
    (let* ((dir-target (alist-get selected targets-alist nil nil #'string=))
           (dir (car dir-target))
           (target (cdr dir-target))
           (command (format "make -C %s %s"
                            (file-relative-name dir (project-root project))
                            target))
           (compile-command command))
      (if prefix
          (call-interactively 'compile)
        (compile command)))))

(defun my-make-targets (dir)
  "Extract make targets from Makefile in DIR."
  (let ((makefile (expand-file-name "Makefile" dir)))
    (when (file-exists-p makefile)
      (with-temp-buffer
        (insert-file-contents makefile)
        (let (targets)
          (goto-char (point-min))
          (while (re-search-forward "^\\([a-zA-Z\\-0-9]*\\):" nil t)
            (push (match-string 1) targets))
          (nreverse targets))))))

(defun make-completion--find-makefiles ()
  "Finds all Makefiles starting from default directory."
  (directory-files-recursively
   default-directory "^Makefile$"
   nil (lambda (x) (not (string-match-p "/\\." x)))))

(defun make-completion--format-make-target (makefile target)
  "Returns user friendly TARGET representation from MAKEFILE."
  (concat
   (or (file-name-directory (file-relative-name makefile default-directory)) "./")
   target))

(defvar my-make-targets-cache nil)

(provide 'make-completion)
;;; make-completion.el ends here
