;; -*- lexical-binding: t -*-
;; Custom Dashboard with System Information

(defgroup user-dashboard nil
  "Custom dashboard with system info."
  :group 'emacs)

(defcustom user-dashboard-refresh-interval 5
  "Seconds between automatic refreshes."
  :type 'integer
  :group 'user-dashboard)

(defvar user-dashboard-buffer-name "*dashboard*"
  "Name of the dashboard buffer.")

(defvar user-dashboard-refresh-timer nil
  "Timer for automatic dashboard refresh.")

;; ============================================================
;; System Info Functions
;; ============================================================

(defun user-dashboard--shell-output (command)
  "Execute shell COMMAND and return trimmed output."
  (string-trim
   (shell-command-to-string command)))

(defun user-dashboard--system-load ()
  "Get system load average."
  (user-dashboard--shell-output "uptime | awk -F'load average:' '{print \$2}' | tr -d ','"))

(defun user-dashboard--cpu-usage ()
  "Get current CPU usage percentage."
  (condition-case nil
      (user-dashboard--shell-output
       "top -bn1 | grep 'Cpu(s)' | awk '{print \$2}' | cut -d'%' -f1")
    (error "N/A")))

(defun user-dashboard--memory-info ()
  "Get memory usage in human readable format."
  (user-dashboard--shell-output
   "free -h | awk '/^Mem:/ {print \$3 \"/\" \$2 \" (\" int(\$3/\$2*100) \"%)\"}'"))

(defun user-dashboard--disk-info ()
  "Get disk usage for root partition."
  (user-dashboard--shell-output
   "df -h / | awk 'NR==2 {print \$3 \"/\" \$2 \" (\" \$5 \")\"}'"))

(defun user-dashboard--uptime ()
  "Get system uptime."
  (user-dashboard--shell-output "uptime -p | sed 's/up //'"))

(defun user-dashboard--hostname ()
  "Get system hostname."
  (user-dashboard--shell-output "hostname"))

(defun user-dashboard--kernel ()
  "Get kernel version."
  (user-dashboard--shell-output "uname -r"))

(defun user-dashboard--public-ip ()
  "Get public IP address."
  (condition-case nil
      (user-dashboard--shell-output "curl -s ifconfig.me")
    (error "N/A")))

;; ============================================================
;; Dashboard Rendering
;; ============================================================

(defvar user-dashboard-widgets
  '(
    ;; System section
    (:type :heading
     :text "System Information"
     :face (:weight bold :foreground "#e5c07b"))

    (:type :metric
     :label "Hostname"
     :value user-dashboard--hostname)

    (:type :metric
     :label "Kernel"
     :value user-dashboard--kernel)

    (:type :metric
     :label "Uptime"
     :value user-dashboard--uptime)

    (:type :metric
     :label "Load Average"
     :value user-dashboard--system-load)

    (:type :metric
     :label "CPU Usage"
     :value user-dashboard--cpu-usage)

    (:type :spacer)

    ;; Resources section
    (:type :heading
     :text "Resources"
     :face (:weight bold :foreground "#e5c07b"))

    (:type :metric
     :label "Memory"
     :value user-dashboard--memory-info)

    (:type :metric
     :label "Disk (/ )"
     :value user-dashboard--disk-info)

    (:type :spacer)

    ;; Network section
    (:type :heading
     :text "Network"
     :face (:weight bold :foreground "#e5c07b"))

    (:type :metric
     :label "Public IP"
     :value user-dashboard--public-ip)

    (:type :spacer)

    ;; Emacs section
    (:type :heading
     :text "Emacs"
     :face (:weight bold :foreground "#e5c07b"))

    (:type :metric
     :label "Version"
     :value (lambda () emacs-version))

    (:type :metric
     :label "Packages"
     :value (lambda () (format "%d" (length package-alist))))

    (:type :metric
     :label "Init Time"
     :value (lambda () (format "%.3fs" (float-time after-init-time))))
    )
  "List of dashboard widgets.")

(defun user-dashboard--insert-centered (text face)
  "Insert TEXT with FACE centered on line."
  (let* ((width (frame-width))
         (text-width (length text))
         (padding (max 0 (/ (- width text-width) 2))))
    (insert (propertize (make-string padding ? ) 'face 'default))
    (insert (propertize text 'face face))
    (insert "\n")))

(defun user-dashboard--render-widget (widget)
  "Render a single WIDGET."
  (let ((type (plist-get widget :type)))
    (pcase type
      (:title
       (insert "\n")
       (user-dashboard--insert-centered
        (plist-get widget :text)
        (plist-get widget :face))
       (insert "\n"))

      (:heading
       (insert "\n")
       (insert (propertize (concat "  " (plist-get widget :text))
                          'face (plist-get widget :face)))
       (insert "\n")
       (insert (propertize (make-string (- (frame-width) 1) ?─)
                          'face '(:foreground "#5c6370")))
       (insert "\n"))

      (:text
       (let ((text (if (functionp (plist-get widget :text))
                      (funcall (plist-get widget :text))
                    (plist-get widget :text))))
         (user-dashboard--insert-centered text (plist-get widget :face))
         (insert "\n")))

      (:metric
       (let* ((label (plist-get widget :label))
              (value-fn (plist-get widget :value))
              (value (if (functionp value-fn) (funcall value-fn) "N/A"))
              (padding (max 0 (- 20 (length label)))))
         (insert "  ")
         (insert (propertize label 'face '(:weight bold)))
         (insert (make-string padding ? ))
         (insert "  ")
         (insert (propertize value 'face '(:foreground "#abb2bf")))
         (insert "\n")))

      (:button
       (let* ((text (plist-get widget :text))
              (action (plist-get widget :action))
              (face (plist-get widget :face)))
         (insert "  [ ")
         (insert-text-button
          text
          'action action
          'face face
          'follow-link t
          'help-echo (format "Click to: %s" text))
         (insert " ]\n")))

      (:spacer
       (insert "\n"))

      (_
       (message "Unknown widget type: %s" type)))))

;; ============================================================
;; Main Dashboard Functions
;; ============================================================

(defun user-dashboard-refresh ()
  "Refresh the dashboard buffer."
  (when (get-buffer user-dashboard-buffer-name)
    (with-current-buffer user-dashboard-buffer-name
      (let ((inhibit-read-only t)
            (point (point)))
        (erase-buffer)
        (dolist (widget user-dashboard-widgets)
          (user-dashboard--render-widget widget))
        (goto-char (min point (point-max)))
        (setq mode-line-format nil)))))

(defun user-dashboard ()
  "Open or switch to the dashboard."
  (interactive)
  (let ((buffer (get-buffer-create user-dashboard-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'special-mode)
        (special-mode))
      (user-dashboard-refresh))
    (switch-to-buffer buffer)))

(defun user-dashboard-start-auto-refresh ()
  "Start automatic dashboard refresh timer."
  (when user-dashboard-refresh-timer
    (cancel-timer user-dashboard-refresh-timer))
  (setq user-dashboard-refresh-timer
        (run-with-timer user-dashboard-refresh-interval
                       user-dashboard-refresh-interval
                       #'user-dashboard-refresh)))

(defun user-dashboard-stop-auto-refresh ()
  "Stop automatic dashboard refresh timer."
  (when user-dashboard-refresh-timer
    (cancel-timer user-dashboard-refresh-timer)
    (setq user-dashboard-refresh-timer nil)))

;; ============================================================
;; Startup Hook
;; ============================================================

(defun user-dashboard-startup ()
  "Display dashboard on startup (unless files were specified)."
  (unless (or (cl-some (lambda (arg)
                         (or (string-prefix-p "-" arg)
                             (file-exists-p arg)))
                       command-line-args-left)
              (get-buffer "*scratch*"))
    (user-dashboard)
    (user-dashboard-start-auto-refresh)))

;; Hook into startup
(add-hook 'emacs-startup-hook #'user-dashboard-startup)

;; Refresh when switching to dashboard
(add-hook 'window-configuration-change-hook
          (lambda ()
            (when (string= (buffer-name) user-dashboard-buffer-name)
              (user-dashboard-refresh))))

(provide 'user-dashboard)
