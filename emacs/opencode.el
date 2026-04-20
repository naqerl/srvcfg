(require 'auth-source)
(require 'cl-lib)

(defun user/opencode-get-usage ()
  "Fetch and parse usage limits from OpenCode workspace.
Returns an alist with 'rolling, 'weekly, and 'monthly usage percentages."
  (interactive)
  (let* ((html (user/opencode--get-workspace-go-html))
         (usage (user/opencode--extract-usage html)))
    (when (called-interactively-p 'interactive)
      (if usage
          (message "OpenCode Usage: 5h=%s, Week=%s, Month=%s"
                   (or (alist-get 'rolling usage) "N/A")
                   (or (alist-get 'weekly usage) "N/A")
                   (or (alist-get 'monthly usage) "N/A"))
        (message "Failed to fetch usage from OpenCode")))
    usage))

(defun user/opencode--get-workspace-go-html ()
  "Return HTML page for the workspace using credentials from auth-source."
  (let ((creds (user/opencode--get-credentials)))
    (unless creds
      (error "OpenCode credentials not found in auth-source. Please add to ~/.authinfo.gpg: machine opencode.ai login workspace password WRK... and machine opencode.ai login auth password Fe26..."))
    (let* ((workspace (plist-get creds :workspace))
           (auth (plist-get creds :auth))
           (url (format "https://opencode.ai/workspace/%s/go" workspace))
           (cookie (format "auth=%s; oc_locale=en" auth))
           (cmd (concat "curl "
                        (format "'%s' " url)
                        "--compressed -sL "  ; -s silent, -L follow redirects
                        (mapconcat (lambda (h) (format "-H '%s: %s'" (car h) (cdr h)))
                                   `(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64; rv:149.0) Gecko/20100101 Firefox/149.0")
                                     ("Accept" . "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8")
                                     ("Accept-Language" . "en-US,en;q=0.5")
                                     ("Accept-Encoding" . "gzip, deflate, br")
                                     ("Referer" . "https://opencode.ai/")
                                     ("Connection" . "keep-alive")
                                     ("Cookie" . ,cookie)
                                     ("Upgrade-Insecure-Requests" . "1"))
                                   " "))))
      (let ((output (shell-command-to-string cmd)))
        output))))

(defun user/opencode--extract-usage (html)
  "Extract usage limits from HTML using libxml and return as alist."
  (with-temp-buffer
    (insert html)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (result '())
           (current-label nil))
      (cl-labels
          ((parse-node
             (node)
             (when (listp node)
               (let ((tag (car node))
                     (attrs (cadr node))
                     (children (cddr node)))
                 (when (eq tag 'span)
                   (let ((slot (cdr (assoc 'data-slot attrs))))
                     (when (string= slot "usage-label")
                       (setq current-label (car children)))
                     (when (string= slot "usage-value")
                       (when current-label
                         (let ((value
                                (cl-find-if
                                 (lambda (x)
                                   (and (stringp x)
                                        (string-match-p "^[0-9]+$" x)))
                                 children)))
                           (when value
                             (when (string= current-label "Rolling Usage")
                               (push (cons 'rolling (concat value "%")) result))
                             (when (string= current-label "Weekly Usage")
                               (push (cons 'weekly (concat value "%")) result))
                             (when (string= current-label "Monthly Usage")
                               (push (cons 'monthly (concat value "%")) result))))
                         (setq current-label nil)))))
                 (dolist (child children)
                   (parse-node child))))))
        (parse-node dom))
      result)))

(defun user/opencode--get-credentials ()
  "Retrieve OpenCode credentials from auth-source.
Returns plist with :workspace and :auth, or nil if not found."
  (let* ((workspace-entry (car (auth-source-search
                                :machine "opencode.ai"
                                :user "workspace"
                                :require '(:secret))))
         (auth-entry (car (auth-source-search
                           :machine "opencode.ai"
                           :user "auth"
                           :require '(:secret)))))
    (when (and workspace-entry auth-entry)
      (let ((workspace (plist-get workspace-entry :secret))
            (auth (plist-get auth-entry :secret)))
        (list :workspace (if (functionp workspace) (funcall workspace) workspace)
              :auth (if (functionp auth) (funcall auth) auth))))))

(provide 'opencode)
