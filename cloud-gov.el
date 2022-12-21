(defun cloud-gov--check-login-status ()
  (let ((target-string "Not logged in"))
    (if (not (string-match-p (regexp-quote target-string)
                        (shell-command-to-string "cf api")))
        t
      nil)))

(defun cloud-gov-home--insert (text)
  (let ((inhibit-read-only t))
    (insert text)))

(defun cloud-placeholder ()
  (interactive)
  (message "placeholder"))

(transient-define-prefix cloud-gov-home-transient ()
  "Cloud.gov CLI Commands"
  ["Space management"
   ("o" "orgs" cloud-placeholder)
   ("u" "org-users" cloud-placeholder)
   ("S" "set-org-role" cloud-placeholder)
   ("U" "unset-org-role" cloud-placeholder)]
  ["Apps management"
   ("a" "list apps" cloud-gov-list-apps)])

(defun cloud-gov-login-sso ()
  (interactive)
  (let ((passcode (read-passwd "Enter SSO passcode:  ")))
    (shell-command (format "cf login -a https://api.fr.cloud.gov --sso-passcode %s" passcode))))

(define-derived-mode cloud-gov-home-mode special-mode "Cloud.gov"
  "Cloud.gov Home Mode"  
  (let ((inhibit-read-only t)) (erase-buffer))
  (cloud-gov-home--insert "Cloud.gov Tools")
  (display-line-numbers-mode -1)

  (let ((is-logged-in (cloud-gov--check-login-status))
        )

    (cond
     (is-logged-in (cloud-gov-home-transient))
     ((progn
        (cloud-gov-home--insert "\nYou are not logged in!")
        (browse-url "https://login.fr.cloud.gov/passcode")
        (cloud-gov-login-sso))))))

(defun cloud-gov-home ()
  (interactive)
  (switch-to-buffer "*Cloud.gov*")
  (cloud-gov-home-mode))

(defun cloud-gov--parse-process-output (str)
  (mapcar (lambda (inner)
            (split-string inner " [ ]+"))
          (-slice (split-string (string-trim str) "\n") 3)))

(defun cloud-gov--list-to-tabulated-headers (a-list)
  (let ((width (/ 100 (length a-list))))
    (vconcat (mapcar (lambda (name)
                       (list name width))
                     a-list))))

(defun cloud-gov--list-to-tabulated-items (a-list)
  (mapcar (lambda (values)
            (list nil (vconcat values)))
          a-list))


(defun cloud-gov-list-apps ()
  (interactive)
  (with-current-buffer (get-buffer-create "*Cloud.gov[apps]*")
    (let ((sentinel (lambda (proc str)
                      (with-current-buffer (process-buffer proc)
                        (let* ((output (buffer-string))
                               (parsed-output (cloud-gov--parse-process-output output))
                               (headers (cloud-gov--list-to-tabulated-headers (car parsed-output)))
                               (rows (cloud-gov--list-to-tabulated-items (cdr parsed-output))))
                          (erase-buffer)
                          (tabulated-list-mode)
                          (setq tabulated-list-format headers)
                          (setq tabulated-list-entries rows)
                          (tabulated-list-init-header)
                          (tabulated-list-print))))))
      (make-process
       :name "cloud-gov-apps"
       :buffer (current-buffer)
       :command '("cf" "apps")
       :sentinel sentinel)
      (switch-to-buffer (current-buffer)))))
