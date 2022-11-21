(defcustom eg/projects-directory nil "Default directory for all programming projects")
(defcustom eg/simpletalk-repo nil "Default SimpleTalk respository location")

(defun eg/get-project-dirs-alist (dir-name repo-name)
  (let* ((result-list '()))
    (add-to-list
     'result-list
     (cons "root-path"
           (concat
            (file-name-as-directory eg/projects-directory)
            (file-name-as-directory dir-name)
            )))

    (add-to-list
     'result-list
     (cons
      "nodeenv-path"
      (concat
       (file-name-as-directory eg/projects-directory)
       (file-name-as-directory dir-name)
       (file-name-as-directory repo-name)
       "nodeenv")))

    (add-to-list
     'result-list
     (cons
      "repo-path"
      (concat
       (file-name-as-directory eg/projects-directory)
       (file-name-as-directory dir-name)
       repo-name)))
    ))


(defun eg/workon-simpletalk-today ()
  "Create or reconfigure a Simpletalk project
setup for today's month and day combination"
  (interactive)
  (setq eg/projects-directory "~/projects")
  (setq eg/simpletalk-repo "github.com:dkrasner/Simpletalk")
  (setq eg/simpletalk-node-version "14.18.1")
  (let* (
         (dir-paths (eg/get-project-dirs-alist
                     (concat "simpletalk-" (format-time-string "%m-%d-%y"))
                     "SimpleTalk"))
         (root-path (cdr (assoc "root-path" dir-paths)))
         (nodeenv-path (cdr (assoc "nodeenv-path" dir-paths)))
         (repo-path (cdr (assoc "repo-path" dir-paths)))
         (command-list '()))

    ;; If the project root directory doesn't exist
    ;; then create it
    (if
        (not (file-directory-p root-path))
        (make-directory root-path))

    ;; Attempt to clone the repo
    (if
        (not (file-directory-p repo-path))
        (add-to-list
         'command-list
         (format "git clone git@%s %s" eg/simpletalk-repo repo-path)))

    ;; If the nodeenv directory does not
    ;; exist, add the command for making it
    (if
        (not (file-directory-p nodeenv-path))
        (add-to-list
         'command-list
         (format
          "nodeenv --prebuilt --node=%s %s" eg/simpletalk-node-version nodeenv-path)))
    
    ;; Attempt to source into the nodeenv
    ;; and install any dependencies
    (add-to-list
     'command-list
     (concat
      "source "
      (file-name-as-directory nodeenv-path)
      (file-name-as-directory "bin")
      "activate"))

    (add-to-list
     'command-list
     (format "cd %s" repo-path))

    (add-to-list
     'command-list
     "npm install")


    ;; 
    ;; Join the list as shell commands
    ;; and run them as shell commands
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (split-window-below)
    (other-window 1)
    (ansi-term "/bin/bash")
    (comint-send-string
     (buffer-name (current-buffer))
     (concat
      (string-join (reverse command-list) " && ")
      "\r"))
    (other-window 1)
    (sleep-for 1)
    (dired repo-path)
    (other-window 2)))


(setq eg/idp-projectile-contents
      (string-join
       '("-/node_modules/"
         "-/nodeenv/"
         "-/public/acuant/"
         "-/public/packs/"
         "-/app/assets/builds/"
         "-/tmp/")
       "\n"))

(defun eg/get-local-ip ()
  "Return the string representation of the local ip address"
  (let* ((get-ip-command "ipconfig getifaddr en0")
         (command-result (shell-command-to-string get-ip-command)))
    (string-trim command-result)))



(setq eg/idp-application-yml (string-replace "<your-local-ip>" (eg/get-local-ip) "development:
  config_key:
  #domain_name: <your-local-ip>:3000
  #mailer_domain_name: <your-local-ip>:3000"))
(setq eg/idp-dir-locals
      (prin1-to-string
       '((js2-mode . ((js2-basic-offset . 2)))
         (typescriptreact-mode . ((typescript-indent-level . 2))))))

(defun eg/setup-idp ()
  "Setup the config/application.yml and projectile files.
To be used on a fresh clone of the idp repo"
  (interactive)
  (if (and (projectile-project-root) (string-equal (projectile-project-name) "identity-idp"))
      (let ((projectile-filename (concat (projectile-project-root) ".projectile"))
            (application-yml-filename (concat (projectile-project-root) "config/application.yml"))
            (dir-locals-filename (concat (projectile-project-root) ".dir-locals.el")))
        (with-temp-file
            projectile-filename
          (insert eg/idp-projectile-contents))
        (with-temp-file application-yml-filename
          (insert eg/idp-application-yml))
        (with-temp-file dir-locals-filename
          (insert eg/idp-dir-locals)))))
(defun eg/idp-projectile-after-hook ()
  (if
      (string-match-p (regexp-quote "identity-idp") (projectile-project-root))
      (progn
        (message "==IDP PROJECTILE HOOK LOADED=="))))

(add-hook 'projectile-after-switch-project-hook #'eg/idp-projectile-after-hook)


(defun eg/idp-enable-https ()
  "Set the application.yml file for local development"
  (interactive)
  (let ((application-yml-filename (concat (projectile-project-root) "config/application.yml"))
        )
    (with-temp-buffer
    (find-file application-yml-filename)
    (goto-char (point-min))
    (perform-replace "  #domain_name:" "  domain_name:" nil nil nil)
    (perform-replace "  #mailer_domain_name:" "  mailer_domain_name:" nil nil nil)
    (save-buffer)
    (kill-buffer)
    )))

(defun eg/idp-disable-https ()
  "Set the application.yml file for local development"
  (interactive)
  (let ((application-yml-filename (concat (projectile-project-root) "config/application.yml"))
        )
    (with-temp-buffer
    (find-file application-yml-filename)
    (goto-char (point-min))
    (perform-replace "  domain_name:" "  #domain_name:" nil nil nil)
    (perform-replace "  mailer_domain_name:" "  #mailer_domain_name:" nil nil nil)
    (save-buffer)
    (kill-buffer)
    )))



(defun eg/idp-mocha-this-file ()
  "Run Mocha on the current test file.
and open in a compilation buffer"
  (interactive)
  (compile
   (concat "cd " (projectile-project-root) " && npx mocha " (eg/project-filename))
   t))
(global-set-key (kbd "C-c t") #'eg/idp-mocha-this-file)
