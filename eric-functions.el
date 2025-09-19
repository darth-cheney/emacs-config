(defcustom eg/projects-directory nil "Default directory for all programming projects")
(defcustom eg/simpletalk-repo nil "Default SimpleTalk respository location")


(defun eg/adjust-default-font-size ()
  "Adjust the default font size"
  (interactive)
  (let ((size (read-string "New font height")))
    (set-face-attribute 'default nil :height (string-to-number size))))

(defun eg/open-config-file ()
  "Open my Emacs literate configuration org file"
  (interactive)
  (let ((path (concat user-emacs-directory "configuration.org")))
    (find-file path)))

(global-set-key (kbd "C-c c") 'eg/open-config-file)


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


(defun eg/toggle-nano-theme ()
  "Toggle between light and dark nano themes.
Will update the modeline as needed"
  (interactive)
  (if (not (boundp 'eg/nano-current-theme))
      (defvar eg/nano-current-theme nano-theme--current))
  (cond
   ((equal eg/nano-current-theme 'light) (setq eg/nano-current-theme 'dark))
   ((equal eg/nano-current-theme 'dark) (setq eg/nano-current-theme 'light)))
  (nano-modeline-mode nil)
  (eval `(,(intern (concat "nano-" (symbol-name eg/nano-current-theme)))))
  (nano-modeline-mode 1)
  eg/nano-current-theme)


;; Helper functions for working on weather.gov
(defun eg/goto-or-create-todays-dir (name-prefix)
  "Go to the directory named with the prefix and today's date.
If the directory does not yet exist, create it."
  (let* ((suffix (format-time-string "%-m-%-d-%y"))
         (dir-name (concat name-prefix "-" suffix))
         (dir-path
          (if (and (boundp eg/projects-directory) eg/projects-directory)
              (concat eg/projects-directory "/" dir-name)
            (concat "~/Projects/" dir-name))
          ))
    ;; If the directory does not exist, make it
    (unless (file-directory-p dir-path)
      (make-directory dir-path))
    ;; Change into the directory
    (cd dir-path)
    dir-path))

(defun eg/clone-weather-dot-gov (base-path)
  (let ((dir-path (concat base-path "/weather.gov")))
    (unless (file-directory-p dir-path)
      (progn
        (message base-path)
        (cd base-path)
        (shell-command "git clone git@github-tts:weather-gov/weather.gov")))
    
    (cd dir-path)
    (shell-command-to-string "weathergov-git")))

(defun eg/weather-gov-startup-eshell (dir-path)
  (eshell)
  (with-current-buffer "*eshell*"
    (eshell-return-to-prompt)
    (insert (concat "cd " dir-path))
    (eshell-send-input)
    (eshell-return-to-prompt)
    (insert "nse .nodeenv && npm i")
    (eshell-send-input)
    (eshell-return-to-prompt)))

(defun eg/weather-gov-create-nodeenv (dir-path)
  "Create a node environment if it does not already exist"
  (unless (file-directory-p (concat dir-path "/.nodeenv"))
    (shell-command-to-string
     (concat "nodeenv --prebuilt --node=lts " dir-path "/.nodeenv"))))

(defun eg/init-weather-gov-today ()
  (interactive)
  (let* ((base-dir (eg/goto-or-create-todays-dir "weather-gov"))
         (repo-dir (concat base-dir "/weather.gov")))
    (eg/clone-weather-dot-gov base-dir)
    (eg/weather-gov-create-nodeenv repo-dir)
    (eg/weather-gov-startup-eshell repo-dir)))
