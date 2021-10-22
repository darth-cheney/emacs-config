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
