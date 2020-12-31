
(declare-function helm "ext:helm")
(declare-function helm-build-sync-source "ext:helm")

(defgroup run-command nil "Run an external command from a context-dependent list.
:group 'convenience")

(defcustom run-command-completion-method 'helm
  "Completion framework to use to select a command."
  :type '(choice (const :tag "Helm"
                        helm)))

(defcustom run-command-config nil
  "List of functions that will produce runnable commands."
  :type '(repeat function):group'run-command)

(defun run-command ()
  (interactive)
  (pcase run-command-completion-method
    ('helm
     (helm :buffer "*run-command*"
           :prompt "Command name: "
           :sources (run-command--helm-sources)))))

(defun run-command--helm-sources ()
  (mapcar 'run-command--helm-source-from-config
          run-command-config))

(defun run-command--helm-source-from-config (config-name)
  (let* ((scripts (funcall config-name))
         (candidates (mapcar (lambda (script)
                               (cons (plist-get script :display) script))
                             scripts)))
    (helm-build-sync-source (symbol-name config-name)
      :action 'run-command--helm-action
      :candidates candidates
      :filtered-candidate-transformer '(helm-adaptive-sort))))

(defun run-command--helm-action (script)
  (let* ((command (plist-get script :command))
         (command-name (plist-get script :name))
         (scope-name (plist-get script :scope-name))
         (working-dir (plist-get script :working-dir))
         (compilation-buffer-name-function (lambda (name-of-mode)
                                             (concat "*" command-name "(" scope-name ")"
                                                     "*"))))
    (let ((default-directory working-dir))
      (compile (if helm-current-prefix-arg
                   (read-string "> "
                                (concat command " "))
                 command)))))
