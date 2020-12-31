(require 'helm-scripts-source-package-json)
(require 'helm-scripts-source-make)
(require 'helm-scripts-source-hugo)
(require 'helm-scripts-source-global)

(defvar helm-script-config (list 'helm-scripts-source-package-json 'helm-scripts-source-hugo
                                 'helm-scripts-source-make 'helm-scripts-source-global))

(defun helm-scripts-sources (project-dir)
  (list (funcall 'helm-scripts-source-package-json
                 project-dir)
        (funcall 'helm-scripts-source-hugo project-dir)
        (funcall 'helm-scripts-source-make project-dir)
        (funcall 'helm-scripts-source-global project-dir)))

(defun mmr/helm-scripts ()
  (interactive)
  (let ((project-dir (mmr/helm-scripts~get-project-dir)))
    (helm :buffer "*helm scripts*"
          :prompt "Script name: "
          :sources (helm-scripts-sources project-dir))))

(defun mmr/helm-scripts~get-project-dir ()
  (locate-dominating-file default-directory
                          (lambda (dir)
                            (file-exists-p (concat dir ".git")))))

