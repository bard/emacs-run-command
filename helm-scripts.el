(require 'helm-scripts-util)
(require 'helm-scripts-source-package-json)
(require 'helm-scripts-source-make)
(require 'helm-scripts-source-hugo)
(require 'helm-scripts-source-global)

(defvar helm-script-config (list 'helm-scripts-source-package-json 'helm-scripts-source-hugo
                                 'helm-scripts-source-make 'helm-scripts-source-global))

(defun helm-scripts-sources ()
  (mapcar 'funcall helm-script-config))

(defun mmr/helm-scripts ()
  (interactive)
  (helm :buffer "*helm scripts*"
        :prompt "Script name: "
        :sources (helm-scripts-sources)))

