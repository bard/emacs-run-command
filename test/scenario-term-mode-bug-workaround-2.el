(load-file "setup.el")

;; To recreate script and timing file:
;;
;;   git clone https://github.com/bard/starter-node-basic
;;   cd starter-node-basic
;;   script -c yarn -T timingfile script

(defun test-workaround ()
  (interactive)
  (switch-to-buffer
   (run-command-term--run-with-patched-emulator
    "test"
    "scriptreplay -d 30 -t scenario-term-mode-bug-workaround-2-fixture.timingfile scenario-term-mode-bug-workaround-2-fixture.script")))

;; A sign of the bug is a big quantity of spurious newlines after the
;; actual command output and before "Process test finish", thus we
;; check for absence of those newline. This is more robust than, say,
;; checking for number of lines, as that changes depending on the
;; window setup.

(defun command-output-correct-p ()
  (equal (buffer-substring-no-properties (- (point-max) 41) (point-max))
         " Done in 10.11s.


Process test finished
"))

(director-run
 :version 1
 :before-start
 (lambda ()
   (require 'term)
   (require 'run-command)
   )
 :steps
 '((:call test-workaround)
   (:wait 1)
   (:assert (with-current-buffer "*test*"
              (command-output-correct-p))))
 :log-target '(file . "director.log")
 :delay-between-steps 0.01
 :on-error (lambda () (kill-emacs 1))
 :on-failure (lambda () (kill-emacs 1))
 :after-end (lambda () (kill-emacs)))


