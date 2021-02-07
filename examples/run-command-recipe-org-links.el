
;; This example demonstrates the use of Elisp for the :command-line argument of
;; a run-command recipe.
;; 
;; Specifically, this recipe creates a temporary org buffer and insert some
;; headings and some links in it and prompts the user to select one org heading
;; and then open all links under that heading:

(require 'cl-lib)

(defun run-command-recipe-org-links ()
  "Prompt the user to select a heading and then open all org links between that
heading and the next one. If there is no next heading then the search for org
links will extend to the end of the buffer."
  (list
   ;; only enable this command in an org buffer:
   (when (derived-mode-p 'org-mode)
     (list :command-name "open heading's link"
           :command-line
           (lambda ()
             ;; select a heading:
             (let ((heading
                    (completing-read "Select heading: "
                                     (save-excursion
                                       ;; begin at the beginning of the buffer
                                       (goto-char (point-min))
                                       ;; go to the first heading (if point has
                                       ;; not yet been already at one):
                                       (or (outline-on-heading-p t)
                                           (outline-next-heading))
                                       (cl-loop
                                        while (and
                                               ;; keep looping until reaching the end of buffer:
                                               (not (eq (point) (point-max)))
                                               ;; and do the following once reaching a heading:
                                               (outline-on-heading-p t))
                                        ;; collect the heading at point:
                                        collect (prog1 (buffer-substring-no-properties
                                                        (line-beginning-position)
                                                        (line-end-position))
                                                  ;; update cursor position:
                                                  (outline-next-heading)))))))
               ;; now collect the org links:
               (save-excursion
                 ;; search for the heading from the beginning of the buffer
                 (goto-char (point-min))
                 ;; set the boundaries for the search of org links (from the
                 ;; selected heading to the next heading, or if there is no next
                 ;; heading then search until the end of the buffer):
                 (let* ((heading-start (re-search-forward (format "^%s" (regexp-quote heading))))
                        (heading-end (progn (outline-next-heading)
                                            (point)))
                        (last-point (1- heading-start)) ; used for tracking the
                                                        ; last link in this
                                                        ; heading
                        )
                   ;; now search for each org link under the heading, begin at the heading:
                   (goto-char heading-start)
                   (while (and (org-next-link)
                               (< (point) heading-end)
                               (not (eq (point) last-point)))
                     (org-open-at-point)
                     (setq last-point (point)))))))))))

