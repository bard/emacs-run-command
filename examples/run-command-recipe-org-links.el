;; This example demonstrates the feature of executing an Elisp lambda in the
;; :command-line argument of a run-command recipe.

(defun run-command-recipe-org-links ()
  "Prompt the user to select a heading and then open all org links in the
section the cursor is in (in other words, in the region delimited by point at
`org-back-to-heading-or-point-min' and point `outline-next-heading')."
  (list
   ;; only enable this command in an org buffer:
   (when (derived-mode-p 'org-mode)
     (list :command-name "open"
           :display "Open links from current section"
           :command-line
           (lambda ()
             ;; the cursor will move to each link in order to operate on them,
             ;; so `save-excursion' is used below to preserve the original
             ;; cursor position in the org buffer:
             (save-excursion
               (let* ((search-start ; the start position to search for links
                               (if (outline-on-heading-p)
                                   (line-end-position)
                                 (org-back-to-heading-or-point-min t)))
                      (search-end ; the end position to search for links
                               (or (outline-next-heading)
                                   (point-max)))
                      (last-point  ; used for tracking the position of the most
                                   ; recent link
                               (1- search-start)))
                 ;; get back to the beginning of the search
                 (goto-char search-start)
                 ;; now search for each org link and open it:
                 (while (and (org-next-link)
                             (< (point) search-end)
                             (not (eq (point) last-point)))
                   (org-open-at-point)
                   (setq last-point (point))))))))))

