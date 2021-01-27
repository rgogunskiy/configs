(require 'org)
;; (set-face-attribute 'org-level-1 nil :height 1.0 :background nil)
(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-startup-indented t)
;; (setq org-hide-leading-stars t)
(setq org-odd-level-only nil) 
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)

(setq org-log-done t)

(setq org-default-notes-file (concat org-directory "~/Dropbox/org/inbox.org"))
(setq org-agenda-files (list "~/Dropbox/org/work.org"
                             "~/Dropbox/org/inbox.org"
			     "~/Dropbox/org/personal.org"
			     "~/Dropbox/org/research.org"
			     "~/Dropbox/org/refile.org"
			     ))

;; (setq org-refile-targets '((nil :maxlevel . 2)))
(setq org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
(setq org-agenda-custom-commands 
      '(("o" "At the work" tags-todo "@work"
         ((org-agenda-overriding-header "Work")
          (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
	("i" "Inbox" tags-todo "@inbox"
	 ((org-agenda-overriding-header "Inbox")))
	("r" "Research" tags-todo "@research"
	 ((org-agenda-overriding-header "Research")))
	("m" "Refile" tags-todo "REFILE"
	 ((org-agenda-overriding-header "Refile")))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))
		  
(defun org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))


(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/org/refile.org" "Tasks")
         "* TODO %?")
	))
