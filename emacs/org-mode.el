(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-font-lock-mode 1)
(setq org-todo-keywords
  '((sequence "TODO"
      "MAYBE"
      "NEXT"
      "WAITING"
      "DELEGATED"
      "|"
      "DONE"
      "DEFERRED"
      "CANCELLED")))
(setq org-tag-alist '(
		      ("@work" . ?w)
		      ("@home" . ?h)
		      ("@read" . ?r)
		      ))
(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

(add-hook 'org-mode-hook 'my/org-mode-hook)

(setq org-agenda-files (list "~/Dropbox/org/personal.org"
			     ))
