# Quickstart
An emacs WYSIWYG minor mode for org mode.

    (require 'writer)
    (add-hook 'org-mode-hook
	      (lambda ()
	        (define-key org-mode-map (kbd "C-c w") #'writer-mode)))
