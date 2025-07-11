# Quickstart
An emacs WYSIWYG minor mode for org mode.
    (require 'writer)
    ; (add-hook 'org-mode-hook #'writer-setup) ; Use this to start with writer mode when entering org-mode
    (global-set-key (kbd "C-c w") #'writer-toggle)
