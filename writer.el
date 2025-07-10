;;; writer.el --- Prettify text in org mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Misciagno

;; Author: John Misciagno <johnmisciagno@gmail.com>
;; Keywords: faces
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'org-bullets)
(require 'org-variable-pitch)

(defun writer-setup ()
  (setq org-hide-emphasis-markers t)
  (org-indent-mode 1)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-bullets-mode 1)
  (org-variable-pitch-minor-mode)
  (text-scale-set 1)
  (let* ((variable-tuple (cond ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                               ((x-list-fonts "Verdana")         '(:font "Verdana"))
                               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))
    
    (custom-theme-set-faces 'user
                            `(org-level-8 ((t (,@headline ,@variable-tuple))))
                            `(org-level-7 ((t (,@headline ,@variable-tuple))))
                            `(org-level-6 ((t (,@headline ,@variable-tuple))))
                            `(org-level-5 ((t (,@headline ,@variable-tuple))))
                            `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
                            `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
                            `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.35))))
                            `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.5))))
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))))

(setq writer-p nil)

(defun writer-off ()
  (interactive)
  (setq writer-p nil)
  (setq org-hide-emphasis-markers nil)
  (org-indent-mode -1)
  (remove-hook 'org-mode-hook #'writer-setup)
  (org-bullets-mode -1)
  (font-lock-remove-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (text-scale-set 0)
  (custom-theme-set-faces 'user
                          `(org-level-8 ((t :inherit outline-8)))
                          `(org-level-7 ((t :inherit outline-7)))
                          `(org-level-6 ((t :inherit outline-6)))
                          `(org-level-5 ((t :inherit outline-5)))
                          `(org-level-4 ((t :inherit outline-4)))
                          `(org-level-3 ((t :inherit outline-3)))
                          `(org-level-2 ((t :inherit outline-2)))
                          `(org-level-1 ((t :inherit outline-1)))
                          `(org-document-title ((((class color) (background light)) (:foreground "midnight blue" :weight bold))
    (((class color) (background dark)) (:foreground "pale turquoise" :weight bold)))
    (t (:weight bold))))
  (org-mode))

(defun writer-on ()
  (interactive)
  (setq writer-p t)
  (add-hook 'org-mode-hook #'writer-setup)
  (org-mode))

(defun writer-toggle ()
  (interactive)
  (if writer-p (writer-off) (writer-on)))



(provide 'writer)
