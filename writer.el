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
(setq org-bullets-bullet-list (list "" "" "" ""))

(defun writer-mode-on ()
  (setq mode-line-format nil)
  (setq org-hide-emphasis-markers t)
  (org-indent-mode 1)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-bullets-mode 1)
  (org-variable-pitch-minor-mode)
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
                            `(org-document-title ((t (,@headline ,@variable-tuple :height 1.5 :underline nil)))))
    ))


(defun writer-mode-off ()
  (setq mode-line-format t)
  (setq org-hide-emphasis-markers nil)
  (org-indent-mode -1)
  (remove-hook 'org-mode-hook #'writer-setup)
  (org-bullets-mode -1)
  (org-variable-pitch-minor-mode -1)
  (font-lock-remove-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
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
    (((class color) (background dark)) (:foreground "pale turquoise" :weight bold))) (t (:weight bold))))
  (org-mode))

;;; There are three minor modes enabled and showing in the mode line
;; - buffer-face-mode " BufFace"
;; - org-indent-mode " Ind"
;; - org-variable-pitch-minor-mode " OVP"
;; I need to get rid of these


(defun copy-as-rtf ()
  "Export region to RTF and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((toc org-export-with-toc)
	   (with-section-numbers org-export-with-section-numbers)
	   (buf (progn
		  (setq org-export-with-toc nil) ; Preserve export settings
		  (setq org-export-with-section-numbers nil) ; Preserve export settings
		  (org-export-to-buffer 'html "Formatted Copy" nil nil t t)))
	   (html (progn
		   (setq org-export-with-toc toc)
		   (setq org-export-with-section-numbers with-section-number)
		   (with-current-buffer buf (buffer-string)))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf)
      (deactivate-mark))))

(defvar writer-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map) ; set parent
    (define-key map (kbd "s-c") #'copy-as-rtf)
    map)
  "Keymap for my extended minor mode.")

(define-minor-mode writer-mode
  "Language mode documentation."
  :lighter " Writer")

(defun writer-mode-toggle ()
  (if (derived-mode-p 'org-mode)
      (if writer-mode (writer-mode-on) (writer-mode-off))
      (progn (princ "Failed to launch Writer mode.  Org mode must be active.")
	     (ding))))

(add-hook 'writer-mode-hook #'writer-mode-toggle) ; start or clean up

;; Clean up mode-line
(defun set-mode-line (m s)
  (let ((res (assq m minor-mode-alist)))
    (when res (setcar (cdr res) s))))

(defun remove-from-mode-line (m m0 s)
  (lambda (&rest args)
    (if (bound-and-true-p m)
	(set-mode-line m0 "")
        (set-mode-line m0 s))))

;; Remove minor mode dependencies from modeline
;; (advice-add 'buffer-face-mode :before (remove-from-mode-line writer-mode 'buffer-face-mode " BufFace"))
;; (advice-add 'org-indent-mode :before (remove-from-mode-line writer-mode 'org-indent-mode " Ind"))
;; (advice-add 'org-variable-pitch-minor-mode :before (remove-from-mode-line writer-mode 'org-variable-pitch-minor-mode " OVP"))

(defun set-mode-line (m s)
  (let ((res (assq m minor-mode-alist)))
    (when res (setcar (cdr res) s))))

(defmacro remove-from-mode-line (child-mode parent-mode str)
  `(advice-add ,parent-mode  ; hide BufFace in modeline
	    :before
	    (lambda (&rest args)
	      (if (bound-and-true-p ,child-mode)
		  (set-mode-line ,parent-mode "") ; no Mode Line display for word wrap
		  (set-mode-line ,parent-mode (concat " " ,str)))))) ; restore modeline

(remove-from-mode-line writer-mode 'buffer-face-mode "BufFace")
(remove-from-mode-line writer-mode 'org-indent-mode "Ind")
(remove-from-mode-line writer-mode 'org-variable-pitch-minor-mode "OVP")

(provide 'writer)
