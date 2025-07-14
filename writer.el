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
(setq org-bullets-bullet-list (list "○" "" "" "")) ; Eliminate all Bullet points except for the first.

(defun writer-mode-on (reset)
  (setq mode-line-format nil)
  (setq org-hide-emphasis-markers t)
  (org-indent-mode 1)
  (org-variable-pitch-minor-mode 1)
  (font-lock-add-keywords nil '(("^ *\\([-]\\) " (0 (prog1 ()(compose-region (match-beginning 1) (match-end 1) "•"))))))
  (let ((base-font-color (face-foreground 'default nil 'default)))
    (face-remap-add-relative 'org-level-8 `(:weight bold :foreground ,base-font-color))
    (face-remap-add-relative 'org-level-7 `(:weight bold :foreground ,base-font-color))
    (face-remap-add-relative 'org-level-6 `(:weight bold :foreground ,base-font-color))
    (face-remap-add-relative 'org-level-5 `(:weight bold :foreground ,base-font-color))
    (face-remap-add-relative 'org-level-4 `(:weight bold :foreground ,base-font-color :height 1.1))
    (face-remap-add-relative 'org-level-3 `(:weight bold :foreground ,base-font-color :height 1.25))
    (face-remap-add-relative 'org-level-2 `(:weight bold :foreground ,base-font-color :height 1.35))
    (face-remap-add-relative 'org-level-1 `(:weight bold :foreground ,base-font-color :height 1.5))
    (face-remap-add-relative 'org-document-title `(:weight bold :foreground ,base-font-color :height 1.75 :underline nil)))
  (org-bullets-mode 1) ; must be last
  (when reset (org-mode)))

(defun writer-mode-off (reset)
  (setq mode-line-format t)
  (setq org-hide-emphasis-markers nil)
  (org-indent-mode -1)
  (org-variable-pitch-minor-mode -1)
  (font-lock-remove-keywords nil '(("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (org-bullets-mode -1) ; must be last
  (when reset (org-mode)))

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
		   (setq org-export-with-section-numbers with-section-numbers)
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
    (set-keymap-parent map org-mode-map) ; set parent mode map
    (define-key map (kbd "s-c") #'copy-as-rtf)
    map))

(define-minor-mode writer-mode
  :lighter " Writer")

; Manage State
(defun writer-mode-toggle ()
  (if (derived-mode-p 'org-mode)
      (if writer-mode (writer-mode-on nil) (writer-mode-off t))
      (progn (princ "Failed to launch Writer mode.  Org mode must be active.")
	     (ding))))

(add-hook 'org-mode-hook (lambda () (if writer-mode (writer-mode-on t) (writer-mode-off nil))))
(add-hook 'writer-mode-hook #'writer-mode-toggle) ; start or clean up

;; Remove minor mode dependencies from modeline
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
