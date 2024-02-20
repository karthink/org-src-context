;;; org-src-context.el --- LSP support for org-src buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation, Inc.

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: tools, languages, extensions

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

;;; Commentary:

;; This file contains the code dealing with Language Server Protocol support via
;; other packages in Org Source buffers.

;;; Code:

(require 'org)
(require 'ob)
(require 'ob-tangle)
(require 'org-src)
(require 'cl-lib)

(declare-function eglot--maybe-activate-editing-mode "eglot")
(declare-function lsp-deferred "lsp-mode")

(defgroup org-src-context nil
  "Provides LSP support in org-src buffers."
  :group 'org)

(defcustom org-src-context-narrow-p nil
  "Whether org-src buffers should be narrowed to the code block
with Eglot enabled."
  :type 'boolean
  :group 'org-src-context)

(defcustom org-src-context-lsp-command #'eglot--maybe-activate-editing-mode
  "LSP integration backend for org-src-context.

Choose between Eglot and LSP-mode."
  :type '(choice
          (function-item :tag "Eglot" eglot--maybe-activate-editing-mode)
          (function-item :tag "Lsp-mode" lsp-deferred))
  :group 'org-src-context)

(defface org-src-context-read-only
  '((((class color) (min-colors 257) (background light))
     :background "#ffeeee" :extend t)
    (((class color) (min-colors 88) (background light))
     :background "#ffdddd" :extend t)
    (((class color) (min-colors 88) (background dark))
     :background "#553333" :extend t))
  "Face for read-only sections of org-src buffer"
  :group 'org-src-context)

(defvar-local org-src-context--before-block-marker nil)
(defvar-local org-src-context--after-block-marker nil)

(defun org-src-context--edit-src-ad (orig-fn &rest args)
  (if-let* ((info (org-babel-get-src-block-info 'light))
            (lang (car info))
            (this-block-data
             (save-excursion
               (goto-char
                (org-element-property :post-affiliated (org-element-at-point)))
               (car (org-babel-tangle-single-block 1 t))))
            (tangle-file (car this-block-data))
            (this-block (cadr this-block-data))
            (all-blocks (cdar (org-babel-tangle-collect-blocks
                               lang (alist-get :tangle (caddr info)))))
            (extra-blocks (list nil)))
    
      (prog1 (apply orig-fn args)
        (setq extra-blocks
              (cl-loop for block in all-blocks
                       until (equal (nth 1 block) (nth 1 this-block))
                       collect block into before-blocks
                       finally return
                       (cons before-blocks (nthcdr (1+ (length before-blocks))
                                                   all-blocks))))
        
        (when (or (car extra-blocks) (cdr extra-blocks))
          (setq-local org-src-context--before-block-marker (point-min-marker))
          (set-marker-insertion-type org-src-context--before-block-marker t)
          (setq-local org-src-context--after-block-marker  (point-max-marker))
          (set-marker-insertion-type org-src-context--after-block-marker nil)
          ;; TODO: Handle :padlines, :shebang
          ;; Code blocks before the current one
          (cl-loop initially do
                   (progn (goto-char (marker-position org-src-context--before-block-marker))
                          (when (car extra-blocks) (insert "\n") (backward-char 1)))
                   for block in (car extra-blocks)
                   for code = (propertize (concat "\n" (nth 6 block)
                                                  (propertize "\n" 'rear-nonsticky t))
                                          'read-only t
                                          'font-lock-face 'org-src-context-read-only)
                   do (insert code))

	  (set-marker-insertion-type org-src-context--before-block-marker nil)

          ;; Code blocks after the current one
          (cl-loop initially do (goto-char (marker-position org-src-context--after-block-marker))
                   for block in (cdr extra-blocks)

                   for code = (propertize (concat "\n" (nth 6 block)
                                                  (propertize "\n" 'rear-nonsticky t))
                                          'read-only t
                                          'font-lock-face 'org-src-context-read-only)
                   do (insert code))
          
          (when org-src-context-narrow-p
            (narrow-to-region (marker-position org-src-context--before-block-marker)
                              (marker-position org-src-context--after-block-marker)))
          
          (goto-char (marker-position org-src-context--before-block-marker))
          (set-window-start (selected-window)
                            (marker-position org-src-context--before-block-marker)))
        
        (org-src-context--connect-maybe info tangle-file))
    
    ;; No tangle file, don't do anything
    (apply orig-fn args)))

(defun org-src-context--exit-src-ad ()
  (when-let ((markerp org-src-context--before-block-marker)
             (markerp org-src-context--after-block-marker)
             (beg (marker-position org-src-context--before-block-marker))
             (end (marker-position org-src-context--after-block-marker))
             (inhibit-read-only t))
    (when org-src-context-narrow-p
      (widen))
    (delete-region end (point-max))
    (delete-region (point-min) beg)))

(defun org-src-context--connect-maybe (info tangle-file)
  "Prepare org source block buffer for an LSP connection"
  (when tangle-file
    ;; Handle directory paths in tangle-file
    (let* ((fnd (file-name-directory tangle-file))
           (mkdirp (thread-last info caddr (alist-get :mkdirp)))
           ;;`file-name-concat' is emacs 28.1+ only
           (fnd-absolute (concat (temporary-file-directory) (or fnd ""))))
      (cond
       ((not fnd) t)
       ((file-directory-p fnd-absolute) t)
       ((and fnd (and (stringp mkdirp) (string= (downcase mkdirp) "yes")))
        (make-directory fnd-absolute 'parents))
       (t (user-error
           (format "Cannot create directory \"%s\", please use the :mkdirp header arg." fnd))))
      
      (setq buffer-file-name (concat (temporary-file-directory) tangle-file))
      (pcase org-src-context-lsp-command
	('eglot--maybe-activate-editing-mode
	 (require 'eglot)
	 (when-let ((current-server (eglot-current-server)))
	   (funcall org-src-context-lsp-command)))
	('lsp-deferred (funcall org-src-context-lsp-command))))))

;;;###autoload
(define-minor-mode org-src-context-mode
  "Toggle Org-Src-Context mode. When turned on, you can start persistent
LSP connections using Eglot in org-src buffers.

To inform the Language Server about files corresponding to code
blocks to track, use `:tangle' headers with code blocks. LSP
support is limited to the current file being edited."
  :global t
  :lighter nil
  :group 'org-src-context
  (if org-src-context-mode
      (progn
        (advice-add 'org-edit-src-code :around #'org-src-context--edit-src-ad)
        (advice-add 'org-edit-src-exit :before #'org-src-context--exit-src-ad))
    (advice-remove 'org-edit-src-code #'org-src-context--edit-src-ad)
    (advice-remove 'org-edit-src-exit #'org-src-context--exit-src-ad)))

(provide 'org-src-context)
;;; org-src-context.el ends here

