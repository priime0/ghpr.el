;;; ghpr-prs.el --- PR list buffer functionality -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
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

;; ghpr-prs.el provides buffer-based PR list functionality using magit-section
;; for interactive navigation and selection of pull requests.

;;; Code:

(require 'magit)
(require 'magit-section)
(require 'ghpr-api)
(require 'ghpr-utils)
(require 'ghpr-repo)

(defvar-keymap ghpr-pr-section-map
  :doc "Keymap for PR sections."
  :parent magit-section-mode-map
  "RET" #'ghpr-show-pr
  "<remap> <magit-visit-thing>" #'ghpr-show-pr)

(defvar ghpr-prs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "g" 'ghpr-prs-refresh)
    (define-key map (kbd "RET") 'ghpr-show-pr)
    map)
  "Keymap for `ghpr-prs-mode'.")

(define-derived-mode ghpr-prs-mode magit-mode "GHPR PRs"
  "Major mode for listing GitHub pull requests."
  :group 'ghpr
  :keymap ghpr-prs-mode-map
  (hack-dir-local-variables-non-file-buffer)
  (setq truncate-lines t))

(defvar-local ghpr--prs-repo-name nil
  "Repository name for the current PR list buffer.")

(defun ghpr--prs-setup-buffer (repo-name)
  "Create and setup the PR list buffer for REPO-NAME, or reuse existing buffer."
  (let* ((buffer-name (format "*ghpr-prs: %s*" repo-name))
         (existing-buffer (get-buffer buffer-name))
         (buffer (or existing-buffer (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      (unless existing-buffer
        (ghpr-prs-mode)
        (setq ghpr--prs-repo-name repo-name))
      (ghpr--prs-refresh-buffer))
    buffer))

(defun ghpr--prs-refresh-buffer (&optional error-message)
  "Refresh the PR list buffer content, preserving cursor position."
  (unless ghpr--prs-repo-name
    (error "No repository name set in buffer"))
  (let ((current-section (ghpr--get-current-pr))
        (prs+error (ghpr--fetch-prs-with-error-handling error-message)))
    (ghpr--rebuild-buffer-content (car prs+error) (cdr prs+error))
    (ghpr--restore-cursor-or-top current-section)))

(defun ghpr--get-current-pr ()
  "Get the current PR value to restore cursor position after refresh."
  (and (magit-current-section)
       (oref (magit-current-section) value)))

(defun ghpr--fetch-prs-with-error-handling (error-message)
  "Fetch PRs, returning (prs . error-message) cons cell."
  (if error-message
      (cons nil error-message)
    (condition-case err
        (cons (ghpr--list-open-prs ghpr--prs-repo-name) nil)
      (error (cons nil (error-message-string err))))))

(defun ghpr--rebuild-buffer-content (prs error-message)
  "Rebuild buffer content with PRS and optional ERROR-MESSAGE."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (ghpr--insert-error-if-present error-message)
    (ghpr--insert-prs-section prs error-message)))

(defun ghpr--insert-error-if-present (error-message)
  "Insert error message at top of buffer if ERROR-MESSAGE is non-nil."
  (when error-message
    (insert (propertize (format "Error: %s\n\n" error-message)
                        'face 'error))))

(defun ghpr--insert-prs-section (prs error-message)
  "Insert the main PRs section with PRS list or appropriate message."
  (magit-insert-section (prs-buffer)
    (magit-insert-heading (format "Pull Requests for %s" ghpr--prs-repo-name))
    (cond
     ((and prs (not error-message))
      (dolist (pr prs)
        (ghpr--insert-pr-section pr)))
     (error-message
      (insert "Could not fetch pull requests due to error above.\n"))
     (t
      (insert "No open pull requests found.\n")))))

(defun ghpr--restore-cursor-or-top (current-section)
  "Restore cursor to CURRENT-SECTION if it exists, otherwise go to top."
  (if current-section
      (ghpr--restore-cursor-position current-section)
    (goto-char (point-min))))

(defun ghpr--restore-cursor-position (target-pr)
  "Restore cursor position to TARGET-PR if it still exists after refresh."
  (goto-char (point-min))
  (unless (ghpr--find-and-goto-pr target-pr)
    (goto-char (point-min))))

(defun ghpr--find-and-goto-pr (target-pr)
  "Find TARGET-PR in buffer and move cursor to it. Return t if found."
  (catch 'found
    (while (not (eobp))
      (when (ghpr--current-line-matches-pr-p target-pr)
        (throw 'found t))
      (forward-line 1))
    nil))

(defun ghpr--current-line-matches-pr-p (target-pr)
  "Return t if current line contains a section matching TARGET-PR."
  (let ((section (magit-current-section)))
    (and section
         (eq (oref section type) 'pr)
         (equal (oref section value) target-pr))))

(defun ghpr-prs-refresh ()
  "Refresh the current PR list buffer."
  (interactive)
  (ghpr--prs-refresh-buffer))

(defun ghpr--insert-pr-section (pr)
  "Insert a magit section for PR."
  (let ((pr-summary (ghpr--pr-summary pr))
        (pr-number (alist-get 'number pr)))
    (magit-insert-section (pr pr)
      (oset (magit-current-section) keymap ghpr-pr-section-map)
      (insert pr-summary)
      (insert "\n"))))

(defun ghpr-show-pr ()
  "Open the PR at point in review mode."
  (interactive)
  (let* ((section (magit-current-section))
         (pr (and section
                  (eq (oref section type) 'pr)
                  (oref section value)))
         (prs-buffer (current-buffer)))
    (if pr
        (progn
          (require 'ghpr-review)
          (ghpr--open-pr pr ghpr--prs-repo-name prs-buffer))
      (user-error "No PR at point: section-type=%S, section-value=%S"
                  (and section (oref section type))
                  (and section (oref section value))))))

(defun ghpr-prs-list (repo-name)
  "Display a buffer listing open PRs for REPO-NAME."
  (let ((buffer (ghpr--prs-setup-buffer repo-name)))
    (switch-to-buffer buffer)))

(provide 'ghpr-prs)

;;; ghpr-prs.el ends here
