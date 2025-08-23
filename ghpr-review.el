;;; ghpr-review.el --- PR workflow functionality -*- lexical-binding: t -*-

;; Author: Lucas Sta Maria
;; Maintainer: Lucas Sta Maria
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

;; ghpr-review.el provides the functionality for interacting with a pull request
;; -- from inserting PR comments to viewing the PR diffs.

;;; Code:

(require 'request)

(require 'ghpr-api)
(require 'ghpr-utils)

(defface ghpr-review-added-line
  '((t (:inherit diff-indicator-added)))
  "Face for added lines in PR diff."
  :group 'ghpr)

(defface ghpr-review-removed-line
  '((t (:inherit diff-indicator-removed)))
  "Face for removed lines in PR diff."
  :group 'ghpr)

(defface ghpr-review-existing-review
  '((t (:foreground "purple")))
  "Face for existing review comments."
  :group 'ghpr)

(defface ghpr-review-misc-line
  '((t (:inherit diff-header)))
  "Face for miscellaneous lines in PR diff."
  :group 'ghpr)

(defvar ghpr-review-font-lock-keywords
  '(("^\\(> \\+[^+]\\)\\(.*\\)$"
     (0 'ghpr-review-added-line t))
    ("^\\(> \\+\\)$"  ; Handle line with just "> +"
     (0 'ghpr-review-added-line t))
    ("^\\(> -[^-]\\)\\(.*\\)$"
     (0 'ghpr-review-removed-line t))
    ("^\\(> -\\)$"  ; Handle line with just "> -"
     (0 'ghpr-review-removed-line t))
    ("^<.*$"
     (0 'ghpr-review-existing-review t))
    ("^>.*$"
     (0 'ghpr-review-misc-line)))
  "Font lock keywords for ghpr-review-mode.")

(defvar-local ghpr--review-diff-content nil
  "Buffer-local variable storing the diff content for the current PR.")

(define-derived-mode ghpr-review-mode text-mode "GHPR Review"
  "Major mode for reviewing GitHub pull requests."
  :group 'ghpr
  (setq truncate-lines t)
  (setq font-lock-defaults '(ghpr-review-font-lock-keywords t nil nil nil))
  (font-lock-ensure))
(defun ghpr--prefix-line (line)
  "Prefix the LINE with `> '."
  (concat "> " line))

(defun ghpr--prefix-lines (text)
  "Prefix every line in TEXT with `> '."
  (let ((lines (split-string text "\n")))
    (mapconcat #'ghpr--prefix-line lines "\n")))

(defun ghpr--open-pr/collect-contents (pr diff-content)
  "Return a string of the title, body, and patch content separated by newlines."
  (let* ((body (or (alist-get 'body pr) ""))
         (content-parts (list (ghpr--pr-summary pr))))
    (when (not (string-empty-p body))
      (push body content-parts))
    (when diff-content
      (push diff-content content-parts))
    (mapconcat 'identity (reverse content-parts) "\n\n")))

(defun ghpr--open-pr/insert-contents (pr repo-name)
  "Insert the contents of the pr into the current buffer."
  (erase-buffer)
  (let* ((number (alist-get 'number pr))
         (diff-content (ghpr--get-diff-content repo-name number))
         (contents (ghpr--open-pr/collect-contents pr diff-content)))
    (setq ghpr--review-diff-content diff-content)
    (insert (ghpr--prefix-lines contents))))

(defun ghpr--open-pr (pr repo-name)
  "Open a new buffer containing the body of the PR."
  (let* ((number (alist-get 'number pr))
         (buffer-name (format "*ghpr-pr-%s*" number))
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (ghpr-review-mode)
      (ghpr--open-pr/insert-contents pr repo-name)
      (goto-char (point-min)))
    (display-buffer buffer)))

(provide 'ghpr-review)

;;; ghpr-review.el ends here
