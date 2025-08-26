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
(require 'magit-git)

(require 'ghpr-api)
(require 'ghpr-repo)
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
  '(("^\\(> \\+[^+]\\)\\(.*\\)$" (0 'ghpr-review-added-line t))
    ("^\\(> \\+\\)$"             (0 'ghpr-review-added-line t))
    ("^\\(> -[^-]\\)\\(.*\\)$"   (0 'ghpr-review-removed-line t))
    ("^\\(> -\\)$"               (0 'ghpr-review-removed-line t))
    ("^<.*$"                     (0 'ghpr-review-existing-review t))
    ("^\\(> [^+-]\\)\\(.*\\)$"   (0 'ghpr-review-misc-line t))
    ("^\\(> \\)$"                (0 'ghpr-review-misc-line t)))
  "Font lock keywords for ghpr-review-mode.")

(defvar-local ghpr--review-diff-content nil
  "Buffer-local variable storing the diff content for the current PR.")

(defvar-local ghpr--review-pr-metadata nil
  "Buffer-local variable storing the PR metadata for the current PR.")

(defvar-local ghpr--review-repo-name nil
  "Buffer-local variable storing the repository name for the current PR.")

(defvar ghpr-review-mode-map
  (let ((map (make-sparse-keymap))
        (prefix-map (make-sparse-keymap)))
    (define-key prefix-map (kbd "C-c") 'ghpr-review-comment)
    (define-key prefix-map (kbd "C-a") 'ghpr-review-approve)
    (define-key prefix-map (kbd "C-r") 'ghpr-review-reject-changes)
    (define-key prefix-map (kbd "C-o") 'ghpr-review-checkout-branch)
    (define-key prefix-map (kbd "C-k") 'ghpr-review-quit)
    (define-key map (kbd "C-c") prefix-map)
    map)
  "Keymap for `ghpr-review-mode'.")

(define-derived-mode ghpr-review-mode text-mode "GHPR Review"
  "Major mode for reviewing GitHub pull requests."
  :group 'ghpr
  :keymap ghpr-review-mode-map
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
    (setq ghpr--review-pr-metadata pr)
    (setq ghpr--review-repo-name repo-name)
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

(defun ghpr--is-comment-line (line)
  "Return t if LINE is a user comment (no prefix), nil otherwise."
  (not (or (string-prefix-p ">" line)
           (string-prefix-p "<" line))))

(defun ghpr--is-code-line (line)
  "Return t if LINE is a diff code line (> + or > -), nil otherwise."
  (or (string-prefix-p "> +" line)
      (string-prefix-p "> -" line)))

(defun ghpr--find-preceding-code-line (lines current-index)
  "Find the most recent code line before CURRENT-INDEX in LINES.
Returns the line content and its index as (line . index), or nil if none found."
  (let ((index (1- current-index))
        (found nil))
    (while (and (>= index 0) (not found))
      (let ((line (aref lines index)))
        (if (ghpr--is-code-line line)
            (setq found (cons line index))
          (setq index (1- index)))))
    found))

(defun ghpr--file-path+sha (lines start-index)
  "Find file path and commit SHA by searching backwards from START-INDEX in LINES.
Returns a cons cell (file-path . commit-sha)."
  (let ((file-path nil)
        (commit-sha nil)
        (index start-index))
    (while (and (>= index 0) (or (not file-path) (not commit-sha)))
      (let ((line (aref lines index)))
        (cond
         ((string-match "^> diff --git a/\\(.+\\) b/" line)
          (setq file-path (match-string 1 line)))
         ((string-match "^> \\+\\+\\+ b/\\(.+\\)$" line)
          (setq file-path (match-string 1 line)))
         ((string-match "^> index \\([a-f0-9]+\\)\\.\\.\\([a-f0-9]+\\)" line)
          (setq commit-sha (match-string 2 line))))
        (setq index (1- index))))
    (cons (when file-path (substring-no-properties file-path))
          (when commit-sha (substring-no-properties commit-sha)))))

(defun ghpr--hunk-header (lines start-index)
  "Find hunk header by searching backwards from START-INDEX in LINES.
Returns a cons cell (hunk-info . hunk-start-index) or (nil . nil) if not found."
  (let ((index start-index)
        (hunk-info nil)
        (hunk-start nil))
    (while (and (>= index 0) (not hunk-info))
      (let ((line (aref lines index)))
        (when (string-match "^> @@.*@@" line)
          (setq hunk-info (substring-no-properties line))
          (setq hunk-start index)))
        (setq index (1- index)))
    (cons hunk-info hunk-start)))

(defun ghpr--line-meta-p (line)
  "Determines if the given LINE starts with `>'."
  (string-prefix-p ">" line))

(defun ghpr--line-existing-comment-p (line)
  "Determines if the given LINE is an existing comment starting with `>'."
  (string-prefix-p "<" line))

(defun ghpr--special-line-p (line)
  "Determines if the given LINE starts with a special character."
  (or (ghpr--line-meta-p line)
      (ghpr--line-existing-comment-p line)))

(defun ghpr--calculate-diff-position (lines hunk-start-index code-line-index)
  "Calculate diff position from HUNK-START-INDEX to CODE-LINE-INDEX in LINES.
Position 1 is the line just below the first @@ hunk header.
Count only lines that start with > (diff content, not comments).
Returns the diff position as an integer."
  (let ((position-count 0)
        (index (1+ hunk-start-index)))
    (while (<= index code-line-index)
      (let ((line (aref lines index)))
        (when (ghpr--line-meta-p line)
          (setq position-count (1+ position-count))))
      (setq index (1+ index)))
    position-count))

(defun ghpr--determine-line-type (lines code-line-index)
  "Determine the line type (added, removed, or context) for the line at CODE-LINE-INDEX.
Returns a string: \"added\", \"removed\", or \"context\"."
  (let ((code-line (aref lines code-line-index)))
    (cond
     ((string-prefix-p "> +" code-line) "added")
     ((string-prefix-p "> -" code-line) "removed")
     (t "context"))))

(defun ghpr--parse-diff-context (lines code-line-index)
  "Parse diff context around CODE-LINE-INDEX to extract file path, line type, commit SHA, and diff position.
Returns an alist with file-path, line-type, commit-sha, and diff-position information."
  (let* ((file+sha (ghpr--file-path+sha lines code-line-index))
         (file-path (car file+sha))
         (commit-sha (cdr file+sha))
         (hunk-result (ghpr--hunk-header lines code-line-index))
         (hunk-info (car hunk-result))
         (hunk-start (cdr hunk-result))
         (diff-position
          (when hunk-start
            (ghpr--calculate-diff-position lines hunk-start code-line-index)))
         (line-type (ghpr--determine-line-type lines code-line-index)))

    `((file-path . ,file-path)
      (line-type . ,line-type)
      (hunk-info . ,hunk-info)
      (commit-sha . ,commit-sha)
      (diff-position . ,diff-position))))

(defun ghpr--collect-multiline-comment (lines start-index)
  "Collect consecutive comment lines starting from START-INDEX in LINES.
Returns a cons cell (comment-lines . next-index) where comment-lines is a list
of comment lines and next-index is the index after the last comment line."
  (let ((comment-lines (list (substring-no-properties (aref lines start-index))))
        (next-index (1+ start-index)))
    (while (and (< next-index (length lines))
                (ghpr--is-comment-line (aref lines next-index)))
      (push (substring-no-properties (aref lines next-index)) comment-lines)
      (setq next-index (1+ next-index)))
    (cons (reverse comment-lines) next-index)))

(defun ghpr--collect-review-body ()
  "Collect review body comment from the top of the buffer.
Returns the body text as a string, or nil if no body found.
Body is everything from the start until the first line with special characters (>, <)."
  (let* ((lines (vconcat (split-string (buffer-string) "\n")))
         (body-lines '())
         (index 0))
    (while (and (< index (length lines))
                (not (ghpr--special-line-p (aref lines index))))
      (let ((line (aref lines index)))
        (unless (string-empty-p (string-trim line))
          (push (substring-no-properties line) body-lines)))
      (setq index (1+ index)))
    (when body-lines
      (string-join (reverse body-lines) "\n"))))

(defun ghpr--skip-review-body (lines)
  "Skip to the first line with special characters (>, <) to get past the body.
Returns the index of the first line after the review body."
  (let ((index 0)
        (past-body nil))
    (while (and (< index (length lines)) (not past-body))
      (let ((line (aref lines index)))
        (when (ghpr--special-line-p line)
          (setq past-body t))
        (unless past-body
          (setq index (1+ index)))))
    index))

(defun ghpr--collect-inline-comments-after-body (lines start-index)
  "Collect inline comments starting from START-INDEX in LINES.
Returns a list of comment entries with their GitHub API context."
  (let ((comments '())
        (index start-index))
    (while (< index (length lines))
      (let ((line (aref lines index)))
        (when (ghpr--is-comment-line line)
          (let* ((comment-result (ghpr--collect-multiline-comment lines index))
                 (comment-lines (car comment-result))
                 (next-index (cdr comment-result))
                 (comment-entry (ghpr--build-comment-with-context comment-lines index lines)))
            (when comment-entry
              (push comment-entry comments))
            (setq index (1- next-index))))
      (setq index (1+ index))))
    (reverse comments)))

(defun ghpr--comment-to-api-format (comment)
  "Convert a collected comment to GitHub API format.
Errors if the comment is missing required fields or has empty body."
  (let ((path (alist-get 'file-path comment))
        (position (alist-get 'diff-position comment))
        (comment-body (alist-get 'comment comment)))
    (unless path
      (error "Comment missing file path: %S" comment))
    (unless position
      (error "Comment missing diff position: %S" comment))
    (unless (and comment-body (not (string-empty-p (string-trim comment-body))))
      (error "Comment missing or empty body: %S" comment))
    `((path . ,path)
      (position . ,position)
      (body . ,comment-body))))

(defun ghpr--build-comment-with-context (comment-lines comment-start-index lines)
  "Build a comment entry with GitHub API context for COMMENT-LINES.
Uses COMMENT-START-INDEX to find the preceding code line in LINES.
Returns an alist with comment, file-path, commit-sha, and diff-position, or nil if no context found."
  (let ((preceding-code (ghpr--find-preceding-code-line lines comment-start-index)))
    (when preceding-code
      (let* ((code-index (cdr preceding-code))
             (context (ghpr--parse-diff-context lines code-index))
             (full-comment (string-join comment-lines "\n")))
        `((comment . ,full-comment)
          (file-path . ,(alist-get 'file-path context))
          (commit-sha . ,(alist-get 'commit-sha context))
          (diff-position . ,(alist-get 'diff-position context)))))))

(defun ghpr--collect-review-comments ()
  "Collect all inline review comments from the current buffer.
Returns an alist of comments with their associated diff lines and GitHub API context.
Multi-line comments are grouped together until the next line with angle brackets.
Skips the review body at the top of the buffer."
  (let* ((lines (vconcat (split-string (buffer-string) "\n")))
         (body-end-index (ghpr--skip-review-body lines)))
    (ghpr--collect-inline-comments-after-body lines body-end-index)))

(defun ghpr-collect-review-comments ()
  "Interactive command to collect and display review comments from current buffer."
  (interactive)
  (let ((body (ghpr--collect-review-body))
        (comments (ghpr--collect-review-comments)))
    (if (or body comments)
        (with-output-to-temp-buffer "*GHPR Review Comments*"
          (prin1 body)
          (prin1 comments))
      (message "No review comments found in current buffer."))))

(defun ghpr--submit-review (event)
  "Submit a review with the specified EVENT type.
EVENT should be 'COMMENT', 'APPROVE', or 'REQUEST_CHANGES'.
Collects review body and inline comments from current buffer."
  (unless ghpr--review-pr-metadata
    (error "No PR metadata found in buffer"))
  (unless ghpr--review-repo-name
    (error "No repository name found in buffer"))

  (let* ((body (ghpr--collect-review-body))
         (inline-comments (ghpr--collect-review-comments))
         (pr-number (alist-get 'number ghpr--review-pr-metadata))
         (commit-sha (magit-rev-parse (or (alist-get 'head_sha ghpr--review-pr-metadata)
                                          (alist-get 'merge_commit_sha ghpr--review-pr-metadata))))
         (api-comments (mapcar #'ghpr--comment-to-api-format inline-comments)))

    (when (and (not body) (not api-comments))
      (error "No review body or comments found"))

    (when (and (member event '("REQUEST_CHANGES" "COMMENT"))
               (or (not body) (string-empty-p (string-trim body))))
      (error "Review body is required for %s events" event))

    (unless (ghpr--create-review ghpr--review-repo-name
                                 pr-number
                                 commit-sha
                                 (or body "")
                                 event
                                 api-comments)
      (message "Failed to submit review"))))

(defun ghpr-review-comment ()
  "Submit review comments with COMMENT event."
  (interactive)
  (ghpr--submit-review "COMMENT"))

(defun ghpr-review-approve ()
  "Submit review with APPROVE event."
  (interactive)
  (ghpr--submit-review "APPROVE"))

(defun ghpr-review-reject-changes ()
  "Submit review with REQUEST_CHANGES event."
  (interactive)
  (ghpr--submit-review "REQUEST_CHANGES"))

(defun ghpr-review-checkout-branch ()
  "Check out the PR branch using Magit."
  (interactive)
  (unless ghpr--review-pr-metadata
    (error "No PR metadata found in buffer"))
  (ghpr--checkout-pr-branch ghpr--review-pr-metadata))

(defun ghpr-review-quit ()
  "Quit and close the review buffer with confirmation."
  (interactive)
  (when (yes-or-no-p "Quit review? ")
    (kill-buffer (current-buffer))))

(provide 'ghpr-review)

;;; ghpr-review.el ends here
