;;; ghpr-api.el --- Interactions for the GitHub API -*- lexical-binding: t -*-

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

;; Provides procedures for interacting with the GitHub API, including
;; authentication.

;;; Code:

(require 'auth-source)
(require 'request)
(require 'json)

(defun ghpr--get-token ()
  "Retrieve GitHub authentication token from auth-sources.
Returns the token string if found, nil otherwise."
  (auth-source-pick-first-password :host "api.github.com"))

(defun ghpr--parse-api-pr (pr)
  "Parse a single pull request object, keeping only essential fields."
  (let ((base (alist-get 'base pr))
        (head (alist-get 'head pr))
        (user (alist-get 'user pr)))
    `((url . ,(alist-get 'url pr))
      (id . ,(alist-get 'id pr))
      (diff_url . ,(alist-get 'diff_url pr))
      (patch_url . ,(alist-get 'patch_url pr))
      (number . ,(alist-get 'number pr))
      (state . ,(alist-get 'state pr))
      (locked . ,(alist-get 'locked pr))
      (title . ,(alist-get 'title pr))
      (body . ,(alist-get 'body pr))
      (username . ,(alist-get 'login user))
      (author . ,(alist-get 'login user))
      (base_sha . ,(alist-get 'sha base))
      (base_ref . ,(alist-get 'ref base))
      (head_sha . ,(alist-get 'sha head))
      (head_ref . ,(alist-get 'ref head))
      (head_repo . ,(alist-get 'repo head))
      (head_user . ,(alist-get 'user head))
      (merge_commit_sha . ,(alist-get 'merge_commit_sha pr)))))

(defun ghpr--parse-api-pr-list (pr-list)
  "Parse a list of pull request objects, keeping only essential fields."
  (mapcar #'ghpr--parse-api-pr pr-list))

(defun ghpr--list-open-prs (repo-name)
  "List open pull requests for REPO-NAME in owner/repo format.
Returns a list of pull request objects on success, nil on failure."
  (let ((token (ghpr--get-token))
        (url (format "https://api.github.com/repos/%s/pulls?state=open" repo-name))
        (result nil))
    (when token
      (request url
        :type "GET"
        :headers `(("Accept" . "application/vnd.github+json")
                   ("Authorization" . ,(format "Bearer %s" token))
                   ("X-GitHub-Api-Version" . "2022-11-28"))
        :parser 'json-read
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result (ghpr--parse-api-pr-list data))))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Error fetching pull requests: %s" error-thrown)))))
    result))

(defun ghpr--get-pr (repo-name pr-number)
  "Retrieve a single pull request by PR-NUMBER in REPO-NAME.
Returns a parsed PR object on success, nil on failure."
  (let ((token (ghpr--get-token))
        (url (format "https://api.github.com/repos/%s/pulls/%s" repo-name pr-number))
        (result nil))
    (when token
      (request url
        :type "GET"
        :headers `(("Accept" . "application/vnd.github+json")
                   ("Authorization" . ,(format "Bearer %s" token))
                   ("X-GitHub-Api-Version" . "2022-11-28"))
        :parser 'json-read
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result (ghpr--parse-api-pr data))))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Error fetching PR: %s" error-thrown)))))
    result))

(defun ghpr--get-diff-content (repo-name pr-number)
  "Retrieve the diff contents for PR-NUMBER in REPO-NAME."
  (let ((token (ghpr--get-token))
        (url (format "https://api.github.com/repos/%s/pulls/%s" repo-name pr-number))
        (result nil))
    (when token
      (request url
        :type "GET"
        :headers `(("Accept" . "application/vnd.github.diff")
                   ("Authorization" . ,(format "Bearer %s" token))
                   ("X-GitHub-Api-Version" . "2022-11-28"))
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result data)))
        :error (cl-function
                (lambda (&key error-thrown &allow-other-keys)
                  (message "Error fetching diff: %s" error-thrown)))))
    result))

(defun ghpr--create-review (repo-name pr-number commit-id body event comments)
  "Create a review for PR-NUMBER in REPO-NAME.
COMMIT-ID is the SHA of the commit to review.
BODY is the overall review comment.
EVENT should be 'APPROVE', 'REQUEST_CHANGES', or 'COMMENT'.
COMMENTS is a list of inline comments, each with keys: path, position, body.
Returns t on success, nil on failure."
  (let ((token (ghpr--get-token))
        (url (format "https://api.github.com/repos/%s/pulls/%s/reviews" repo-name pr-number))
        (result nil)
        (payload `((commit_id . ,commit-id)
                   (body . ,body)
                   (event . ,event)
                   (comments . ,(vconcat comments)))))

    (when token
      (request url
        :type "POST"
        :headers `(("Accept" . "application/vnd.github+json")
                   ("Authorization" . ,(format "Bearer %s" token))
                   ("X-GitHub-Api-Version" . "2022-11-28")
                   ("Content-Type" . "application/json"))
        :data (json-encode payload)
        :parser 'json-read
        :sync t
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    (setq result t)
                    (message "Review created successfully")))
        :error (cl-function
                (lambda (&key data error-thrown response &allow-other-keys)
                  (let ((status-code (when response (request-response-status-code response)))
                        (error-body (when data (json-encode data)))
                        (errors (when data (alist-get 'errors data))))
                    (message "Error creating review (HTTP %s): %s"
                             (or status-code "unknown")
                             error-thrown)
                    (when errors
                      (--each errors
                        (message "Error creating review (HTTP %s): %s" (or status-code "unknown") it))))))))
    result))

(provide 'ghpr-api)

;;; ghpr-api.el ends here
