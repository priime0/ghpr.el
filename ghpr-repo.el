;;; ghpr-repo.el --- Interactions with the local git repository -*- lexical-binding: t -*-

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

;; ghpr-repo.el provides functionality for interacting with the local git
;; repository through Magit.

;;; Code:

(require 'magit)

(defun ghpr--get-repo-name ()
  "Get the GitHub repository name in owner/repo format from origin remote.
Returns nil if origin remote doesn't exist or is not a GitHub repository."
  (when (magit-remote-p "origin")
    (let ((origin-url (magit-get "remote.origin.url")))
      (when (string-match "github\\.com[:/]\\([^/]+/[^/.]+\\)" origin-url)
        (match-string 1 origin-url)))))

(defun ghpr--is-fork-pr-p (pr-metadata)
  "Return t if PR is from a fork, nil if from same repository.
PR-METADATA should contain head_repo and head_user information."
  (let ((head-repo (alist-get 'head_repo pr-metadata))
        (head-user (alist-get 'head_user pr-metadata))
        (origin-repo-name (ghpr--get-repo-name)))
    (when (and head-repo head-user origin-repo-name)
      (let ((head-repo-full-name (alist-get 'full_name head-repo))
            (head-user-login (alist-get 'login head-user)))
        (not (string= head-repo-full-name origin-repo-name))))))

(defun ghpr--get-or-add-remote (pr-metadata)
  "Get or add the remote for the PR's head repository.
Returns the remote name to use for fetching the PR branch."
  (let* ((head-repo (alist-get 'head_repo pr-metadata))
         (head-user (alist-get 'head_user pr-metadata))
         (head-user-login (alist-get 'login head-user))
         (head-repo-clone-url (alist-get 'clone_url head-repo))
         (remote-name head-user-login))

    (if (ghpr--is-fork-pr-p pr-metadata)
        (progn
          (unless (magit-remote-p remote-name)
            (magit-remote-add remote-name head-repo-clone-url))
          remote-name)
      "origin")))

(defun ghpr--checkout-pr-branch (pr-metadata)
  "Check out the PR branch using Magit.
Fetches the branch from appropriate remote if needed, then checks it out.
PR-METADATA should contain head_ref, head_sha, and repository information."
  (let* ((head-ref (alist-get 'head_ref pr-metadata))
         (head-sha (alist-get 'head_sha pr-metadata))
         (pr-number (alist-get 'number pr-metadata))
         (remote-name (ghpr--get-or-add-remote pr-metadata))
         (remote-branch-ref (format "%s/%s" remote-name head-ref))
         (local-branch-name head-ref))

    (unless head-ref
      (error "PR metadata missing head branch reference"))

    (message "Fetching PR branch from %s..." remote-name)
    (magit-fetch-branch remote-name head-ref local-branch-name)

    (message "Checking out PR branch: %s" local-branch-name)
    (magit-checkout local-branch-name)

    (message "Checked out PR #%s branch: %s" pr-number local-branch-name)))

(provide 'ghpr-repo)

;;; ghpr-repo.el ends here
