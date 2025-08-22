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

(provide 'ghpr-repo)

;;; ghpr-repo.el ends here
