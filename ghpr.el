 ;;; ghpr.el --- Interacting with GitHub pull requests from Emacs -*- lexical-binding: t -*-

;; Author: Lucas Sta Maria
;; Maintainer: Lucas Sta Maria
;; Version: 0.2
;; Package-Requires: (magit request)
;; Homepage: https://git.priime.dev/lucas/ghpr.el
;; Keywords: git


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

;; ghpr.el provides capabilities for reviewing GitHub pull requests from the
;; comfort of Emacs. This is the main source package, mainly providing
;; interactive commands for controlling ghpr.

;;; Code:

(require 'ghpr-api)
(require 'ghpr-repo)
(require 'ghpr-utils)
(require 'ghpr-review)
(require 'ghpr-prs)

(defun ghpr-prs ()
  "Display a buffer listing the current repository's open PRs."
  (interactive)
  (let ((repo-name (ghpr--get-repo-name)))
    (cond
     ((not repo-name)
      (message "Not in a GitHub repository"))
     (t
      (ghpr-prs-list repo-name)))))

(defun ghpr-open-pr (pr-number)
  "Open a specific pull request by PR-NUMBER."
  (interactive "nPR number: ")
  (let* ((repo-name (ghpr--get-repo-name))
         (pr (when repo-name (ghpr--get-pr repo-name pr-number))))
    (if pr
        (ghpr--open-pr pr repo-name)
      (message "Could not open PR #%d" pr-number))))

(provide 'ghpr)

;;; ghpr.el ends here
