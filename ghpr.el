 ;;; ghpr.el --- Interacting with GitHub pull requests from Emacs -*- lexical-binding: t -*-

;; Author: Lucas Sta Maria
;; Maintainer: Lucas Sta Maria
;; Version: 0.1
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

(defun ghpr-prs ()
  "List and choose from the current repository's open PRs."
  (interactive)
  (let* ((repo-name (ghpr--get-repo-name))
         (prs (and repo-name (ghpr--list-open-prs repo-name))))
    (cond
     ((not repo-name)
      (message "Not in a GitHub repository"))
     ((not prs)
      (message "No open pull requests found"))
     (t
      (let* ((pr-items (mapcar #'ghpr--pr-summary-selection prs))
             (selected-item (completing-read "Select PR: " pr-items nil t)))
        (when selected-item
          (let ((pr (cdr (assoc selected-item pr-items))))
            (ghpr--open-pr pr repo-name))))))))

(provide 'ghpr)

;;; ghpr.el ends here
