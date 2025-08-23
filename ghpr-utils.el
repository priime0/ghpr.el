;;; ghpr-utils.el --- Utility functions for ghpr -*- lexical-binding: t -*-

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

;; Provides utility functions for ghpr.el.

;;; Code:

(defun ghpr--pr-summary (pr)
  "Formats a PR into a summary."
  (let* ((number (alist-get 'number pr))
         (title (alist-get 'title pr))
         (author (alist-get 'author pr)))
    (format "[#%s] @%s: %s" number author title)))

(defun ghpr--pr-summary-selection (pr)
  "Formats a PR into a summary for a minibuffer selection."
  (cons (ghpr--pr-summary pr) pr))

(provide 'ghpr-utils)

;;; ghpr-utils.el ends here
