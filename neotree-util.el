;;; neotree-util.el --- summary

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun neo-filter (condp lst)
    (delq nil
          (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun neo-newline-and-begin ()
  (newline)
  (beginning-of-line))

(defun neo-scroll-to-line (line)
  "Recommended way to set the cursor to specified line"
  (goto-char (point-min))
  (forward-line (1- line)))

(defun neo-file-short-name (file)
  "Base file/directory name. Taken from
 http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (neo-printable-string (file-name-nondirectory (directory-file-name file))))

(defun neo-printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (replace-regexp-in-string "\n" "" string))  

(provide 'neotree-util)
;;; neotree-util.el ends here
