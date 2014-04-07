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

(defun neo-find (where which)
  "find element of the list `where` matching predicate `which`"
  (catch 'found
    (dolist (elt where)
      (when (funcall which elt)
        (throw 'found elt)))
    nil))

(defun neo-newline-and-begin ()
  (newline)
  (beginning-of-line))

(defun neo-scroll-to-line (line &optional wind start-pos)
  "Recommended way to set the cursor to specified line"
  (goto-char (point-min))
  (forward-line (1- line))
  (if start-pos (set-window-start wind start-pos)))

(defun neo-file-short-name (file)
  "Base file/directory name. Taken from
 http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (neo-printable-string (file-name-nondirectory (directory-file-name file))))

(defun neo-printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (replace-regexp-in-string "\n" "" string))

(defun neo-insert-with-face (content face)
  (let ((pos-start (point)))
    (insert content)
    (set-text-properties pos-start
                         (point)
                         (list 'face face))))
    

(provide 'neotree-util)
;;; neotree-util.el ends here
