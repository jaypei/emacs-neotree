;;; neotree-util.el --- summary

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>

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
  (or (if (string= file "/") "/")
      (neo-printable-string (file-name-nondirectory (directory-file-name file)))))

(defun neo-printable-string (string)
  "Strip newline character from file names, like 'Icon\n'"
  (replace-regexp-in-string "\n" "" string))

(defun neo-insert-with-face (content face)
  (let ((pos-start (point)))
    (insert content)
    (set-text-properties pos-start
                         (point)
                         (list 'face face))))


(defun neo-file-truename (path)
  (let ((rlt (file-truename path)))
    (if (not (null rlt))
        (progn
          (if (and (file-directory-p rlt)
                   (> (length rlt) 0)
                   (not (equal (substring rlt -1) "/")))
              (setq rlt (concat rlt "/")))
          rlt)
      nil)))


(defun neo-path-expand-name (path &optional current-dir)
  (or (if (file-name-absolute-p path) path)
      (let ((r-path path))
        (setq r-path (substitute-in-file-name r-path))
        (setq r-path (expand-file-name r-path current-dir))
        r-path)))
  

(defun neo-path-join (root &rest dirs)
  "Joins a series of directories together, like Python's os.path.join,
  (neo-path-join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c"
  (or (if (not dirs) root)
      (let ((tdir (car dirs))
            (epath nil))
        (setq epath
              (or (if (equal tdir ".") root)
                  (if (equal tdir "..") (neo-path-updir root))
                  (neo-path-expand-name tdir root)))
        (apply 'neo-path-join
               epath
               (cdr dirs)))))


(defun neo-path-updir (path)
  (let ((r-path (neo-path-expand-name path)))
    (if (and (> (length r-path) 0)
             (equal (substring r-path -1) "/"))
        (setq r-path (substring r-path 0 -1)))
    (if (eq (length r-path) 0)
        (setq r-path "/"))
    (directory-file-name
     (file-name-directory r-path))))


(defun neo-walk-dir (path)
  (let* ((full-path (neo-file-truename path)))
    (directory-files path 'full
                     directory-files-no-dot-files-regexp)))


(defun neo-directory-has-file (dir)
  "To determine whether a directory(DIR) contains files"
  (and (file-exists-p dir)
       (file-directory-p dir)
       (neo-walk-dir dir)
       t))


(defun neo-match-path-directory (path)
  (let ((true-path (neo-file-truename path))
        (rlt-path nil))
    (setq rlt-path
          (catch 'rlt
            (if (file-directory-p true-path)
                (throw 'rlt true-path))
            (setq true-path
                  (file-name-directory true-path))
            (if (file-directory-p true-path)
                (throw 'rlt true-path))))
    (if (not (null rlt-path))
        (setq rlt-path (neo-path-join "." rlt-path "./")))
    rlt-path))

      
(provide 'neotree-util)
;;; neotree-util.el ends here
