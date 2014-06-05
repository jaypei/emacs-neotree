;;; neotree-test.el --- test utilities

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; URL: https://github.com/jaypei/emacs-neotree

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

(defmacro neo-test--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  `(let* ((temp-cwd (file-name-as-directory (make-temp-file "dir" t)))
          (temp-pd (neo-path--join temp-cwd "neo-test" "./")))
     (mkdir temp-pd)
     (unwind-protect
         (let ((default-directory temp-cwd)) ,@body)
       (delete-directory temp-cwd t))))


(provide 'neotree-test)
;;; neotree-test.el ends here
