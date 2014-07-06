;;; test-buffer.el --- summary

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; Version: 0

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

(require 'neotree)
(require 'neotree-test)

(ert-deftest neo-test-save-current-pos ()
  (neotree)
  (neo-global--select-window)
  (beginning-of-line)
  (condition-case err
      (while t
        (next-line)
        (neo-buffer--save-cursor-pos)
        (let ((current-file-path (neo-buffer--get-filename-current-line))
              (current-line-number (line-number-at-pos)))
          (should (eq (car neo-buffer--cursor-pos) current-file-path))
          (should (eq (cdr neo-buffer--cursor-pos) current-line-number))))
    (error
     (should (equal err '(end-of-buffer)))))
  (neo-buffer--save-cursor-pos "/tmp/nbs" 192)
  (should (equal neo-buffer--cursor-pos (cons "/tmp/nbs" 192))))

(ert-deftest neo-test-set-node-list ()
  (neotree)
  (neo-global--select-window)
  (neo-buffer--node-list-clear)
  (should (equal neo-buffer--node-list nil))
  (neo-buffer--node-list-set 10 "foo")
  (should (equal neo-buffer--node-list
                 [nil nil nil nil nil nil nil nil nil "foo"]))
  (neo-buffer--node-list-set 3 "bar")
  (neo-buffer--node-list-set 15 "foobar")
  (should (equal neo-buffer--node-list
                 [nil nil "bar" nil nil
                      nil nil nil nil "foo"
                      nil nil nil nil "foobar"]))
  (neo-buffer--node-list-clear)
  (should (equal neo-buffer--node-list nil)))

(ert-deftest neo-test-set-node-list-current-line-number ()
  (neotree)
  (neo-global--select-window)
  (end-of-line)
  (let ((n (line-number-at-pos)))
    (neo-buffer--node-list-set nil "DUMMY")
    (beginning-of-line)
    (should (string= (elt neo-buffer--node-list (1- n)) "DUMMY"))))


(provide 'test-buffer)
;;; test-buffer.el ends here
