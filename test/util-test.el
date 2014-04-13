;;; util-test.el --- summary

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

(require 'neotree-util)


(ert-deftest neo-test-filter ()
  (should (equal (neo-filter (lambda (x) (> x 10)) '(1 2 8 12 30))
                 '(12 30)))
  (should (null (neo-filter (lambda (x) nil) '(1 2 8 12 30))))
  (should (equal (neo-filter (lambda (x) t) '(1 2 8 12 30))
                 '(1 2 8 12 30)))
  (should (equal (neo-filter (lambda (x) (and (not (equal x "."))
                                              (not (equal x ".."))))
                             '("." ".." ".neotree/" "otherfiles"))
                 '(".neotree/" "otherfiles"))))


(ert-deftest neo-test-find ()
  (should (equal (neo-find '("hello" 1 "world") 'integerp) 1)))

(ert-deftest neo-test-newline-and-begin ()
  (with-temp-buffer
    (neo-newline-and-begin)))


;;; util-test.el ends here
