;;; test-utils.el --- test cases

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

(require 'neotree)
(require 'neotree-test)


(ert-deftest neo-test-filter ()
  (let ((should-equal (lambda (condp lst y)
                        (should (equal (neo-util--filter condp lst) y)))))
    (apply should-equal (list (lambda (x) (> x 10))
                              '(1 2 8 12 30)
                              '(12 30)))
    (apply should-equal (list (lambda (x) nil)
                              '(1 2 8 12 30)
                              nil))
    (apply should-equal (list (lambda (x) t)
                              '(1 2 8 12 30)
                              '(1 2 8 12 30)))
    (apply should-equal (list 'integerp
                              '(1 "foo" 2 8 "bar" 12 30)
                              '(1 2 8 12 30)))
    (apply should-equal (list 'integerp
                              '(1 "foo" "bar")
                              '(1)))
    (apply should-equal (list 'integerp
                              '("foo" "bar" 30)
                              '(30)))
    (apply should-equal (list (lambda (x) (and (not (string= x "."))
                                               (not (string= x ".."))))
                              '("." ".." ".neotree/" "otherfiles")
                              '(".neotree/" "otherfiles")))))


(ert-deftest neo-test-find ()
  (let ((should-equal (lambda (where which y)
                        (should (equal (neo-util--find where which) y)))))
    (apply should-equal (list '("hello" 1 "world") 'integerp 1))
    (apply should-equal (list '("hello" 1 "world") 'stringp "hello"))
    (apply should-equal (list '("hello" "world" 100000) 'integerp 100000))))


(ert-deftest neo-test-newline-and-begin ()
  (with-temp-buffer
    (neo-buffer--newline-and-begin)))


(ert-deftest neo-test-file-short-name ()
  (let ((should-equal (lambda (x y)
                        (should (string= (neo-path--file-short-name x) y)))))
    (apply should-equal '("~/" "~"))
    (apply should-equal '("/" "/"))
    (apply should-equal '("~/." "."))
    (apply should-equal '("afile" "afile"))
    (apply should-equal '("a/" "a"))
    (apply should-equal '("" ""))
    (apply should-equal '("~/abc.org" "abc.org"))
    (apply should-equal '("/home/q/hello/world/" "world"))
    (apply should-equal '("/home/q/hello/world/.abc" ".abc"))))


(ert-deftest neo-test-insert-with-face ()
  (with-temp-buffer
    (insert "foo")
    (neo-buffer--insert-with-face "ButtonContent" 'default)
    (insert "bar")
    (goto-char 4)
    (should (eq (face-at-point) 'default))
    (should (string= (buffer-string) "fooButtonContentbar"))))

(ert-deftest neo-test-strip-path ()
  (setq cases '((" /" . "/")
                ("\n/" . "/")
                ("\n \t/" . "/")
                ("\n \t~/abc.org " . "~/abc.org")))
  (mapc
   (lambda (x) (should (equal (neo-path--strip (car x))
                              (cdr x))))
   cases))

(ert-deftest neo-test-file-equal-p ()
  (let ((fn 'neo-path--file-equal-p)
        (cases '((("/etc/passwd" "/etc/passwd") . t)
                 (("/tmp/no-exists-file" "/tmp/no-exists-file") . nil)
                 ((nil "/etc/passwd") . nil)
                 (("/etc/passwd" nil) . nil))))
    (mapcar (lambda (x) (should (eq (apply fn (car x)) (cdr x)))) cases)))


;;; test-utils.el ends here
