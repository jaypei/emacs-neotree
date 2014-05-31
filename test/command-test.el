;;; command-test.el --- summary

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

(defadvice window-at (around neo-test-neotree-startup activate)
  ad-do-it
  (setq ad-return-value (selected-window))
  ad-return-value)

(ert-deftest neo-test-neotree-startup ()
  (with-temp-buffer
    (neotree)
    (should (neo-window-exists-p))))

(ert-deftest neo-test-neotree-toggle ()
  (with-temp-buffer
    (neotree-toggle)
    (should (neo-window-exists-p))))

;;; command-test.el ends here
