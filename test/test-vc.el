;;; test-vc.el --- test cases

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

(neo-test--try-open
 neo-test-vc-mode-with-face
 (shell-command-to-string "git init")
 (setq neo-vc-integration '(face)))

(neo-test--try-open
 neo-test-vc-mode-with-char
 (shell-command-to-string "git init")
 (setq neo-vc-integration '(char)))

(neo-test--try-open
 neo-test-vc-mode-with-char-face
 (shell-command-to-string "git init")
 (setq neo-vc-integration '(char face)))


;;; test-vc.el ends here
