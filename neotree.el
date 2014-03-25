;;; neotree.el --- summary

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

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/neotree/")
;; (require 'neotree)
;;
;; Type M-x neotree to start.
;;
;; To set options for NeoTree, type M-x customize, then select
;; Applications, NeoTree.
;;

;;; Code:

(defconst neo-buffer-name "*NeoTree*"
  "Name of the buffer where neotree shows directory contents.")

;; Customization
(defgroup neotree-options nil
  "Options for neotree."
  :prefix "neo-"
  :group 'neotree
  :link '(info-link "(neotree)Configuration"))

(defun neo--init-window ()
  (save-selected-window
    (select-window (window-at 0 0))
    (split-window-horizontally)))


(defun neo--get-working-dir ()
  (file-name-as-directory (file-truename default-directory)))

(defun neo--create-buffer ()
  (let ((neo-buffer nil))
    (save-excursion
      (split-window-horizontally)
      (setq neo-buffer
            (switch-to-buffer
             (generate-new-buffer-name neo-buffer-name)))
      (delete-window))
    neo-buffer))

(defmacro neo-save-window-excursion (&rest body)
  `(save-window-excursion
     (switch-to-buffer (neo-get-buffer))
     (end-of-buffer)
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only t)))

;;;###autoload
(defun neo-get-buffer ()
  (let ((neo-buffer (get-buffer neo-buffer-name)))
    (if (null neo-buffer)
        (neo--create-buffer)
      neo-buffer)))

;;;###autoload
(defun neotree ()
  (interactive)
  (let ((default-directory (neo--get-working-dir)))
    ))

(provide 'neotree)
;;; neotree.el ends here
