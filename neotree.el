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

(defcustom neo-width 25
  "*If non-nil, neo will change its width to this when it show."
  :type 'integer
  :group 'neotree)


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

(defun neo-shrink-window-horizontally (delta)
  (neo-save-selected-window
   (shrink-window-horizontally delta)))

(defun neo-enlarge-window-horizontally (delta)
  (neo-save-selected-window
   (enlarge-window-horizontally delta)))

(defun neo-set-window-width (n)
  (let ((w (max n window-min-width)))
    (neo-save-selected-window
     (if (> (window-width) w)
           (shrink-window-horizontally (- (window-width) w))
       (if (< (window-width) w)
           (enlarge-window-horizontally (- w (window-width))))))))

(defun neo-show ()
  (neo--init-window))

(defmacro neo-save-window-excursion (&rest body)
  `(save-window-excursion
     (switch-to-buffer (neo-get-buffer))
     (end-of-buffer)
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only t)))

(defmacro neo-save-selected-window (&rest body)
  `(save-selected-window
     (select-window (get-buffer-window
                     (neo-get-buffer)))
     (end-of-buffer)
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only t)))


(defun neo--init-window ()
  (let ((neo-window nil))
    (select-window (window-at 0 0))
    (split-window-horizontally)
    (switch-to-buffer (neo-get-buffer))
    (setf neo-window (get-buffer-window))
    (select-window (window-right (get-buffer-window)))
    (neo-set-window-width neo-width)
    neo-window))

(defun neo-get-window ()
  (let* ((buffer (neo-get-buffer))
         (window (get-buffer-window buffer)))
    (if (not window)
        (setf window (neo--init-window)))
    window))

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
