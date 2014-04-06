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

(require 'neotree-util)


;;
;; Constants
;;

(defconst neo-buffer-name "*NeoTree*"
  "Name of the buffer where neotree shows directory contents.")

(defconst neo-hidden-files-regexp "^\\."
  "Hidden files regexp. By default all filest starting with dot '.',
including . and ..")


;;
;; Customization
;;

(defgroup neotree-options nil
  "Options for neotree."
  :prefix "neo-"
  :group 'neotree
  :link '(info-link "(neotree)Configuration"))

(defcustom neo-width 25
  "*If non-nil, neo will change its width to this when it show."
  :type 'integer
  :group 'neotree)


;;
;; Faces
;;

(defface neo-header-face
  '((((type tty pc) (class color)) :foreground "lightblue" :weight bold)
    (((background dark)) (:foreground "lightblue" :weight bold))
    (t :foreground "darkblue" :weight bold))
  "*Face used for the header in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-header-face 'neo-header-face)

(defface neo-dir-link-face
  '((((background dark)) (:foreground "DeepSkyBlue"))
    (t                   (:foreground "#8d8d8d")))
  "*Face used for expand sign [+] in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-dir-link-face 'neo-dir-link-face)

(defface neo-file-link-face
  '((((background dark)) (:foreground "White"))
    (t                   (:foreground "#8d8d8d")))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-file-link-face 'neo-file-link-face)


;;
;; Variables
;;

(defvar neo-start-node nil
  "Start node(i.e. directory) for the window.")
(make-variable-buffer-local 'neo-start-node)

(defvar neo-start-line nil
  "Index of the start line - the root")
(make-variable-buffer-local 'neo-start-line)

(defvar neo-show-hidden-nodes nil
  "Show hidden nodes in tree.")
(make-variable-buffer-local 'neo-start-line)

(defvar neo-expanded-nodes-list nil
  "A list of expanded dir nodes.")
(make-variable-buffer-local 'neo-enlarge-window-horizontally)

;;
;; Major mode definitions
;;

(defvar neotree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "\r") 'neo-node-open)
    (define-key map (kbd "SPC") 'neo-node-toggle-expand)
    (define-key map (kbd "TAB") 'neo-node-toggle-expand)
    (define-key map (kbd "g") 'neo-refresh-buffer)
    (define-key map (kbd "p") 'neo-previous-node)
    (define-key map (kbd "n") 'neo-next-node)
    map)
  "Keymap for `neotree-mode'.")

;;;###autoload
(define-derived-mode neotree-mode special-mode "NeoTree"
  "A major mode for displaying the directory tree in text mode."
  ;; only spaces
  (setq indent-tabs-mode nil)
  ;; fix for electric-indent-mode
  ;; for emacs 24.4
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)
    ;; for emacs 24.3 or less
    (add-hook 'electric-indent-functions
              (lambda (arg) 'no-indent) nil 'local)))


;;
;; Privates functions
;;

(defun neo--get-working-dir ()
  (file-name-as-directory (file-truename default-directory)))


(defmacro neo-save-window-excursion (&rest body)
  `(save-window-excursion
     (let ((rlt nil))
       (switch-to-buffer (neo-get-buffer))
       (setq buffer-read-only nil)
       (setf rlt (progn ,@body))
       (setq buffer-read-only t)
       rlt)))


(defmacro neo-save-selected-window (&rest body)
  `(save-selected-window
     (let ((rlt nil))
       (select-window (get-buffer-window
                       (neo-get-buffer)))
       (setq buffer-read-only nil)
       (setf rlt (progn ,@body))
       (setq buffer-read-only t)
       rlt)))


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


(defun neo--create-buffer ()
  (let ((neo-buffer nil))
    (save-excursion
      (split-window-horizontally)
      (setq neo-buffer
            (switch-to-buffer
             (generate-new-buffer-name neo-buffer-name)))
      (neotree-mode)
      (setq buffer-read-only t)
      (delete-window))
    neo-buffer))


(defun neo-get-buffer ()
  (let ((neo-buffer (get-buffer neo-buffer-name)))
    (if (null neo-buffer)
        (neo--create-buffer)
      neo-buffer)))


(defun neo-insert-buffer-header ()
  (let ((start (point)))
    (insert "press ? for neotree help")
    (set-text-properties start (point) '(face neo-header-face)))
  (neo-newline-and-begin))

(defun neo-insert-root-entry (node)
  (neo-newline-and-begin)
  (insert ".. (up a dir)")
  (neo-newline-and-begin)
  (insert node)
  (neo-newline-and-begin))

(defun neo-insert-dir-entry (node depth expanded)
  (let ((btn-start-pos nil)
        (btn-end-pos nil)
        (node-short-name (neo-file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (setq btn-start-pos (point))
    (if expanded
        (insert "▸")
      (insert "▾"))
    (insert (concat " " node-short-name "/"))
    (setq btn-end-pos (point))
    (make-button btn-start-pos
                 btn-end-pos
                 'face neo-dir-link-face
                 'neo-full-path node)
    (neo-newline-and-begin)))

(defun neo-insert-file-entry (node depth)
  (let ((node-short-name (neo-file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (insert-char ?\s 2)
    (insert-button node-short-name
                   'face neo-file-link-face
                   'neo-full-path node)
    (neo-newline-and-begin)))

(defun neo-node-hidden-filter (node)
  (if (not neo-show-hidden-nodes)
      (not (string-match neo-hidden-files-regexp
                         (neo-file-short-name node)))
    node))

(defun neo-walk-dir (path)
  (let* ((nodes (directory-files path))
         (nodes (neo-filter
                 (lambda (node)
                   (if (not (or (equal node ".")
                                (equal node "..")))
                       node
                     nil))
                 nodes)))
    nodes))

(defun neo-get-contents (path)
  (let* ((nodes (neo-walk-dir path))
         (comp  #'(lambda (x y)
                    (string< x y)))
         (nodes (neo-filter 'neo-node-hidden-filter nodes)))
    (cons (sort (neo-filter 'file-directory-p nodes) comp)
          (sort (neo-filter #'(lambda (f) (not (file-directory-p f))) nodes) comp))))

(defun neo-is-expanded-node (node)
  (if (neo-find neo-expanded-nodes-list
                #'(lambda (x) (equal x node)))
      t nil))

(defun neo-expand-set (node do-expand)
  "Set the expanded state of the node to do-expand"
  (if (not do-expand)
      (setq neo-expanded-nodes-list
            (neo-filter
             #'(lambda (x) (not (equal node x)))
             neo-expanded-nodes-list))
    (push node neo-expanded-nodes-list)))

(defun neo-expand-toggle (node)
  (neo-expand-set node (not (neo-is-expanded-node node))))


;; TODO
;; (defun neo-insert-subtree (node depth)
;;   )
  
;; TODO
(defun neo-refresh-buffer (&optional line)
  (interactive)
  (neo-save-selected-window
   (let ((start-node neo-start-node))
     (setq neo-start-line (line-number-at-pos (point)))
     (erase-buffer)
     (neo-insert-buffer-header)
     (neo-insert-root-entry start-node)
     (let* ((contents (neo-get-contents start-node))
            (nodes (car contents))
            (leafs (cdr contents))
            (full-path nil))
       (dolist (node nodes)
         (setq full-path (file-truename node))
         (neo-insert-dir-entry full-path 1
                               (neo-is-expanded-node full-path)))
       (dolist (leaf leafs)
         (setq full-path (file-truename leaf))
         (neo-insert-file-entry full-path 1))
       (neo-scroll-to-line (if line line neo-start-line))))))


;;
;; Public functions
;;

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


;;
;; Interactive functions
;;

(defun neo-previous-node ()
  (interactive)
  (backward-button 1 nil))

(defun neo-next-node ()
  (interactive)
  (forward-button 1 nil))

;; TODO
(defun neo-node-toggle-expand ()
  (interactive)
  (catch 'no-node-button
    (let ((btn (button-at (point)))
          (btn-full-path nil))
      (if (null btn) (throw 'no-node-button nil))
      (setq btn-full-path (button-get btn 'neo-full-path))
      (if (null btn-full-path) (throw 'no-node-button nil))
      (neo-expand-toggle btn-full-path)
      (neo-refresh-buffer)
      (message "full path is %S" btn-full-path))))

;; TODO
(defun neotree-toggle ()
  )

;; TODO
(defun neotree-show ()
  )

;; TODO
(defun neotree-hide ()
  )


;;;###autoload
(defun neotree-dir (path)
  (interactive "DDirectory: ")
  (when (and (file-exists-p path) (file-directory-p path))
    (neo-get-window)
    (neo-save-window-excursion
     (let ((start-path-name (expand-file-name (substitute-in-file-name path))))
       (setq neo-start-node start-path-name))
     (neo-refresh-buffer))))


;;;###autoload
(defun neotree ()
  (interactive)
  (let ((default-directory (neo--get-working-dir)))
    (neotree-dir default-directory)))


(provide 'neotree)
;;; neotree.el ends here
