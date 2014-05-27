;;; neotree.el --- A emacs tree plugin like NerdTree for Vim

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; Version: 0.1.4
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
  '((((background dark)) (:foreground "lightblue" :weight bold))
    (t (:foreground "DarkMagenta")))
  "*Face used for the header in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-header-face 'neo-header-face)

(defface neo-dir-link-face
  '((((background dark)) (:foreground "DeepSkyBlue"))
    (t (:foreground "MediumBlue")))
  "*Face used for expand sign [+] in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-dir-link-face 'neo-dir-link-face)

(defface neo-file-link-face
  '((((background dark)) (:foreground "White"))
    (t (:foreground "Black")))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-file-link-face 'neo-file-link-face)

(defface neo-expand-btn-face
  '((((background dark)) (:foreground "SkyBlue"))
    (t                   (:foreground "DarkCyan")))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-expand-btn-face 'neo-expand-btn-face)

(defface neo-button-face
  '((t (:underline nil)))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-button-face 'neo-button-face)


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
    (define-key map (kbd "SPC") 'neo-node-do-enter)
    (define-key map (kbd "TAB") 'neo-node-do-enter)
    (define-key map (kbd "RET") 'neo-node-do-enter)
    (define-key map (kbd "g") 'neo-refresh-buffer)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    (define-key map (kbd "C-x C-f") 'find-file-other-window)
    (define-key map (kbd "C-c C-c") 'neo-node-do-change-root)
    (define-key map (kbd "C-c C-f") 'find-file-other-window)
    (define-key map (kbd "C-c C-n") 'neo-create-node)
    (define-key map (kbd "C-c C-d") 'neo-delete-current-node)
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
;; internal utility functions
;;

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


(defun neo--init-window ()
  (let ((neo-window nil))
    (select-window (window-at 0 0))
    (split-window-horizontally)
    (switch-to-buffer (neo-get-buffer))
    (if (and (boundp 'linum-mode)
             (not (null linum-mode)))
        (linum-mode -1))
    (setf neo-window (get-buffer-window))
    (select-window (window-right (get-buffer-window)))
    (neo-set-window-width neo-width)
    (set-window-dedicated-p neo-window t)
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
  (insert-button ".."
                 'action '(lambda (x) (neo-node-do-change-root))
                 'follow-link t
                 'face neo-file-link-face
                 'neo-full-path (neo-path-updir neo-start-node))
  (insert " (up a dir)")
  (neo-newline-and-begin)
  (neo-insert-with-face node
                        'neo-header-face)
  (neo-newline-and-begin))


(defun neo-insert-dir-entry (node depth expanded)
  (let ((btn-start-pos nil)
        (btn-end-pos nil)
        (node-short-name (neo-file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (setq btn-start-pos (point))
    (neo-insert-with-face (if expanded "-" "+")
                          'neo-expand-btn-face)
    (neo-insert-with-face (concat " " node-short-name "/")
                          'neo-dir-link-face)
    (setq btn-end-pos (point))
    (make-button btn-start-pos
                 btn-end-pos
                 'action '(lambda (x) (neo-node-do-enter))
                 'follow-link t
                 'face neo-button-face
                 'neo-full-path node)
    (neo-newline-and-begin)))


(defun neo-insert-file-entry (node depth)
  (let ((node-short-name (neo-file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (insert-char ?\s 2)
    (insert-button node-short-name
                   'action '(lambda (x) (neo-node-do-enter))
                   'follow-link t
                   'face neo-file-link-face
                   'neo-full-path node)
    (neo-newline-and-begin)))


(defun neo-node-hidden-filter (node)
  (if (not neo-show-hidden-nodes)
      (not (string-match neo-hidden-files-regexp
                         (neo-file-short-name node)))
    node))


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


(defun neo-insert-dirtree (path depth)
  (if (eq depth 1)
      (neo-insert-root-entry start-node))
  (let* ((contents (neo-get-contents path))
         (nodes (car contents))
         (leafs (cdr contents)))
    (dolist (node nodes)
      (let ((expanded (neo-is-expanded-node node)))
        (neo-insert-dir-entry 
         node depth expanded)
        (if expanded (neo-insert-dirtree (concat node "/") (+ depth 1)))))
    (dolist (leaf leafs)
      (neo-insert-file-entry leaf depth))))

  
(defun neo-refresh-buffer (&optional line)
  (interactive)
  (neo-select-window)
  (let ((start-node neo-start-node)
        (ws-wind (selected-window))
        (ws-pos (window-start)))
    (neo-save-window-excursion
     (setq neo-start-line (line-number-at-pos (point)))
     (erase-buffer)
     (neo-insert-buffer-header)
     (neo-insert-dirtree start-node 1))
    (neo-scroll-to-line
     (if line line neo-start-line)
     ws-wind ws-pos)))


;;
;; Public functions
;;

(defun neo-set-window-width (n)
  (let ((w (max n window-min-width))
        (window (neo-get-window)))
    (save-selected-window
      (select-window window)
      (if (> (window-width) w)
          (shrink-window-horizontally (- (window-width) w))
        (if (< (window-width) w)
            (enlarge-window-horizontally (- w (window-width))))))))


(defun neo-get-current-line-button ()
  (let* ((btn-position nil)
         (pos-line-start (line-beginning-position))
         (pos-line-end (line-end-position))
         ;; NOTE: cannot find button when the button
         ;;       at beginning of the line
         (current-button (or (button-at (point))
                             (button-at pos-line-start))))
    (if (null current-button)
        (progn
          (setf btn-position
                (catch 'ret-button
                  (let* ((next-button (next-button pos-line-start))
                         (pos-btn nil))
                    (if (null next-button) (throw 'ret-button nil))
                    (setf pos-btn (overlay-start next-button))
                    (if (> pos-btn pos-line-end) (throw 'ret-button nil))
                    (throw 'ret-button pos-btn))))
          (if (null btn-position)
              nil
            (setf current-button (button-at btn-position)))))
    current-button))


(defun neo-get-current-line-filename (&optional default)
  (let ((btn (neo-get-current-line-button)))
    (if (not (null btn))
        (button-get btn 'neo-full-path)
      default)))


;;
;; Interactive functions
;;

(defun neo-previous-node ()
  (interactive)
  (backward-button 1 nil))

(defun neo-next-node ()
  (interactive)
  (forward-button 1 nil))


(defun neo-select-window ()
  (interactive)
  (let ((window (neo-get-window)))
    (select-window window)))


(defun neo-node-do-enter ()
  (interactive)
  (neo-select-window)
  (let ((btn-full-path (neo-get-current-line-filename)))
    (when (not (null btn-full-path))
      (if (file-directory-p btn-full-path)
          (progn
            (neo-expand-toggle btn-full-path)
            (neo-refresh-buffer))
        (find-file-other-window btn-full-path)))
    btn-full-path))


(defun neo-node-do-change-root ()
  (interactive)
  (neo-select-window)
  (let ((btn-full-path (neo-get-current-line-filename)))
    (if (null btn-full-path)
        (call-interactively 'neotree-dir)
      (neotree-dir btn-full-path))))


(defun neo-create-node (filename)
  (interactive
   (let* ((current-dir (neo-get-current-line-filename neo-start-node))
          (current-dir (neo-match-path-directory current-dir))
          (filename (read-file-name "Filename:" current-dir)))
     (if (file-directory-p filename)
         (setq filename (concat filename "/")))
     (list filename)))
  (catch 'rlt
    (let ((is-file nil))
      (when (= (length filename) 0)
        (throw 'rlt nil))
      (setq is-file (not (equal (substring filename -1) "/")))
      (when (file-exists-p filename)
        (message "File %S already exists." filename)
        (throw 'rlt nil))
      (when (and is-file
                 (yes-or-no-p (format "Do you want to create file %S ?"
                                      filename)))
        ;; NOTE: create a empty file
        (write-region "" nil filename)
        (neo-refresh-buffer)
        (find-file-other-window filename))
      (when (and (not is-file)
                 (yes-or-no-p (format "Do you want to create directory %S ?"
                                      filename)))
        (mkdir filename)
        (neo-refresh-buffer)))))


(defun neo-delete-current-node ()
  (interactive)
  (catch 'end
    (let ((filename (neo-get-current-line-filename)))
      (if (null filename) (throw 'end nil))
      (if (not (file-exists-p filename)) (throw 'end nil))
      (if (not (yes-or-no-p (format "Do you really want to delete %S ?"
                                    filename)))
          (throw 'end nil))
      (if (file-directory-p filename)
          (progn
            (if (neo-directory-has-file filename)
                (if (yes-or-no-p (format
                                  "%S is directory, delete it by recursive ?"
                                  filename))
                    (delete-directory filename t))
              (delete-directory filename)))
        (delete-file filename))
      (message "Delete successed!")
      (neo-refresh-buffer)
      filename)))


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
  (when (and (file-exists-p path)
             (file-directory-p path))
    (neo-get-window)
    (neo-save-window-excursion
     (let ((start-path-name (expand-file-name (substitute-in-file-name path))))
       (setq neo-start-node start-path-name)
       (cd start-path-name))
     (neo-refresh-buffer))))


;;;###autoload
(defun neotree ()
  (interactive)
  (let ((default-directory (neo--get-working-dir)))
    (neotree-dir default-directory)))


(provide 'neotree)
;;; neotree.el ends here
