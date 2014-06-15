;;; neotree.el --- A emacs tree plugin like NerdTree for Vim

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; URL: https://github.com/jaypei/emacs-neotree
;; Version: 0.1.5

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

(defconst neo-buffer-name " *NeoTree*"
  "Name of the buffer where neotree shows directory contents.")

(defconst neo-hidden-files-regexp "^\\."
  "Hidden files regexp.
By default all filest starting with dot '.' including . and ..")


;;
;; Customization
;;

(defgroup neotree-options nil
  "Options for neotree."
  :prefix "neo-"
  :group 'neotree
  :link '(info-link "(neotree)Configuration"))

(defcustom neo-width 25
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)

(defcustom neo-show-header t
  "*If non-nil, a help message will be displayed on the top of the window."
  :type 'boolean
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

(defvar neo-global--buffer nil)

(defvar neo-global--window nil)

(defvar neo-buffer--start-node nil
  "Start node(i.e. directory) for the window.")
(make-variable-buffer-local 'neo-buffer--start-node)

(defvar neo-buffer--start-line nil
  "Index of the start line of the root.")
(make-variable-buffer-local 'neo-buffer--start-line)

(defvar neo-buffer--show-hidden-p nil
  "Show hidden nodes in tree.")
(make-variable-buffer-local 'neo-buffer--show-hidden-p)

(defvar neo-buffer--expanded-node-list nil
  "A list of expanded dir nodes.")
(make-variable-buffer-local 'neo-enlarge-window-horizontally)


;;
;; Major mode definitions
;;

(defvar neotree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC")     'neo-node-do-enter)
    (define-key map (kbd "TAB")     'neo-node-do-enter)
    (define-key map (kbd "RET")     'neo-node-do-enter)
    (define-key map (kbd "g")       'neotree-refresh)
    (define-key map (kbd "p")       'previous-line)
    (define-key map (kbd "n")       'next-line)
    (define-key map (kbd "C-x C-f") 'find-file-other-window)
    (define-key map (kbd "C-c C-c") 'neo-node-do-change-root)
    (define-key map (kbd "C-c C-f") 'find-file-other-window)
    (define-key map (kbd "C-c C-n") 'neo-create-node)
    (define-key map (kbd "C-c C-d") 'neo-delete-current-node)
    map)
  "Keymap for `neotree-mode'.")

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
;; global methods
;;

(defmacro neo-global--with-buffer (&rest body)
  "Execute the forms in BODY with global neotree buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (neo-global--get-buffer)
     ,@body))

(defun neo-global--window-exists-p ()
  "Return non-nil if neotree window exists."
  (and (not (null (window-buffer neo-global--window)))
       (eql (window-buffer neo-global--window) (neo-global--get-buffer))))

(defun neo-global--get-window (&optional auto-create-p)
  "Return the neotree window if it exists, else return nil.
But when the neotree window is not exists and AUTO-CREATE-P is non-nil,
it will be auto create neotree window and return it."
  (unless (neo-global--window-exists-p)
    (setf neo-global--window nil))
  (when (and (null neo-global--window)
             auto-create-p)
    (neo-window--init))
  neo-global--window)

(defun neo-global--get-buffer ()
  "Return the global neotree buffer if it exists."
  (if (not (equal (buffer-name neo-global--buffer)
                  neo-buffer-name))
      (setf neo-global--buffer nil))
  (if (null neo-global--buffer)
      (save-window-excursion
        (neo-buffer--create)))
  neo-global--buffer)

(defadvice delete-other-windows
  (around neotree-delete-other-windows activate)
  "Delete all windows except neotree."
  (interactive)
  (mapcar (lambda (window)
	    (if (not (string-equal (buffer-name (window-buffer window)) neo-buffer-name))
		(delete-window window)))
	  (cdr (window-list))))

(defadvice mouse-drag-vertical-line
  (around neotree-drag-vertical-line (start-event) activate)
  (neo-global--with-buffer
   (neo-buffer--unlock-width))
  ad-do-it
  (neo-global--with-buffer
   (neo-buffer--lock-width))
  ad-return-value)


;;
;; util methods
;;

(defun neo-util--filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun neo-util--find (where which)
  "Find element of the list WHERE matching predicate WHICH."
  (catch 'found
    (dolist (elt where)
      (when (funcall which elt)
        (throw 'found elt)))
    nil))

(defun neo-util--make-printable-string (string)
  "Strip newline character from STRING, like 'Icon\n'."
  (replace-regexp-in-string "\n" "" string))

(defun neo-util--walk-dir (path)
  (let* ((full-path (neo-path--file-truename path)))
    (directory-files path 'full
                     directory-files-no-dot-files-regexp)))

(defun neo-util--hidden-path-filter (node)
  (if (not neo-buffer--show-hidden-p)
      (not (string-match neo-hidden-files-regexp
                         (neo-path--file-short-name node)))
    node))

(defun neo-path--expand-name (path &optional current-dir)
  (or (if (file-name-absolute-p path) path)
      (let ((r-path path))
        (setq r-path (substitute-in-file-name r-path))
        (setq r-path (expand-file-name r-path current-dir))
        r-path)))

(defun neo-path--shorten (path length)
  "Shorten a given path to a specified length. This is needed for paths, which
are to long for the window to display completely. The function cuts of the
first part of the path to remain the last folder (the current one)."
    (if (> (string-width path) length)
	(concat "<" (substring path (- (- length 1))))
      path))

(defun neo-path--updir (path)
  (let ((r-path (neo-path--expand-name path)))
    (if (and (> (length r-path) 0)
             (equal (substring r-path -1) "/"))
        (setq r-path (substring r-path 0 -1)))
    (if (eq (length r-path) 0)
        (setq r-path "/"))
    (directory-file-name
     (file-name-directory r-path))))

(defun neo-path--join (root &rest dirs)
  "Joins a series of directories together with ROOT and DIRS.
Like Python's os.path.join,
  (neo-path--join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c ."
  (or (if (not dirs) root)
      (let ((tdir (car dirs))
            (epath nil))
        (setq epath
              (or (if (equal tdir ".") root)
                  (if (equal tdir "..") (neo-path--updir root))
                  (neo-path--expand-name tdir root)))
        (apply 'neo-path--join
               epath
               (cdr dirs)))))

(defun neo-path--file-short-name (file)
  "Base file/directory name by FILE.
Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (or (if (string= file "/") "/")
      (neo-util--make-printable-string (file-name-nondirectory (directory-file-name file)))))

(defun neo-path--file-truename (path)
  (let ((rlt (file-truename path)))
    (if (not (null rlt))
        (progn
          (if (and (file-directory-p rlt)
                   (> (length rlt) 0)
                   (not (equal (substring rlt -1) "/")))
              (setq rlt (concat rlt "/")))
          rlt)
      nil)))

(defun neo-path--has-subfile-p (dir)
  "To determine whether a directory(DIR) contains files"
  (and (file-exists-p dir)
       (file-directory-p dir)
       (neo-util--walk-dir dir)
       t))

(defun neo-path--match-path-directory (path)
  (let ((true-path (neo-path--file-truename path))
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
        (setq rlt-path (neo-path--join "." rlt-path "./")))
    rlt-path))

(defun neo-path--get-working-dir ()
  (file-name-as-directory (file-truename default-directory)))


;;
;; buffer methods
;;

(defmacro neo-buffer--save-excursion (&rest body)
  `(save-window-excursion
     (let ((rlt nil))
       (switch-to-buffer (neo-global--get-buffer))
       (setq buffer-read-only nil)
       (setf rlt (progn ,@body))
       (setq buffer-read-only t)
       rlt)))

(defun neo-buffer--newline-and-begin ()
  (newline)
  (beginning-of-line))

(defun neo-buffer--scroll-to-line (line &optional wind start-pos)
  "Recommended way to set the cursor to LINE."
  (goto-char (point-min))
  (forward-line (1- line))
  (if start-pos (set-window-start wind start-pos)))

(defun neo-buffer--insert-with-face (content face)
  (let ((pos-start (point)))
    (insert content)
    (set-text-properties pos-start
                         (point)
                         (list 'face face))))

(defun neo-buffer--valid-start-node-p ()
  (and (not (null neo-buffer--start-node))
       (file-accessible-directory-p neo-buffer--start-node)))

(defun neo-buffer--create ()
  (setq neo-global--buffer
        (switch-to-buffer
         (generate-new-buffer-name neo-buffer-name)))
  (neotree-mode)
  (setq buffer-read-only t)
  neo-global--buffer)

(defun neo-buffer--insert-header ()
  (let ((start (point)))
    (insert "Press ? for help.")
    (set-text-properties start (point) '(face neo-header-face)))
  (neo-buffer--newline-and-begin))

(defun neo-buffer--insert-root-entry (node)
  (neo-buffer--newline-and-begin)
  (insert-button ".."
                 'action '(lambda (x) (neo-node-do-change-root))
                 'follow-link t
                 'face neo-file-link-face
                 'neo-full-path (neo-path--updir neo-buffer--start-node))
  (insert " (up a dir)")
  (neo-buffer--newline-and-begin)
  (neo-buffer--insert-with-face (neo-path--shorten node (window-body-width))
                        'neo-header-face)
  (neo-buffer--newline-and-begin))

(defun neo-buffer--insert-dir-entry (node depth expanded)
  (let ((btn-start-pos nil)
        (btn-end-pos nil)
        (node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (setq btn-start-pos (point))
    (neo-buffer--insert-with-face (if expanded "-" "+")
                          'neo-expand-btn-face)
    (neo-buffer--insert-with-face (concat " " node-short-name "/")
                          'neo-dir-link-face)
    (setq btn-end-pos (point))
    (make-button btn-start-pos
                 btn-end-pos
                 'action '(lambda (x) (neo-node-do-enter))
                 'follow-link t
                 'face neo-button-face
                 'neo-full-path node)
    (neo-buffer--newline-and-begin)))

(defun neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (insert-char ?\s 2)
    (insert-button node-short-name
                   'action '(lambda (x) (neo-node-do-enter))
                   'follow-link t
                   'face neo-file-link-face
                   'neo-full-path node)
    (neo-buffer--newline-and-begin)))

(defun neo-buffer--get-nodes (path)
  (let* ((nodes (neo-util--walk-dir path))
         (comp  #'(lambda (x y)
                    (string< x y)))
         (nodes (neo-util--filter 'neo-util--hidden-path-filter nodes)))
    (cons (sort (neo-util--filter 'file-directory-p nodes) comp)
          (sort (neo-util--filter #'(lambda (f) (not (file-directory-p f))) nodes) comp))))

(defun neo-buffer--expanded-node-p (node)
  (if (neo-util--find neo-buffer--expanded-node-list
                #'(lambda (x) (equal x node)))
      t nil))

(defun neo-buffer--set-expand (node do-expand)
  "Set the expanded state of the node to do-expand"
  (if (not do-expand)
      (setq neo-buffer--expanded-node-list
            (neo-util--filter
             #'(lambda (x) (not (equal node x)))
             neo-buffer--expanded-node-list))
    (push node neo-buffer--expanded-node-list)))

(defun neo-buffer--toggle-expand (node)
  (neo-buffer--set-expand node (not (neo-buffer--expanded-node-p node))))

(defun neo-buffer--insert-tree (path depth)
  (if (eq depth 1)
      (neo-buffer--insert-root-entry start-node))
  (let* ((contents (neo-buffer--get-nodes path))
         (nodes (car contents))
         (leafs (cdr contents)))
    (dolist (node nodes)
      (let ((expanded (neo-buffer--expanded-node-p node)))
        (neo-buffer--insert-dir-entry
         node depth expanded)
        (if expanded (neo-buffer--insert-tree (concat node "/") (+ depth 1)))))
    (dolist (leaf leafs)
      (neo-buffer--insert-file-entry leaf depth))))

(defun neo-buffer--refresh (&optional line)
  (interactive)
  (neo-window--select)
  (let ((start-node neo-buffer--start-node)
        (ws-wind (selected-window))
        (ws-pos (window-start)))
    (neo-buffer--save-excursion
     (setq neo-buffer--start-line (line-number-at-pos (point)))
     (erase-buffer)
     (if neo-show-header (neo-buffer--insert-header))
     (neo-buffer--insert-tree start-node 1))
    (neo-buffer--scroll-to-line
     (if line line neo-buffer--start-line)
     ws-wind ws-pos)))

(defun neo-buffer--get-button-current-line ()
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

(defun neo-buffer--get-filename-current-line (&optional default)
  (let ((btn (neo-buffer--get-button-current-line)))
    (if (not (null btn))
        (button-get btn 'neo-full-path)
      default)))

(defun neo-buffer--lock-width ()
  (setq window-size-fixed 'width))

(defun neo-buffer--unlock-width ()
  (setq window-size-fixed nil))


;;
;; window methods
;;

(defun neo-window--init ()
  "Create neotree window."
  (select-window (window-at 0 0))
  (split-window-horizontally)
  (neo-global--with-buffer
   (neo-buffer--unlock-width))
  (switch-to-buffer (neo-global--get-buffer))
  (if (and (boundp 'linum-mode)         ; disable line number
           (not (null linum-mode)))
      (linum-mode -1))
  (setq truncate-lines -1)
  (setf neo-global--window (get-buffer-window))
  (neo-window--set-width neo-width)
  (set-window-dedicated-p neo-global--window t)
  (neo-global--with-buffer
   (neo-buffer--lock-width))
  (select-window (window-right (get-buffer-window)))
  neo-global--window)

(defun neo-window--set-width (n)
  "Make neotree widnow N columns width."
  (let ((w (max n window-min-width))
        (window (neo-global--get-window)))
    (unless (null window)
      (if (> (window-width) w)
          (shrink-window-horizontally (- (window-width) w))
        (if (< (window-width) w)
            (enlarge-window-horizontally (- w (window-width))))))))

(defun neo-window--select ()
  (interactive)
  (let ((window (neo-global--get-window t)))
    (select-window window)))


;;
;; Interactive functions
;;

(defun neo-previous-node ()
  (interactive)
  (backward-button 1 nil))

(defun neo-next-node ()
  (interactive)
  (forward-button 1 nil))

(defun neo-node-do-enter ()
  (interactive)
  ;(neo-window--select)
  (let ((btn-full-path (neo-buffer--get-filename-current-line)))
    (unless (null btn-full-path)
      (if (file-directory-p btn-full-path)
          (progn
            (neo-buffer--toggle-expand btn-full-path)
            (neo-buffer--refresh))
	(progn
	  (switch-to-buffer (other-buffer (current-buffer) 1))
	 (find-file btn-full-path))))
    btn-full-path))

(defun neo-node-do-change-root ()
  (interactive)
  (neo-window--select)
  (let ((btn-full-path (neo-buffer--get-filename-current-line)))
    (if (null btn-full-path)
        (call-interactively 'neotree-dir)
      (neotree-dir btn-full-path))))

(defun neo-create-node (filename)
  (interactive
   (let* ((current-dir (neo-buffer--get-filename-current-line neo-buffer--start-node))
          (current-dir (neo-path--match-path-directory current-dir))
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
        (neo-buffer--refresh)
        (find-file-other-window filename))
      (when (and (not is-file)
                 (yes-or-no-p (format "Do you want to create directory %S?"
                                      filename)))
        (mkdir filename)
        (neo-buffer--refresh)))))

(defun neo-delete-current-node ()
  (interactive)
  (catch 'end
    (let ((filename (neo-buffer--get-filename-current-line)))
      (if (null filename) (throw 'end nil))
      (if (not (file-exists-p filename)) (throw 'end nil))
      (if (not (yes-or-no-p (format "Do you really want to delete %S?"
                                    filename)))
          (throw 'end nil))
      (if (file-directory-p filename)
          (progn
            (if (neo-path--has-subfile-p filename)
                (if (yes-or-no-p (format
                                  "%S is a directory, delete it recursively?"
                                  filename))
                    (delete-directory filename t))
              (delete-directory filename)))
        (delete-file filename))
      (message "%S deleted." filename)
      (neo-buffer--refresh)
      filename)))

(defun neotree-refresh ()
  (interactive)
  (neo-buffer--refresh))

;;;###autoload
(defun neotree-toggle ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-show)))

;;;###autoload
(defun neotree-show ()
  (interactive)
  (let ((valid-start-node-p nil))
    (neo-buffer--save-excursion
     (setf valid-start-node-p (neo-buffer--valid-start-node-p)))
    (if (not valid-start-node-p)
        (neotree-dir (neo-path--get-working-dir))
      (neo-global--get-window t))))

;;;###autoload
(defun neotree-hide ()
  (interactive)
  (if (neo-global--window-exists-p)
      (delete-window neo-global--window)))

;;;###autoload
(defun neotree-dir (path)
  (interactive "DDirectory: ")
  (when (and (file-exists-p path)
             (file-directory-p path))
    (neo-global--get-window t)
    (neo-buffer--save-excursion
     (let ((start-path-name (expand-file-name (substitute-in-file-name path))))
       (setq neo-buffer--start-node start-path-name)
       (cd start-path-name))
     (neo-buffer--refresh))))

;;;###autoload
(defun neotree ()
  (interactive)
  (neotree-show))


(provide 'neotree)
;;; neotree.el ends here
