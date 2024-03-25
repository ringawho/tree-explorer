(defface tree-explorer-child-face
  '((t (:inherit font-lock-variable-name-face)))
  "Default item face for tree-explorer.")

(defface tree-explorer-parent-face
  '((t (:inherit font-lock-type-face)))
  "Default face for tree-explorer.")

(defvar-local tree-explorer-type nil)
(define-derived-mode tree-explorer-mode special-mode "Tree Explorer"
  "Show tree to explorer item.")
(defun tree-explorer-open ()
  (interactive)
  (let* ((info (get-text-property (point) 'tree-explorer-info))
         (callback (plist-get info :callback)))
    (if (fboundp callback)
        (funcall callback info)
      (message "No callback function!"))))

(defun tree-explorer-fold ()
  (interactive)
  (when (plist-get (get-text-property (point) 'tree-explorer-info) :status)
    (tree-explorer-open)))

(define-key tree-explorer-mode-map (kbd "RET") #'tree-explorer-open)
(define-key tree-explorer-mode-map (kbd "TAB") #'tree-explorer-fold)


(defun tree-explorer-file-children (info)
  (tree-explorer-list nil (plist-get info :value)))

(defun tree-explorer-tree-end-pos (info)
  (let ((indent (plist-get info :indent)))
    (save-excursion
      (forward-line)
      (while (> (or (plist-get (get-text-property (point) 'tree-explorer-info) :indent)
                    0)
                indent)
        (forward-line))
      (line-beginning-position))))

(defun tree-explorer--fold (info)
  (let ((inhibit-read-only t)
        (children (plist-get info :children))
        (indent (plist-get info :indent))
        (status (plist-get info :status)))
    (when status
      (save-excursion
        (delete-region (line-beginning-position)
                       (tree-explorer-tree-end-pos info))
        (tree-explorer-insert
         (thread-first
           (plist-put info :children
                      (if (functionp children)
                          (funcall children info)
                        children))
           (plist-put :status (if (eq status 'unfold) 'fold 'unfold)))
         indent)))))

(defun tree-explorer-find-file (info)
  (find-file-other-window (plist-get info :value)))

(defun tree-explorer-list (&optional is-root dir)
  (unless dir
    (setq dir default-directory))
  (if is-root
      (list :root t
            :name (abbreviate-file-name default-directory)
            :face 'tree-explorer-parent-face
            :status 'unfold
            :children (tree-explorer-list nil dir))
    (mapcar (lambda (file)
              (let ((expand (expand-file-name file dir)))
                (cond
                 ((string= "." file) (list :name file :callback nil))
                 ((string= ".." file) (list :name file :callback nil))
                 ((file-directory-p expand) (list :name file
                                                  :value expand
                                                  :face 'tree-explorer-parent-face
                                                  :children #'tree-explorer-file-children
                                                  :callback #'tree-explorer--fold
                                                  :status 'fold))
                 (t (list :name file
                          :value expand
                          :callback #'tree-explorer-find-file)))))
            (seq-filter (lambda (item)
                          (not (or (string= item ".") (string= item ".."))))
                        (directory-files (expand-file-name dir))))))

(defun tree-explorer-item-name (info)
  (let ((name (plist-get info :name)))
    (cond
     ((plist-get info :root) name)
     ((eq (plist-get info :status) 'fold) (format "%s %s" "▸" name))
     ((eq (plist-get info :status) 'unfold) (format "%s %s" "▾" name))
     (t (format "%s %s" "⋅" name)))))

(defun tree-explorer-insert (info &optional indent)
  (unless indent
    (setq indent 0))
  (insert (propertize (format "%s%s" (make-string indent ?\s) (tree-explorer-item-name info))
                      'tree-explorer-info (plist-put info :indent indent)
                      'face (or (plist-get info :face)
                                'tree-explorer-child-face))
          "\n")
  (let ((children (plist-get info :children))
        (status (plist-get info :status)))
    (when (and children (eq status 'unfold))
      (dolist (item children)
        (tree-explorer-insert item (+ 2 indent))))))

(defun tree-explorer-update ()
  (let ((inhibit-read-only t))
    (save-excursion
      (erase-buffer)
      (tree-explorer-insert (tree-explorer-list t)))))

(defun tree-explorer-file-revert (&optional _ignore-auto _noconfirm)
  (tree-explorer-update))

(defmacro define-tree-explorer (name)
  (let ((whole-name (format "tree-explorer-%s" name)))
    `(defun ,(intern whole-name) ()
       (interactive)
       (let ((buf (get-buffer-create ,(format "*%s*" whole-name))))
         (set-buffer buf)
         (tree-explorer-mode)
         (setq tree-explorer-type ',name)
         (setq revert-buffer-function ',(intern (format "tree-explorer-%s-revert" name)))
         (revert-buffer)
         (select-window (display-buffer buf))))))

(define-tree-explorer file)

(provide 'tree-explorer)

