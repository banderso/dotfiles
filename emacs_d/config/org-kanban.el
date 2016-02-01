(require 'org-table)

(make-variable-buffer-local
 (defvar org-stored-story-links (list)
   "List of story links"))

(defun org-kanban-element-info ()
  (interactive)
  (message (prin1-to-string (org-element-at-point) t)))

(defun org-kanban-store-story-link ()
  (interactive)
  (let* ((pos (point))
         (story-element (org-element-at-point))
         (ltext (org-element-property :title story-element))
         (update-text (concat "<<" ltext ">>")))
    (push (list "" ltext) org-stored-story-links)
    (kill-region pos (+ pos (length ltext)))
    (insert update-text)))

(defun org-kanban-insert-story-link ()
  (interactive)
  (insert (concat "[[" (cadar org-stored-story-links) "]]")))

(defun org-kanban-move-table-field-right ()
  (interactive)
  (unless (org-at-table-p) (user-error "Not a table"))
  (let ((content (org-table-get-field nil "")))
    (org-table-next-field)
    (insert content)
    (org-table-align)))

(defun org-kanban-move-table-field-left ()
  (interactive)
  (unless (org-at-table-p) (user-error "Not a table"))
  (let ((content (org-table-get-field nil "")))
    (org-table-previous-field)
    (insert content)
    (org-table-align)))

(defvar org-kanban-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-ce" 'org-kanban-element-info)
    (define-key keymap "\C-cs" 'org-kanban-store-story-link)
    (define-key keymap "\C-ci" 'org-kanban-insert-story-link)
    (define-key keymap "\C-c\C-]" 'org-kanban-move-table-field-right)
    (define-key keymap "\C-c\C-[" 'org-kanban-move-table-field-left)
    keymap)
  "Keymap for 'org-kanban-mode'.")

;;;###autoload
(define-minor-mode org-kanban-mode
  "Toggle org-kanban-mode."
  nil
  " Kanban"
  org-kanban-mode-map)

  
(provide 'org-kanban-mode)
