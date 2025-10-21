;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with ‘C-x C-f’ and enter text in its buffer.

(defun img/dired-sp-convert-to-gif-2 ()
 "?"
 (interactive)
 (let* ((directory (dired-current-directory))
	(filename (dired-get-filename 'no-dir))
	(basename (file-name-base filename))
	(cmd-buffer (generate-new-buffer "*Convert to gif*")))
   (display-buffer cmd-buffer '((display-buffer-pop-up-window)))
   (select-window (get-buffer-window  cmd-buffer))
   (txt/insert-sp-text-comment "# C-c C-c to execute\n# C-c C-k to cancel\n\n")
   (txt/insert-sp-labelled-line
    `(("Directory: " . ,directory)
      ("Input file: " . ,filename)
      ("Output file: " . ,(format "%s.gif" basename))
      ("Start: " . "00:00:00")
      ("End: " . "00:00:00")
      ("Dither mode [bayer/none]: " . "bayer")
      ("FPS [10/15/20/25]: " . "15")))
   (sp/define-dither-field (sp/find-regex-bounds "bayer$"))
   (goto-line 7)
   (goto-char (line-end-position))
   ))

(defvar sp/dither-field-keymap
  (let ((map (make-sparse-keymap)))
    ;; Enter (RET) triggers the completing-read function
    (define-key map (kbd "RET") 'txt/dither-field-complete)
    map)
  "Keymap active when cursor is in a sp/field.")

(defun sp/define-dither-field (bounds)
  "Make region START to END a field with RET keymap."
  (let ((start (car bounds))
	(end   (cdr bounds)))
    (put-text-property start end 'sp/dither-field t)
    (put-text-property start end 'keymap sp/dither-field-keymap)
    (put-text-property start end 'read-only nil)
    (put-text-property start end 'face 'highlight)))

(defun txt/dither-field-complete ()
  "Prompt for a value with completing-read and replace field contents."
  (interactive)
  (let* ((field-bounds (sp/get-dither-field-bounds))
         (value (completing-read "Choose: " '("bayer" "none"))))
    (when field-bounds
      (delete-region (car field-bounds) (cdr field-bounds))
      (let ((start (point)))
	(insert value)
	(sp/define-dither-field (cons start (point)))))))

(defun sp/get-dither-field-bounds ()
  "Return (START . END) of the field under point, or nil."
  (let ((start (previous-single-property-change (point) 'sp/dither-field))
        (end (next-single-property-change (point) 'sp/dither-field)))
    (when (get-text-property (point) 'sp/dither-field)
      (cons (or start (point-min))
            (or end (point-max))))))

(defun txt/insert-sp-text-comment (txt)
  "Insert text propertized with the `font-lock-comment-face'"
  (insert (propertize txt 'face 'font-lock-comment-face)))


(defun txt/insert-sp-labelled-line (elems)
  (dolist (pair elems)
    (let ((key (car pair))
	  (value (cdr pair)))
      (insert
       (propertize (format "%-25s" key) 'face 'font-lock-keyword-face)
       (propertize value 'face 'font-lock-string-face)
       "\n"))))

(defun sp/find-regex-bounds (regex &optional start end)
  "Search for REGEX in the current buffer between START and END.
Return a cons cell (START . END) of the first match, or nil if not found."
  (save-excursion
    (goto-char (or start (point-min)))
    (when (re-search-forward regex end t)
      (cons (match-beginning 0) (match-end 0)))))

