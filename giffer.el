;;; giffer.el --- Clip gifs from video files  -*- lexical-binding: t -*-

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
      ("Dither mode: " . "bayer")
      ("FPS: " . "15")))
   (sp/define-field
    (sp/find-regex-bounds "bayer$")
    'gif-dither-field
    gif-dither-field-keymap)
   (sp/define-field
    (sp/find-regex-bounds "15$")
    'gif-fps-field
    gif-fps-field-keymap)
   (goto-line 7)
   (goto-char (line-end-position))
   (local-set-key (kbd "C-c C-c") #'gif-compose-cmd-from-buffer-lines)
   (local-set-key (kbd "C-c C-k") #'sp/kill-this-buffer)))

(defun sp/define-field (bounds field-sym keymap)
  "Make region START to END a field with RET keymap."
  (let ((start (car bounds))
	(end   (cdr bounds)))
    (put-text-property start end field-sym t)
    (put-text-property start end 'keymap keymap)
    (put-text-property start end 'read-only nil)
    (put-text-property start end 'face 'highlight)))

(defun sp/get-field-bounds (field-sym)
  "Return (START . END) of the field under point, or nil."
  (interactive)
  (let ((start (previous-single-property-change (point) field-sym))
        (end (next-single-property-change (point) field-sym)))
    (when (get-text-property (point) field-sym)
      (cons (or start (point-min))
            (or end (point-max))))))

(defun sp/find-regex-bounds (regex &optional start end)
  "Search for REGEX in the current buffer between START and END.
Return a cons cell (START . END) of the first match, or nil if not found."
  (save-excursion
    (goto-char (or start (point-min)))
    (when (re-search-forward regex end t)
      (cons (match-beginning 0) (match-end 0)))))

(defun gif-dither-field-complete ()
  "Prompt for a value with completing-read and replace field contents."
  (interactive)
  (let* ((field-bounds (sp/get-field-bounds 'gif-dither-field))
	 (value (completing-read "Choose: " '("bayer" "none")))
	 (start (car field-bounds))
	 (end (cdr field-bounds))
	 (length (- end start)))
    (goto-char start)
    (insert value)
    (sp/define-field (cons start (point)) 'gif-dither-field gif-dither-field-keymap)
    (kill-line nil)))

(defvar gif-dither-field-keymap
  (let ((map (make-sparse-keymap)))
    ;; Enter (RET) triggers the completing-read function
    (define-key map (kbd "RET") 'gif-dither-field-complete)
    map)
  "Keymap active when cursor is in a gif-dither-field.")

(defun gif-fps-field-complete ()
  "Prompt for a value with completing-read and replace field contents."
  (interactive)
  (let* ((field-bounds (sp/get-field-bounds 'gif-fps-field))
	 (value (completing-read "Choose: " '("10" "15" "20" "25")))
	 (start (car field-bounds))
	 (end (cdr field-bounds)))
    (goto-char start)
    (insert value)
    (sp/define-field (cons start (point)) 'gif-fps-field gif-fps-field-keymap)
    (kill-line nil)))

(defvar gif-fps-field-keymap
  (let ((map (make-sparse-keymap)))
    ;; Enter (RET) triggers the completing-read function
    (define-key map (kbd "RET") 'gif-fps-field-complete)
    map)
  "Keymap active when cursor is in a gif-fps-field.")

(defun txt/insert-sp-text-comment (txt)
  "Insert text propertized with the `font-lock-comment-face'"
  (insert (propertize txt 'face 'font-lock-comment-face)))


(defun txt/insert-sp-labelled-line (elems)
  (dolist (pair elems)
    (let ((key (car pair))
	  (value (cdr pair)))
      (insert
       (propertize (format "%-14s" key) 'face 'font-lock-keyword-face)
       (propertize value 'face 'font-lock-string-face)
       "\n"))))


(defun sp/collect-arg-from-buffer (prefix)
  "Return the captured part of the first line starting with PREFIX, or nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^" (regexp-quote prefix) "\\s-*\\(.*\\)$") nil t)
      (match-string-no-properties 1))))

(defun gif-compose-cmd-from-buffer-lines ()
  (interactive)
  (let ((directory (sp/collect-arg-from-buffer "Directory: "))
	(input-file (sp/collect-arg-from-buffer "Input file: "))
	(output-file (sp/collect-arg-from-buffer "Output file: "))
	(start-time (sp/collect-arg-from-buffer "Start: "))
	(end-time (sp/collect-arg-from-buffer "End: "))
	(dither-mode (sp/collect-arg-from-buffer "Dither mode: "))
	(fps (sp/collect-arg-from-buffer "FPS: ")))
    (message "%s %s %s %s %s %s %s" directory input-file output-file start-time end-time dither-mode fps)))


(defun sp/kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))


(provide 'giffer)

;;; giffer.el ends here
