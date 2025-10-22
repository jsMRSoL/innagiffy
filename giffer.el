;;; giffer.el --- Clip gifs from video files  -*- lexical-binding: t -*-

(require 'sp-form-funcs)
(require 'sp-buffer-utils)

;; setup initial window
(defun img/dired-sp-convert-to-gif-windowed ()
  "Open a buffer to set arguments to be passed to ffmpeg to convert
   part of a video file to a high-quality gif."
  (interactive)
  (let* ((directory (dired-current-directory))
	 (filename (dired-get-filename 'no-dir))
	 (basename (file-name-base filename))
	 (cmd-buffer (generate-new-buffer "*Convert to gif*"))
	 (lines `(("Directory: " . ,directory)
		  ("Input file: " . ,filename)
		  ("Output file: " . ,(format "%s.gif" basename))
		  ("Start: " . "00:00:00")
		  ("End: " . "00:00:00")
		  ("Dither mode: " . "bayer")
		  ("FPS: " . "15"))))

    ;; Create buffer and write text
    (display-buffer cmd-buffer '((display-buffer-below-selected)))
    (select-window (get-buffer-window  cmd-buffer))
    (txt/insert-sp-text-comment "# C-c C-c to execute\n# C-c C-k to cancel\n\n")
    (txt/insert-sp-labelled-line lines "%-14s")

    ;; define fields
    (dolist (pair `(("bayer$" . (gif-dither-field . ,gif-dither-field-keymap))
		    ("15$" . (gif-fps-field . ,gif-fps-field-keymap))))
      (let* ((rgx (car pair))
	     (args (cdr pair))
	     (field-sym (car args))
	     (keymap (cdr args)))
	(sp/define-field
	 (sp/find-regex-bounds rgx) field-sym keymap)))

    ;; Set line headings as read only
    (dolist (line-pair lines)
      (let* ((rgx (car line-pair)))
	(sp/set-rgx-range-readonly rgx)))

    ;; finish setup
    (goto-line 6)
    (goto-char (- (line-end-position) 4))
    (undo-tree-mode 1)
    (local-set-key (kbd "C-c C-c") #'gif-compose-cmd-from-buffer-lines)
    (local-set-key (kbd "C-c C-k") #'kill-buffer-and-window)))

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

(defun gif-compose-cmd-from-buffer-lines ()
  (interactive)
  (let* ((directory (sp/collect-arg-from-buffer "Directory: "))
	(input-file (sp/collect-arg-from-buffer "Input file: "))
	(output-file (sp/collect-arg-from-buffer "Output file: "))
	(start-time (sp/collect-arg-from-buffer "Start: "))
	(end-time (sp/collect-arg-from-buffer "End: "))
	(dither-mode (sp/collect-arg-from-buffer "Dither mode: "))
	(fps (sp/collect-arg-from-buffer "FPS: "))
	(palette-file "/tmp/palette%02d.png")
	(cmd (concat
	      "ffmpeg -ss " start-time
	      " -to " end-time
	      " -i " input-file
	      " -vf \"fps=" fps
	      ",scale=480:-1:flags=lanczos,palettegen=stats_mode=diff\""
	      " -y " palette-file "; "
	      "ffmpeg -ss " start-time
	      " -to " end-time
	      " -i " input-file
	      " -i " palette-file
	      " -lavfi \"fps=" fps
	      ",scale=480:-1:flags=lanczos [x]; [x][1:v] paletteuse=dither=" dither-mode
	      "\" -loop 0 -y " output-file "; "
	      "rm -vf /tmp/palette*.png")))
    (message "%s %s %s %s %s %s %s" directory input-file output-file start-time end-time dither-mode fps)
    (start-process "convert-to-gif" "*ffmpeg conversion*" "bash" "-c" cmd)
    (kill-buffer-and-window)))


(provide 'giffer)

;;; giffer.el ends here
