;;;; grep.el
;;;;
;;;; Intended to replicate some basic grep functionality.
;;;;
;;;; Mainly this is for learning basic file system interaction with Emacs.

(defun lame-grep (file-path search-term)
  "Search the file give by 'file-path' for 'search-term', and
return the number of instances of that search term."
  (let ((count 0))
    (with-temp-buffer
      (insert-file-contents file-path)
      (goto-char 1)  ;; Needed in case the file is already open.
      (while (search-forward search-term nil t)
        (setq count (1+ count))))
    ;; Return count of occurrences.
    count))

;; A better implementation is probably to remove any line from the
;; temp buffer which does NOT match the search term. Then append to
;; the original buffer the entire contents of the temp buffer.
(defun better-grep (file-path search-term)
  "Insert lines containing matches for 'search-term' into the
  current buffer."
  (let ((original-buffer (current-buffer)))
    (with-temp-buffer
      (setq temp-buffer (current-buffer))
      (insert-file-contents file-path)
      (goto-char 1)
      (while (search-forward search-term nil t)
        ;; Known bug: prints out same line multiple times if search
        ;; term appears multiple times.
        (let ((p1 (line-beginning-position)) (p2 (line-end-position)))
          (save-excursion
            (switch-to-buffer original-buffer)
            (insert-buffer-substring temp-buffer p1 p2)
            (insert "\n")))))))
