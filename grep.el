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
      (goto-char 1)  ;; Will have to see if this is necessary.
      (while (search-forward search-term nil t)
        (setq count (1+ count))))
    ;; Return count of occurrences.
    count))
