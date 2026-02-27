;;; flashcards-parse.el --- Parsing logic for flashcards -*- lexical-binding: t; -*-

;;; Commentary:
;; This file contains the logic for parsing org and org-roam files to flashcards format

;;; Code:

;;;###autoload
(defun flashcards-parse-buffer ()
  "Parse the current buffer to check for flashcards.
This requires the flashcards-directory to be set."
  (interactive)
  (flashcards-directory-validate)
  ;; Parsing logic here
  (message "Parsing flashcards in buffer: %s" (buffer-name))
  (message "Using %s as a flashcards directory" flashcards-directory))


(provide 'flashcards-parse)

;;; flashcards-parse.el ends here
