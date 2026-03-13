;;; flashcards-display.el --- Display flashcards for review -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles displaying the flashcards during review sessions.

;;; Code:

;;;###autoload
(defun flashcards-display-review-session (flashcard-files)
  "Start a review session with the given FLASHCARD-FILES.
Shows each flashcard one by one in a review buffer."
  (if (null flashcard-files)
      (message "No flashcards to review")
    (let ((random-file (flashcards-fetch-random-flashcard flashcard-files)))
      (flashcards-display-flashcard random-file))))

(defun flashcards-display-flashcard (flashcard-file)
  "Display a flashcard from FLASHCARD-FILE in an interactive window."
  (let ((buffer (get-buffer-create "*Flashcard Review*"))
        (running t))

    (while running
      (with-current-buffer buffer
        (erase-buffer)

        (insert-file-contents flashcard-file)
        (goto-char (point-min))

        (let ((subjects "unknown"))
          (when (re-search-forward "^;; subjects: \\(.+\\)$" nil t)
            (setq subjects (match-string-no-properties 1)))
          
          (goto-char (point-min))
          (let ((question "No question Found"))
            (when (re-search-forward "^\\*Question:\n\\(.+\\)$" nil t)
              (setq question (match-string-no-properties 1)))

            (erase-buffer)
            (insert (format "=== Flashcard Review ===\n\n"))
            (insert (format "Subjects: %s\n\n" subjects))
            (insert "QUESTION:\n")
            (insert (format "%s\n\n" question))
            (insert "-------------------------------\n")
            (insert "[s] Show answer [c] Correct [w] Wrong [q] Quit \n"))))

      (display-buffer buffer
                      '(display-buffer-in-side-window
                        (side . bottom)
                        (window-height . 15)))

      (with-selected-window (get-buffer-window buffer)
        (let ((char (read-event "Command: ")))
          (cond
           ((equal char ?q)
            (setq running nil)
            (quit-window t)
            (kill-buffer buffer)
            (message "Review ended"))

           ((equal char ?s)
            (message "Show answer - not implemented"))

           ((equal char ?c)
            (message "Mark correct - not implemented"))

           ((equal char ?w)
            (message "Mark wrong - not implemented"))

           (t
            (message "Invalid command - press s, c, w, or q")
            (sit-for 1))))))))
	 

(defun flashcards-fetch-random-flashcard (flashcard-files)
  "Select a random flashcard from FLASHCARD-FILES list.
Returns the path of the randomly selected file."
  (let* ((total (length flashcard-files))
	 (random-index (random total))
	 (random-file (nth random-index flashcard-files)))
    random-file))

(provide 'flashcards-display)

;;; flashcards-display.el ends here
