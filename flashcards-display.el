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

(defun flashcards-display-flashcard (random-file)
  "Display a flaschard from RANDOM-FILE in an interactive window."
  (let ((buffer (get-buffer-create "*Flashcard Review*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "=== Flashcard Review ===\n\n"))
      (insert (format "File: %s\n" random-file))
      (insert "\nPress 'q' to close"))

    (display-buffer buffer
		    '(display-buffer-in-side-window
		      (side . bottom)
		      (window-height . 15)))
    (select-window (get-buffer-window buffer))))

(defun flashcards-fetch-random-flashcard (flashcard-files)
  "Select a random flashcard from FLASHCARD-FILES list.
Returns the path of the randomly selected file."
  (let* ((total (length flashcard-files))
	 (random-index (random total))
	 (random-file (nth random-index flashcard-files)))
    random-file))

(provide 'flashcards-display)

;;; flashcards-display.el ends here
