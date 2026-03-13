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
    (let ((random-index (random (length flashcard-files))))
      (flashcards-display-flashcard flashcard-files random-index))))

(defun flashcards-display-flashcard (flashcard-files current-index)
  "Display a flashcard from FLASHCARD-FILES at CURRENT-INDEX."
  (let ((buffer (get-buffer-create "*Flashcard Review*"))
        (running t)
        (flashcard-file (nth current-index flashcard-files))
        (total (length flashcard-files))
        (show-answer-p nil))

    (while running
      (with-current-buffer buffer
        (erase-buffer)

        (insert-file-contents flashcard-file)
        (goto-char (point-min))

        ;; Get subjects, questions and answers
        (let ((subjects "unknown")
              (question "No question found")
              (answer "No answer found")
	      (score (flashcards-get-score flashcard-file)))
          
          ;; Get subjects
          (when (re-search-forward "^;; subjects: \\(.+\\)$" nil t)
            (setq subjects (match-string-no-properties 1)))
          
          (goto-char (point-min))
          ;; Get questions
          (when (re-search-forward "^\\*Question:\n\\(.+\\)$" nil t)
            (setq question (match-string-no-properties 1)))
          
          (goto-char (point-min))
          ;; Get answers
          (when (re-search-forward "^\\*Answer:\n\\(.+\\)$" nil t)
            (setq answer (match-string-no-properties 1)))

          ;; Show progress
          (erase-buffer)
          (insert (format "=== Flashcard Review (%d/%d) ===\n\n"
                         (1+ current-index) total))
          (insert (format "Subjects: %s\n\n" subjects))
	  (insert (format "Score: %d\n\n" score))
          
          ;; Show question or answer depending on state
          (if show-answer-p
              (progn
                (insert "ANSWER:\n")
                (insert (format "%s\n\n" answer))
                (insert "-------------------------------\n")
                (insert "[s] Hide answer  [c] Correct  [w] Wrong  [q] Quit \n"))
            (progn
              (insert "QUESTION:\n")
              (insert (format "%s\n\n" question))
              (insert "-------------------------------\n")
              (insert "[s] Show answer  [c] Correct  [w] Wrong  [q] Quit \n")))))

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
            (if show-answer-p
                (setq show-answer-p nil)  ;; Hide answer
              (setq show-answer-p t))     ;; Show answer
            ;; Redraw buffer
            )

	   ((equal char ?c)
	    ;; Adds +1 to score
	    (let ((current-score (flashcards-get-score flashcard-file)))
	      (flashcards-update-score flashcard-file (1+ current-score))
	      (message "Score increased to %d" (1+ current-score))
	      (sit-for 1))
	    ;; Move to next question
            (let ((next-index (1+ current-index)))
              (if (< next-index total)
                  (progn
                    (quit-window t)
                    (flashcards-display-flashcard flashcard-files next-index))
                (progn
                  (setq running nil)
                  (quit-window t)
                  (kill-buffer buffer)
                  (message "Review completed! You reviewed %d flashcards." total)))))

	   ((equal char ?w)
	    (let ((current-score (flashcards-get-score flashcard-file)))
	      (flashcards-update-score flashcard-file (1- current-score))
	      (message "Score decreased to %d" (1- current-score))
	      (sit-for 1))
	    ;; Substract -1 to score
            (let ((next-index (1+ current-index)))
              (if (< next-index total)
                  (progn
                    (quit-window t)
                    (flashcards-display-flashcard flashcard-files next-index))
                (progn
                  (setq running nil)
                  (quit-window t)
                  (kill-buffer buffer)
                  (message "Review completed! You reviewed %d flashcards." total)))))

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

(defun flashcards-get-score (flashcard-file)
  "Get the current score from FLASHCARD-FILE.
Returns the score as an integer, or 0 if not found."
  (with-temp-buffer
    (insert-file-contents flashcard-file)
    (goto-char (point-min))
    (if (re-search-forward "^;; score: \\([0-9]+\\)$" nil t)
        (string-to-number (match-string-no-properties 1))
      0)))

(defun flashcards-update-score (flashcard-file new-score)
  "Update the score in FLASHCARD-FILE to NEW-SCORE."
  (with-temp-buffer
    (insert-file-contents flashcard-file)
    (goto-char (point-min))
    (if (re-search-forward "^;; score: [0-9]+$" nil t)
        (replace-match (format ";; score: %d" new-score))
      ;; If the line does not exist we add it after subjects
      (goto-char (point-min))
      (when (re-search-forward "^;; subjects: .+$" nil t)
        (forward-line 1)
        (insert (format ";; score: %d\n" new-score))))
    (write-file flashcard-file)))

(provide 'flashcards-display)

;;; flashcards-display.el ends here
