;;; flashcards-review.el --- Review functionality for flashcards -*- lexical-binding: t; -*-

;;; Commentary:
;;This module handles the review interface for flashcards.

;;;Code:

;;;###autoload
(defun flashcards-review-start()
  "Start a review session."
  (interactive)
  (flashcards-directory-validate)

  (flashcards-review-show-topic-selection))

(defun flashcards-review-show-topic-selection ()
  "Open the selection buffer for the user to choose the topics to review.
Returns the list of selected topics."
  (flashcards-show-subjects-selection-buffer))

(defun flashcards-show-subjects-selection-buffer ()
  "Prepare and show the selection buffer."
  (let ((selection-buffer (get-buffer-create "*Flashcards Subject Selection*"))
	(available-subjects '("math" "science" "physics"))
	(selected-subjects '())
	(running t))

    (while running
      (with-current-buffer selection-buffer
	(erase-buffer)
	(insert "=== Select Topics ===\n")

	(insert "Available Subjects:\n\n")
	(dotimes (i (length available-subjects))
	  (insert (format " %d. %s\n" (1+ i) (nth i available-subjects))))

	(insert "\nSelected Subjects:\n\n")
	(dotimes (i (length selected-subjects))
	  (insert (format " %d. %s\n" (1+ i) (nth i selected-subjects))))
      
	(insert "\nPress number to select\, 'q' to quit\n"))

      (display-buffer selection-buffer
		    '(display-buffer-in-side-window
		      (side . bottom)
		      (window-height . 15)))

      (with-selected-window (get-buffer-window selection-buffer)
	(let ((char (read-event ": ")))
	  (cond
	   ((equal char ?q)
	    (setq running nil)
	    (quit-window t)
	    (kill-buffer selection-buffer)
	    (message "Selected: %s" (mapconcat 'identity selected-subjects ", ")))

	   ((and (>= char ?1) (<= char ?9))
	    (let ((num (- char ?0)))
	      (if (and (> num 0) (<= num (length available-subjects)))
		  (let ((selected (nth (1- num) available-subjects)))

		    (setq selected-subjects
			  (append selected-subjects (list selected)))
		    (setq available-subjects
			  (append (cl-subseq available-subjects 0 (1- num))
				  (cl-subseq available-subjects num)))
		    (message "Added: %s" selected))
		(progn
		  (message "Invalid number")
		  (sit-for 1)))))

	   (t
	    (message "Invalid input -press a number or 'q'")
	    (sit-for 1))))))))

(provide 'flashcards-review)

;;; flashcards-review.el ends here
