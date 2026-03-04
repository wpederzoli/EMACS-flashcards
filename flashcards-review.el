;;; flashcards-review.el --- Review functionality for flashcards -*- lexical-binding: t; -*-

;;; Commentary:
;;This module handles the review interface for flashcards.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun flashcards-review-start()
  "Start a review session."
  (interactive)
  (flashcards-directory-validate)

  (flashcards-review-show-topic-selection))

(defun flashcards-review-show-topic-selection ()
  "Open the selection buffer for the user to choose the topics to review.
Returns the list of selected topics."
  (let* ((available-subjects (flashcards-get-available-subjects))
    (selected (flashcards-show-subjects-selection-buffer available-subjects)))
    (if selected
	(let ((files (flashcards-get-flashcards-by-subject selected)))
	  (if files
	      (progn
		(message "Found %d flashcards for %s"
			 (length files)
			 (mapconcat 'identity selected ", "))
		files)
	    (message "No flashcards found for selected subjects")
	    nil))
      (message "No subjects selected")
      nil)))

(defun flashcards-get-flashcards-by-subject (selected-subjects)
  "Get all flashcards that contain any of the SELECTED-SUBJECTS.
Returns a list of flashcard file paths."
  (let ((matching-files '()))
    (flashcards-directory-validate)

    (dolist (file (directory-files-recursively flashcards-directory "\\.fc$"))
      (with-temp-buffer
	(insert-file-contents file)

	(when (re-search-forward "^;; subjects: \\(.+\\)$" nil t)
	  (let ((file-subjects (split-string (match-string-no-properties 1) ", " t)))
	    (when (cl-intersection file-subjects selected-subjects :test 'string=)
	      (push file matching-files))))))

    matching-files))

(defun flashcards-get-available-subjects ()
  "Get all the available subjects from the flashcard files.
Returns a sorted list of unique subjects."
  (let ((subjects '()))
    (flashcards-directory-validate)

    (dolist (file (directory-files-recursively flashcards-directory "\\.fc$"))
      (with-temp-buffer
	(insert-file-contents file)
	(when (re-search-forward "^;; subjects: \\(.+\\)$" nil t)
	  (let ((file-subjects (split-string
				(match-string-no-properties 1)
				", " t)))
	    (dolist (subject file-subjects)
	      (cl-pushnew subject subjects :test 'string=))))))

    (sort subjects 'string<)))

(defun flashcards-show-subjects-selection-buffer (available-subjects)
  "Prepare and show the selection buffer using AVAILABLE-SUBJECTS."
  (let ((selection-buffer (get-buffer-create "*Flashcards Subject Selection*"))
	(selected-subjects '())
	(running t)
	(result nil))

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
	   ((eq char 'return) ;RET
	    (setq running nil)
	    (setq result selected-subjects)
	    (quit-window t)
	    (kill-buffer selection-buffer)
	    (message "Selected: %s" (mapconcat 'identity selected-subjects ", ")))
	
	   ((equal char ?q)
	    (setq running nil)
	    (quit-window t)
	    (kill-buffer selection-buffer)
	    (message "Selection Cancelled"))

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
	    (sit-for 1))))))
    result))


(provide 'flashcards-review)

;;; flashcards-review.el ends here
