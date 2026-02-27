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

;;; flashcards-review.el ends here
