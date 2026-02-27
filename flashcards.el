;;; flashcards.el --- Simple Flashcards fro Org files system -*- lexical-binding: t; -*-

;; Copyright (C) 2026 William Pederzoli

;; Author: William Pederzoli wpederzoli@gmail.com
;; Keywords: flashcards, spaced repetition, learning, org
;; Version: 0.1

;;; Commentary:
;; This package allows to generate flashcards from org-files and org-roam.

;;; Code:

(defcustom flashcards-directory nil
  "Directory where the generated flashcards will be stored.
This must be defined by the user before using the package."
  :group 'flashcards
  :type 'directory)

(defvar flashcards-subject-list nil
  "List all of the existing subjects for auto-completion.")

(defvar flashcards-index nil
  "In memory indexes for all flashcards.
It is a hashtable where the key is the ID and the value is the path to the file.")

;;;###autoload
(defun flashcards-set-directory (dir)
  "Configures the directory to store the flascards.
DIR must be a valid path."
  (interactive "Directory to store flashcards: ")
  (setq flashcards-directory (expand-file-name dir))
  (message "Flashcards directory set to: %s" flashcards-directory))

(defun flashcards-directory-validate ()
  "Verifies that the set flashcards directory is configured.
If it is not configured it will throw an error."
  (unless flashcards-directory
    (error "The flashcards-directory is not configured.Use M-x flashcards-set-directory or configure through EMACS config"))

  (unless (file-directory-p flashcards-directory)
    (make-directory flashcards-directory t)
    (message "Directory created: %s" flashcards-directory)))

;;;###autoload
(defun flashcards-create-new ()
  "Prompt the user with the required fields for creating a new flaschard."
  (interactive)
  (let* ((question (read-string "Question: "))
	 (answer (read-string "Answer: "))
	 (subject (flashcards-read-subjects))
	 (reference (flashcards-read-reference))
	 (id (flashcards-generate-id))
	 (filename (expand-file-name (concat id ".fc") flashcards-directory)))
    ;; Save flashcard
    (flashcards-save-flashcard filename id question answer subject reference)

    ;; Update indexes
    (flashcards-update-index id filename)

    (message "Flashcard created: %s" filename)))

(defun flashcards-read-reference ()
  "Read a org-roam reference from user.
Returns nul if user skips."
  (when (y-or-n-p "Set reference to a node? ")
    ;; Find nodes
    (read-string "Reference (ID or title): ")))

(defun flashcards-generate-id ()
  "Generate a unique ID for a flashcard."
  (format "fc-%s-%s"
	  (format-time-string "%Y%m%d")
	  (substring (md5 (format "%s%s" (user-uid) (time-convert nil t))) 0 8)))

(defun flashcards-save-flashcard (filename id question answer subject reference)
  "Save a flashcard to FILENAME with given data."
  (with-temp-file filename
    (insert (format ";; -*- mode: flashcard -*-\n"))
    (insert (format ";; id: %s\n" id))
    (insert (format ";; created: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
    (insert (format ";; subjects: $s\n" (mapconcat 'identity subject ", ")))
    (when reference
      (insert (format ";; reference: %s\n" reference)))
    (insert "\nQuestion:\n")
    (insert question "\n\n")
    (insert "Answer:\n")
    (insert answer "\n")))

(defun flashcards-update-index (id filename)
  "Update the in-memory index with new flashcard."
  (unless flashcards-index
    (setq flashcards-index (make-hash-table :test 'equal)))
  (puthash id filename flashcards-index)
  nil)

(defun flashcards-read-subjects ()
  "Read subjects from user with completion from existing subjects.
Returns a list of subjects."
  (let* ((subjects-str
	  (if flashcards-subject-list
	      (completing-read-multiple
	       "Subjects (comma separated): "
	       flashcards-subject-list
	       nil
	       nil
	       nil
	       'flashcards-subjects-history)
	    (read-string "Subjects (comma separated): ")))
	 (subjects (split-string subjects-str ",[ \t]*" t)))

    (dolist (subject subjects)
      (cl-pushnew subject flashcards-subject-list :test 'string=))

    subjects))

;;; Load other modules
(require 'flashcards-parse)

(provide 'flashcards)

;;; flashcards.el ends here
