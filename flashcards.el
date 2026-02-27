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
    (condition-case nil
	(require 'org-roam)
      (error (progn
	       (message "org-roam not available, using manual input")
	       (read-string "Reference (ID or title): "))))

    (if (fboundp 'org-roam-node-read)
	(let ((node (org-roam-node-read)))
	  (when node
	    (format "[[id:%s][%s]]"
		    (org-roam-node-id node)
		    (org-roam-node-title node))))
      ;; Fallback
      (flashcards-select-org-roam-node-simple))))


(defun flashcards-select-org-roam-node-simple ()
  "Select an org-roam node from a list."
  (require 'org-roam)
  (let* ((nodes (org-roam-node-list))
	 (choices (mapcar (lambda (node)
			    (cons (org-roam-node-title node)
				  (org-roam-node-id node)))
			  nodes)))
    (if (null choices)
	(progn
	  message "No org-roam nodes found")
      nil)
    (let ((selected (completing-read "Select node: " choices nil t)))
      (when selected
	(let ((node-id (cdr (assoc selected choices))))
	  (format "[[id:%s][%s]]" node-id selected))))))

(defun flashcards-generate-id ()
  "Generate a unique ID for a flashcard."
  (format "fc-%s-%s"
	  (format-time-string "%Y%m%d")
	  (substring (md5 (format "%s%s" (user-uid) (time-convert nil t))) 0 8)))

(defun flashcards-save-flashcard (filename id question answer subject reference)
  "Save a flashcard to FILENAME with given data."
  (with-temp-file filename
    (insert (format ";; -*- mode: org; -*-\n"))
    (insert (format ";; id: %s\n" id))
    (insert (format ";; created: %s\n" (format-time-string "%Y-%m-%d %H:%M")))
    (insert (format ";; subjects: %s\n" (mapconcat 'identity subject ", ")))
    (when reference
      (insert (format ";; reference: %s\n" reference)))
    (insert "\n*Question:\n")
    (insert question "\n\n")
    (insert "*Answer:\n")
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
	      (progn
		(message "Existing subjects: %s"
			 (mapconcat 'identity flashcards-subject-list ", "))
		(sit-for 1)

		(minibuffer-with-setup-hook
		    (lambda ()
		      (setq-local minibuffer-completion-table
				  flashcards-subject-list)
		      (setq-local completion-auto-help t)
		      (minibuffer-completion-help))
		  (completing-read-multiple
		   "Subjects (TAB to cumplete, comma to separate, RET to confirm): "
		   flashcards-subject-list
		   nil nil nil 'flashcards-subjects-history)))
	    (read-string "Subjects (comma separated): ")))

	 (subjects (if (listp subjects-str)
		       subjects-str
		     (split-string subjects-str ",[ \t]*" t))))

    (dolist (subject subjects)
      (cl-pushnew subject flashcards-subject-list :test 'string=))

    subjects))

;;;###autoload
(defun flashcards-start-review ()
  "Start a review session for a selected subject.
Shows random flashcards from the chosen subject."
  (interactive)
  (flashcards-directory-validate)

  ;; Get all available subjects
  (let ((subjects (flashcards-get-all-subjects)))
    (if (null subjects)
	(message "No subjects found. Create some flashcards first.")
      (flashcards-select-subjects-interactive subjects))))

(defun flashcards-select-subjects-interactive (subjects)
  "Interactive menu to select multiple subjects from SUBJECTS list."
  (let ((selected '())
	(remaining subjects)
	(input "")
	(prompt "Select subjects (numbers): "))

    (while (not (string= input "RET"))
      (flashcards-show-subjects-menu selected remaining)

      (setq input (read-from-minibuffer
		   (flashcards-build-prompt selected)
		   nil nil nil nil))

      (cond
       ((string= input "")
	(setq input "RET")
	(message "Selected: %s" (mapconcat 'identity selected ", ")))

       ((string-match-p "^[0-9]+$" input)
	(let ((num (string-to-number input)))
	  (if (and (> num 0) (<= num (length remaining)))
	      (let ((selected-subject (nth (1- num) remaining)))
		(setq remaining (append (cl-subseq remaining 0 (1- num))
					(cl-subseq remaining num)))
		(push selected-subject selected)
		(flashcards-show-subjects-menu selected remaining))
	    (message "Invalid number. Try again"))))

       (t
	(message "Invalid input. Enter a number or RET to finish."))))

    (reverse selected)))

(defun flashcards-build-prompt (selected)
  "Build the prompt string based on SELECTED subjects."
  (if selected
      (format "Selected: %s\nEnter number (RET to finish): "
	      (mapconcat 'identity selected ", "))
    "Enter number to select subject (RET to finish): "))

(defun flashcards-show-subjects-menu (selected remaining)
  "Display the subject selection menu in a buffer.
SELECTED is list of already chose subjects.
REMAINING is list of subjects still available."
  (with-current-buffer (get-buffer-create "*Subject Selection*")
    (erase-buffer)
    (insert "=== Subject Selection ===\n\n")

    (if selected
	(progn
	  (insert "✅ Selected:\n")
	  (dolist (s selected)
	    (insert (format "  - %s\n" s)))
	  (insert "\n"))
      (insert "🗒️ No subjects selected yet. \n\n"))

    (if remaining
	(progn
	  (insert "🗂️ Available subjects:\n")
	  (dotimes (i (length remaining))
	    (insert (format "  %d. %s\n" (1+ i) (nth i remaining))))
	  (insert "\n"))
      (insert "🟢 All subjects selected!\n\n"))

    (insert "-----------------------------\n")
    (insert "Type a number to add that subject\n")
    (insert "Press RET when done\n")

    (display-buffer (current-buffer)
		    '(display-buffer-in-side-window
		      (side . bottom)
		      (window-height . 20)))))

(defun flashcards-get-all-subjects ()
  "Get a list of all unique subjects from all flashcards."
  (let ((subjects '()))

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


;;; Load other modules
(require 'flashcards-parse)

(provide 'flashcards)

;;; flashcards.el ends here
