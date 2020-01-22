;;; rsj-org.el --- A collection of helpers for org-mode. -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Rasmus Sjostrom <ras.sjostrom@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (org "9.0.10") (avy "0"))
;; Keywords: org todo scheduling planning notify alert
;; URL: https://github.com/rasjostrom/org-today

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides some extended functionality to org-mode. Mainly
;; the focus is on smaller, daily tasks + agendas, problem solving, etc.
;;
;; If any one of these 'features' become good enough or too extensive,
;; they'll be moved into a package of their own.
;;
;;;; Features:
;;
;; * Alerts: Easy-to-use alert timers for todo items that are scheduled or
;;   have a deadline.
;;
;; * Generate Style Tag Functions: Macros for generating interactive
;;   functions used to wrap words with style tags.  These are then bound to
;;   my custom god+org-mode keymap.
;;
;; * TODO: Pushing Tasks: Reschedule tasks taking the rest of the agenda
;;   into consideration (push the remaining tasks of the day aswell).
;;
;; * TODO: Super Tasks: A parent problem that is broken down into smaller
;;   dependency tasks that could be scattered in different agenda files.
;;
;;   A super task could be a long term problem which shouldn't take up
;;   space in the daily agenda view.  Its dependency tasks can easily
;;   be scheduled, canceled and pushed, which makes daliy agendas
;;   easier to write, read and maintain.
;;
;;   Aditionally, interactive functions are provided to add, remove,
;;   close and open dependency tasks within a super task.
;;
;; * TODO: Time Tracking: Clock in/out interactively in the agenda.  When
;;   clocking the dependency task of a Super Task, this will also
;;   accumulate the time spent on the problem in total.

;;; Code:
(require 'hydra)
(require 'notifications)

(defgroup rsj-org nil
  "Customazation options for rsj-org"
  :group 'emacs)

;;;; Alert Scheduler
;;
;;;;; Options
;;
(defcustom rsj/org-set-alert-prop-p t
  "Insert custom property ':ALERT:' if non-nil."
  :type 'boolean
  :group 'rsj-org)

(defcustom rsj/org-sound-on-alert-p t
  "Play a sound on alert if non-nil."
  :type 'boolean
  :group 'rsj-org)

;; Write a function or find something to draw this more efficiently
(defvar message-border "
||==================================================||
||                                                  ||
||                Alert Message                     ||
||                                                  ||
||                                                  ||
||==================================================||
")

;;;;; Interactive Functions
;;
(defun rsj/org-schedule-with-alert ()
  "Schedule an org entry and set a timed alert for it.
Prompts the user for a TIME and ALERT-MSG.

TIME must be in a format supported by both 'run-at-time' and
'org-schedule'.  Assuming the current time is 12:00 and an alert
should be scheduled for 14:00, legal inputs could look like:
'14:00', '14.00', '2 hours', etc.  See `run-at-time' for more.

ALERT-MSG defaults to the headline of the todo entry if left
blank."
  (interactive)
  (if (org-entry-is-todo-p)
      (let ((time (read-string "Time: "))
	    (alert-msg (read-string "Alert Message: " (org-get-heading t t))))
	(rsj/set-alert-timer time alert-msg)
	(org-schedule nil time)
	(if rsj/org-set-alert-prop-p
	    (rsj/org-set-alert-prop time)))
    (signal 'user-error '("Not in a todo entry!"))))

(defun rsj/alert-at-time ()
  (interactive)
        (let ((time (read-string "Time: "))
	    (alert-msg (read-string "Alert Message: ")))
	  (rsj/set-alert-timer
	   time
	   (format "
||==================================================||


%s


||==================================================||
" alert-msg))))

;;;;; Internal Functions
;;
(defun rsj/alert (body &optional title)
  "Send BODY as a notification using the built-in notifications library.
The TITLE of the notification defaults to 'Alert!' if nil."
  (notifications-notify
   :title (or title "Alert!")
   :body body
;; :sound <path-to-sound-file>
   :timeout -1))

(defun rsj/set-alert-timer (time msg)
  "Set a timer to call alert at a TIME with optional MSG.
TIME can be in any format supported by 'run-at-time'."
  (run-at-time time nil (lambda (m) (rsj/alert m)) msg))

(defun rsj/org-set-alert-prop (time)
  "Set property ':ALERT: <TIME>' in current org entry."
  (org-set-property "ALERT" time))


;;;; Generate Style Tag Functions
;; Generate interactive avy function to style a word with TAG.
;; NAME is the symbol suffix.
;;
;; The generated function can be prefixed to style multiple words in
;; a sequence.
;;
;; Example:
;; (rsj/avy-make-word-style '~' 'verbatim')
;; (define-key rsj/god-org-map (kbd 'C-b') 'rsj/avy-make-word-verbatim)"
(defmacro rsj/make-word-style (tag name)
  "Generate interactive avy function to style a word with TAG.
NAME is the symbol suffix."
  (let ((funsymbol (intern (format "rsj/make-word-%s" name)))
	(doc (format "Add %s styling to word." name)))
    `(defun ,funsymbol (&optional arg)
       ,doc
       (interactive "p")
       (if (null (thing-at-point 'word))
           (error "No word at point!")
         (insert ,tag)
         (forward-word arg)
         (insert ,tag)))))

(defmacro rsj/avy-make-word-style (tag name)
    "Generate interactive avy function to style a word with TAG.
NAME is the symbol suffix."
  (let ((funsymbol (intern (format "rsj/avy-make-word-%s" name)))
	(doc (format "Add %s styling to word with avy." name)))
    `(defun ,funsymbol (&optional arg)
       ,doc
       (interactive "p")
       (let ((char (read-char "char: ")))
        (avy-goto-word-1 char))
        (insert ,tag)
        (forward-word arg)
        (insert ,tag))))

;;;; Org Project
;; Groups and triggers sub-todos under a parent project item.
(defun rsj/org-project-init ()
    (interactive)
    (let* ((title (read-string "Project Title: "))
	   (entry (concat
			title
			"\n  :PROPERTIES:"
			"\n  :ID:    " (replace-regexp-in-string " " "-" (downcase title))
			"\n  :DESCRIPTION:    " (read-string "Description: ")
			"\n  :ESTIMATE:    " (read-string "Time Estimate: ")
			"\n  :END:\n")))
      (org-insert-heading nil nil t)
      (insert entry)))

;; TODO: 1. Gather All subentries in a list and do auto-complete
;;       2. Copy and schedule the entry into the agenda

(defhydra rsj/hydra-org-project (:color pink :hint nil)
  "\n
  Project: %`rsj/org-project-active
  -------------------------------------------
  \n"
  ("i" (message rsj/org-project-active) "Clean" :column "Compile")
  ("q" nil "quit"))

;;;; Push Agenda
;; Pushes all scheduled items starting with the current one.

;;;; Slack Manager
;; Groups and manages unfinished tasks in a `rsj/super-task'.

(provide 'rsj-org)
;;; rsj-org.el ends here
