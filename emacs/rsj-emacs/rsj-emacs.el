;;; rsj-emacs.el --- Personal helper functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Free Software Foundation, Inc.

;; Author: Rasmus Sjostrom <ras.sjostrom@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (org "9.0.10"))
;; Keywords: org todo scheduling notifications alert
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
;;; Code:
;;
;;;; Avy Operations

;;;; Window Focus
;; Temporarily hides the content of other windows interactively
;;
;; TODO: rsj/dominate-window() - Toggle maximize / magnify / centerwork on current window.

;; fixme: don't use this entire package just to get the relevant windows
;; (aw-window-list) <-- find alternative
;; (get-buffer-window) <-- ?
(require 'ace-window)

;; TODO: `apply-to-inactive' now works as intended and runs a function on each
;; inactive window. Find a good way to make them less distracting.
(defun rsj/apply-to-inactive (windows action)
  (interactive)
  (if (null windows) '()
    (let ((w (car windows)))
      (unless (eq w (get-buffer-window))
	(funcall action w)) ;; function to call on each inactive window
      (rsj/focus-this (cdr windows)))))

(rsj/focus-this (aw-window-list))

(enlarge-window 1 t)
(balance-windows)
(window-resize (get-buffer-window) 10 t)

(provide 'rsj-emacs)

;;; rsj-emacs.el ends here
