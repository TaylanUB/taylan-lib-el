;;; taylan-shell-commands.el --- Additional shell-command-* functions

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, unix

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; We want 9 shell commands:
;; (normal on-region on-string) × (normal to-string to-kill-ring)
;; Some of these already exist.

;;; Code:

(require 'taylan-with-string-buffer)

(defvar shell-command-remove-trailing-newlines t
  "Whether shell-command functions returning a string or saving
to the kill-ring should remove trailing newlines from their
output.")
(defsubst shell-command--maybe-remove-trailing-newlines (string)
  (if shell-command-remove-trailing-newlines
      (replace-regexp-in-string (rx (+ "\n") eot) "" string)
    string))

;; `shell-command' exists

;; `shell-command-on-region' exists

(defun shell-command-on-string (string command)
  "Execute string COMMAND in inferior shell with STRING as input."
  (with-temp-buffer
    (insert string)
    (shell-command-on-region (point-min) (point-max) command)))

;; `shell-command-to-string' exists
(defadvice shell-command-to-string (around remove-trailing-newlines activate)
  "Remove trailing newlines from the output if
`shell-command-remove-trailing-newlines' is non-nil."
  (setq ad-return-value
        (shell-command--maybe-remove-trailing-newlines ad-do-it)))

(defun shell-command-on-region-to-string (start end command)
  "Execute string COMMAND in inferior shell with region as input
and return its output as a string.  Trailing newlines are removed
if `shell-command-remove-trailing-newlines' is non-nil."
  (shell-command-on-string-to-string (buffer-substring start end) command))

(defun shell-command-on-string-to-string (string command)
  "Execute string COMMAND in inferior shell with STRING as input
and return its output as a string.  Trailing newlines are removed
if `shell-command-remove-trailing-newlines' is non-nil."
  (shell-command--maybe-remove-trailing-newlines
   (with-string-buffer string
     (shell-command-on-region (point-min) (point-max) command nil t))))

(defun shell-command-to-kill-ring (command)
  "Execute string COMMAND in inferior shell and save its output
in the kill-ring.  Trailing newlines are removed if
`shell-command-remove-trailing-newlines' is non-nil."
  (interactive (list (read-shell-command "Shell command: ")))
  (kill-new (shell-command-to-string command)))

(defun shell-command-on-region-to-kill-ring (start end command)
  "Execute string COMMAND in inferior shell with region as input
and save its output in the kill-ring.  Trailing newlines are
removed if `shell-command-remove-trailing-newlines' is non-nil."
  (interactive (list (region-beginning) (region-end)
                     (read-shell-command "Shell command: ")))
  (kill-new (shell-command-on-region-to-string start end command)))

(defun shell-command-on-string-to-kill-ring (string command)
  "Execute string COMMAND in inferior shell with STRING as input
and save its output in the kill-ring.  Trailing newlines are
removed if `shell-command-remove-trailing-newlines' is non-nil."
  (with-temp-buffer
    (insert string)
    (shell-command-on-region-to-kill-ring
     (region-beginning)
     (region-end)
     command)))

(provide 'taylan-shell-commands)
;;; taylan-shell-commands.el ends here
