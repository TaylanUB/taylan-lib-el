;;; taylan-osx.el --- OS X specific utilities

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

;; Some neat utilities for making use of OS X features.

;;; Code:

(require 'taylan-shell-quote)

(defun osx-applescript (script &optional buffer)
  "Execute the AppleScript SCRIPT asynchronously.  Output goes to BUFFER if
non-nil, otherwise discarded."
  (let* ((process (start-process "osx-applescript" buffer "osascript")))
    (when (not buffer)
      (set-process-filter process 'ignore))
    (process-send-string process (concat script "\n"))
    (process-send-eof process)))

(defun osx-applescript-to-string (script)
  "Execute the AppleScript SCRIPT synchronously and return its
output as a string."
  (shell-command-to-string (shell-quasiquote osascript -e ,script)))

(defun osx-alert ()
  "Make Emacs display an OS X alert box.

This will make the app icon bounce on OS X when Emacs isn't in
the fore-ground, so it can be used as a simple notification
mechanism to draw a user's attention to Emacs when they're
working with another program."
  (osx-applescript "tell application \"Emacs\" to display alert \"Alert!\""))

(defun osx-frame-focused-p ()
  "Return non-nil if Emacs.app is \"frontmost\"."
  (let ((output (osx-applescript-to-string
                 "path to frontmost application as Unicode text")))
    (string-match-p ":Emacs.app:$" output)))

(defun osx-growl-register ()
  "Register Emacs with Growl."
  (osx-applescript "\
tell application \"GrowlHelperApp\"
  set the allNotificationsList to {\"Emacs Notification\"}
  set the enabledNotificationsList to {\"Emacs Notification\"}
  register as application \"Emacs\" \
    all notifications allNotificationsList \
    default notifications enabledNotificationsList \
    icon of application \"Emacs\"
end tell"))

(defun osx-growl-notify (title message)
  "Send a Growl notification."
  (osx-applescript (format "\
tell application \"GrowlHelperApp\"
  notify with name \"Emacs Notification\" \
  title %S description %S \
  application name \"Emacs\"
end tell" title message)))

(provide 'taylan-osx)
;;; taylan-osx.el ends here
