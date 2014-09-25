;;; taylan-pastebin-yank.el --- Yank to pastebin, then yank URI

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: convenience

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

;; 

;;; Code:

(require 'taylan-shell-commands)

(defvar pastebin-yank--yank-function
  (lambda (&rest args)
    (funcall (key-binding (kbd "C-y")) args))
  "The function used to replicate yanking.")

(defvar pastebin-yank--pastebin-command "sprunge"
  "The command for pastebinning.
This is executed as a shell command.")

(defun pastebin-yank--pastebin (string)
  "Pass STRING to the stdin of `pastebin-yank--pastebin-command'
and return the stdout."
  (shell-command-on-string-to-string string pastebin-yank--pastebin-command))

(defun pastebin-yank (&rest args)
  "Yank a pastebin link that contains what a yank would have yanked.
Actually, a call to `pastebin-yank--yank-function' determines the
pastebin content.  Arguments are forwarded to this function.  The
default value imitates pressing C-y.  The pastebin used depends
on `pastebin-yank--pastebin-command'."
  (interactive)
  (insert (pastebin-yank--pastebin
           (with-string-buffer nil
             (apply pastebin-yank--yank-function args)))))

(provide 'taylan-pastebin-yank)
;;; taylan-pastebin-yank.el ends here
