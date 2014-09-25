;;; taylan-ssh.el --- SSH

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: comm, processes, terminals, unix

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

(require 'term)

(defun ssh (host &optional user)
  "Run SSH in a term-mode buffer for HOST."
  (interactive "sHost: \nsUser: ")
  (let ((address (concat (if (< 0 (length user)) (concat user "@")) host)))
    (switch-to-buffer
     (make-term (concat "SSH for " address) "ssh" nil address))
    (term-char-mode)))

(defun ssh-dired (host &optional user)
  "Visit home directory at HOST via tramp/ssh."
  (interactive "sHost: \nsUser: ")
  (find-file
   (concat "/ssh:" (if (< 0 (length user)) (concat user "@")) host ":~")))

(provide 'taylan-ssh)
;;; taylan-ssh.el ends here
