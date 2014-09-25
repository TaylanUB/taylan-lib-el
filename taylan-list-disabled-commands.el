;;; taylan-list-disabled-commands.el --- List disabled commands

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: help

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

(defun list-disabled-commands ()
  "List all disabled commands."
  (interactive)
  (switch-to-buffer (get-buffer-create "*disabled-commands*"))
  (erase-buffer)
  (mapatoms
   (lambda (a)
     (when (and (commandp a) (get a 'disabled))
       (insert (symbol-name a) "\n")))
   obarray))

(provide 'taylan-list-disabled-commands)
;;; taylan-list-disabled-commands.el ends here
