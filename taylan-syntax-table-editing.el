;;; taylan-syntax-table-editing.el --- Edit syntax table conveniently

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions

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

(require 'taylan-plural-convenience-macros)

(defun syntax-table-add-paren-pair (open close &optional table)
  "Add OPEN and CLOSE to the syntax table as a parenthesis pair.
OPEN and CLOSE can be chars or strings containing one char."
  (let ((open (if (stringp open) (elt open 0) open))
        (close (if (stringp close) (elt close 0) close)))
   (modify-syntax-entries table
     (open (string ?\( close)))))

(defun syntax-table-add-quote-char (char &optional table)
  (modify-syntax-entries table
    (char "\"")))

(provide 'taylan-syntax-table-editing)
;;; taylan-syntax-table-editing.el ends here
