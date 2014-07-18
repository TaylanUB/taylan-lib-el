;;; taylan-with-string-buffer.el --- with-string-buffer function

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

(defmacro with-string-buffer (initial-contents &rest body)
  "Evaluate BODY like `progn' in temporary buffer, return contents.
INITIAL-CONTENTS is evaluated before the temporary buffer is
created, and inserted if non-nil."
  (declare (indent 1))
  (let ((content (make-symbol "initial-contents")))
    `(let ((,content ,initial-contents))
       (with-temp-buffer
         (if ,content (insert ,content))
         ,@body
         (buffer-string)))))

(provide 'taylan-with-string-buffer)
;;; taylan-with-string-buffer.el ends here
