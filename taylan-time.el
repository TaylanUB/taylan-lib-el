;;; taylan-time.el --- Time execution of an Elisp function

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, tools

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

;; Time the execution of an Elisp function.

;;; Code:

(defun time (function)
  "Return execution time of calling FUNCTION in seconds as a
float.  FUNCTION is byte-compiled automatically."
  (setq function (byte-compile function))
  (let ((start (float-time)))
    (funcall function)
    (let ((end (float-time)))
      (- end start))))

(provide 'taylan-time)
;;; taylan-time.el ends here
