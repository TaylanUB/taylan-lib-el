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
  "Return execution time of body in seconds as a float."
  (let ((start-time (current-time)))
    (funcall function)
    (let ((time (time-subtract (current-time) start-time)))
      (+ (* (nth 0 time) (expt 2 16))
         (nth 1 time)
         (/ (nth 2 time) 1000000.0)))))

(provide 'taylan-time)
;;; taylan-time.el ends here
