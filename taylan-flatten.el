;;; taylan-flatten.el --- Flatten a tree in-place

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

;; Flatten a tree in-place.

;;; Code:

(defun flatten (tree)
  "Flatten TREE, in place.  Nil that are in car position, and
non-nil values in cdr position, are preserved."
  (flatten1 tree nil))

(defun flatten1 (tree tail)
  (cond
   ((null tree)
    tail)
   ((atom tree)
    (cons tree tail))
   (t
    (let ((list tree))
      (catch 'break
        (while t
          (when (consp (car list))
            (let ((tail (flatten1 (car list) (flatten1 (cdr list) tail))))
              (setcar list (car tail))
              (setcdr list (cdr tail)))
            (throw 'break nil))
          (when (null (cdr list))
            (setcdr list tail)
            (throw 'break nil))
          (when (atom (cdr list))
            (setcdr list (cons (cdr list) tail))
            (throw 'break nil))
          (setq list (cdr list)))))
    tree)))

(provide 'taylan-flatten)
;;; taylan-flatten.el ends here
