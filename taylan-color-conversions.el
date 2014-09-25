;;; taylan-color-conversions.el --- Color conversion functions

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

(defun color-name-to-hex (color)
  "Return the hex representation of the color NAME."
  (apply 'color-rgb-to-hex (color-name-to-rgb color)))

(defun color-term-to-name (num)
  "Return a string that is the name of the color NUM in the
terminal colorspace."
  (concat "color-" (number-to-string num)))

(defun color-term-to-hex (num)
  "Return the hex representation of the color NUM in the terminal
colorspace."
  (color-name-to-hex (color-term-to-name num)))

(defun color-term-to-rgb (num)
  "Return the rgb representation of the color NUM in the terminal
colorspace."
  (color-name-to-rgb (color-term-to-name num)))

(provide 'taylan-color-conversions)
;;; taylan-color-conversions.el ends here
