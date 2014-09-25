;;; taylan-plist-merge.el --- Merge plists

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

(defun plist-merge (old new)
  "Merge the plist NEW into the plist OLD.
Overlapping values in NEW overwrite values in OLD.  The merged
plist is returned; use `(setq x (plist-merge x y))' to be sure to
use the new value.  The plist OLD is modified by side-effects."
  (while new
    (setq old (plist-put old (car new) (cadr new)))
    (setq new (cddr new)))
  old)

(provide 'taylan-plist-merge)
;;; taylan-plist-merge.el ends here
