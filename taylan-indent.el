;;; taylan-indent.el --- More auto-indenting

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

(defadvice open-line (after indent activate)
  "Fix indentation after the inserted newline."
  (save-excursion
    (forward-line)
    (unless (= (line-beginning-position) (line-end-position))
      (indent-according-to-mode))))

(defadvice newline-and-indent (before block-opening activate)
  "Append a newline first if the cursor is between { and }."
  (when (and (not (nth 8 (syntax-ppss)))
             (looking-back "{\s*")
             (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode))))

(provide 'taylan-indent)
;;; taylan-indent.el ends here
