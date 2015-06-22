;;; taylan-replace-symbol.el --- Replace symbol

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: convenience, extensions

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

(eval-when-compile
  (put 'replace-regexp 'interactive-only nil))

(defun replace-symbol (from-symbol to-symbol &optional start end)
  "Call `replace-regexp' with FROM-SYMBOL surrounded with
symbol-delimiters, and the other arguments unchanged."
  (interactive
   (let* ((region (and transient-mark-mode mark-active))
          (common
           (query-replace-read-args
            (concat "Replace symbol" (if region " in region"))
            nil)))
     (list (nth 0 common) (nth 1 common)
           (if region (region-beginning))
           (if region (region-end)))))
  (replace-regexp (rx-to-string `(: symbol-start ,from-symbol symbol-end))
                  to-symbol nil start end))

(provide 'taylan-replace-symbol)
;;; taylan-replace-symbol.el ends here
