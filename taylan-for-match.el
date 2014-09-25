;;; taylan-for-match.el --- Execute code for every regexp match

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, matching

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

(defmacro for-match (regexp string &rest body)
  "Evaluate BODY for each occurrence of REGEXP in STRING.

During the evaluation of BODY, `$' is bound to a function which
can be used to get a matched sub-expression, like `match-string'.
E.g. ($ 0) will return the whole string that matched."
  (declare (indent 2))
  (let ((re (make-symbol "regexp"))
        (str (make-symbol "string"))
        (idx (make-symbol "index")))
    `(let ((,re ,regexp)
           (,str ,string))
       (while (string-match ,re ,str)
         (flet (($ (num) (match-string num ,str)))
           ,@body)
         (setq ,str (substring ,str (match-end 0)))))))

(provide 'taylan-for-match)
;;; taylan-for-match.el ends here
