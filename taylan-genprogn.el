;;; taylan-genprogn.el --- Macro creation helper

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

;; Makes it easier to write macros that output a `progn' form which repeats an
;; action over a series of forms.

;;; Code:

(defmacro genprogn (args sequence &rest body)
  "This is a helper for creating macros.
Generate a `progn' expression that would execute BODY for each
element of SEQUENCE, with the variables specified in ARGS bound
to the corresponding values in each element."
  (declare (indent 2))
  `(cons 'progn
         (loop for ,args in ,sequence collect (cons 'progn (list ,@body)))))

(provide 'taylan-genprogn)
;;; taylan-genprogn.el ends here
