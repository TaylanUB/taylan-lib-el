;;; taylan-boolvars.el --- Enable, disable, toggle boolean variables

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

(eval-when-compile
  (require 'taylan-genprogn)
  (require 'cl))

(defmacro toggle (&rest symbols)
  "Set each SYMBOL to (not SYMBOL)."
  (genprogn symbol symbols `(set ',symbol (not ,symbol))))
(defmacro enable (&rest symbols)
  "Set each SYMBOL to t."
  (genprogn symbol symbols `(set ',symbol t)))
(defmacro disable (&rest symbols)
  "Set each SYMBOL to nil."
  (genprogn symbol symbols `(set ',symbol nil)))

(defun toggler-symbol (variable)
  "Returns the symbol of the toggler-function of VARIABLE."
  (intern (concat "toggle-" (symbol-name variable))))

(defun enabler-symbol (variable)
  "Returns the symbol of the enabler-function of VARIABLE."
  (intern (concat "enable-" (symbol-name variable))))

(defun disabler-symbol (variable)
  "Returns the symbol of the disabler-function of VARIABLE."
  (intern (concat "disable-" (symbol-name variable))))

(defun generate-boolean-variable-functions (variable)
  "Define the toggler, enabler, and disabler functions for VARIABLE.
The names of these functions are determined by `toggler-symbol',
`enabler-symbol', and `disabler-symbol', and are
`toggle-<variable>', `enable-<variable>', and
`disable-<variable>' by default."
  (declare (indent 0))
  (let ((name (symbol-name variable)))
    (dolist (spec
             '((toggler-symbol "Toggle `%s' between nil and t." toggle)
               (enabler-symbol "Set `%s' to t." enable)
               (disabler-symbol "Set `%s' to nil." disable)))
      (let ((symbol (funcall (nth 0 spec) variable)))
        (unless (ignore-errors (symbol-function symbol))
          (setf (symbol-function symbol)
                `(lambda ()
                   ,(format (nth 1 spec) name)
                   (interactive)
                   (,(nth 2 spec) ,variable))))))))

(defun toggler (variable)
  "Returns the symbol of the toggler-function for VARIABLE.
Also creates the function if it doesn't exist. See
`generate-boolean-variable-functions'."
  (let ((toggler (toggler-symbol variable)))
    (unless (fboundp toggler)
      (generate-boolean-variable-functions variable))
    toggler))

(defun enabler (variable)
  "Returns the symbol of the enabler-function for VARIABLE.
Also creates the function if it doesn't exist. See
`generate-boolean-variable-functions'."
  (let ((enabler (enabler-symbol variable)))
    (unless (fboundp enabler)
      (generate-boolean-variable-functions variable))
    enabler))

(defun disabler (variable)
  "Returns the symbol of the disabler-function for VARIABLE.
Also creates the function if it doesn't exist. See
`generate-boolean-variable-functions'."
  (let ((disabler (disabler-symbol variable)))
    (unless (fboundp disabler)
      (generate-boolean-variable-functions variable))
    disabler))

(provide 'taylan-boolvars)
;;; taylan-boolvars.el ends here
