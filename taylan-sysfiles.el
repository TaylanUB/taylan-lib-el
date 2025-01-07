;;; taylan-sysfiles.el --- Abstraction around env vars indicating files

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
  (require 'taylan-genprogn))

(require 'cl-lib)

(defun sysfile (name &rest path-components)
  "Return the pathname of the system file denoted by the symbol NAME.
If PATH-COMPONENTS are given, treat all components up to the last
one, including the system file NAME, as directories, and
concatenate them to make a path."
  (let ((file (getenv (symbol-name name))))
    (dolist (component path-components file)
      (setq file (expand-file-name component file)))))

(defun sysdir (name &rest path-components)
  "Like `sysfile', but returns a directory."
  (file-name-as-directory (apply 'sysfile name path-components)))

(defmacro make-dir-abstractions (&rest specs)
  "SPECS is a list of two-elements lists like (NAME PATH), where
NAME is a symbol and PATH a string.  For each NAME `<name>', a
variable `<name>-dir' that holds PATH, a function `<name>-file'
that returns a file in `<name>-dir' by concatenating its
arguments as path-components, and a function `<name>-dir' that is
like `<name>-file' but returns a directory, is created."
  (genprogn (name dir) specs
    (let* ((name (symbol-name name))
           (dir-var (intern (concat name "-dir")))
           (file-fn (intern (concat name "-file")))
           (dir-fn (intern (concat name "-dir"))))
      `(progn
         (defvar ,dir-var ,(file-name-as-directory (expand-file-name dir))
           ,(concat "Path of the " name " directory."))
         (defun ,file-fn (&rest path-components)
           ,(concat "Return a file under `" (symbol-name dir-var)
                    "' by concatenating PATH-COMPONENTS.")
           (apply 'concat ,dir-var (cl-maplist
                                    (lambda (list)
                                      (if (cdr list)
                                          (file-name-as-directory (car list))
                                        (car list)))
                                    path-components)))
         (defun ,dir-fn (&rest path-components)
           ,(concat "Like `" (symbol-name file-fn) "' but returns a directory.")
           (file-name-as-directory (apply ',file-fn path-components)))))))

(provide 'taylan-sysfiles)
;;; taylan-sysfiles.el ends here
