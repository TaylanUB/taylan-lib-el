;;; taylan-plural-convenience-macros.el --- "Plural convenience macros"

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

(defmacro set-face-attributes (&rest dict)
  "DICT is an alist mapping faces to a plist like the ARGS
parameter of `set-face-attribute'."
  (genprogn (face . attrs) dict
    `(progn
       (defface ,face nil nil)
       (set-face-attribute ',face nil ,@attrs))))

(defmacro define-keys (kmap &rest definitions)
  "Define several keys for KMAP.
DEFINITIONS should be an alist mapping KEYSs as understood by
`kbd' to DEFs as understood by `define-key'."
  (declare (indent 1))
  (genprogn (keys def) definitions
    `(define-key ,kmap (kbd ,keys) ,def)))

(defmacro modify-syntax-entries (table &rest entries)
  "Modify several entries in the syntax table.
Pass nil to alter the current syntax table.  ENTRIES is an alist
mapping CHARs to NEWENTRYs. See `modify-syntax-entry'."
  (declare (indent 1))
  (genprogn (char newentry) entries
    `(modify-syntax-entry ,char ,newentry ,table)))

(provide 'taylan-plural-convenience-macros)
;;; taylan-plural-convenience-macros.el ends here
