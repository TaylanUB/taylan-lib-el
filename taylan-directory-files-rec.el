;;; taylan-directory-files-rec.el --- Recursive `directory-files'

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
  (require 'cl-macs))

(require 'taylan-flatten)

(defun directory-files-rec (directory &optional format nosort)
  "Return a list representation of the file hierarchy rooted at
DIRECTORY.  If FORMAT is nil or `relative', a list of pathes
relative to DIRECTORY is returned; if it's `absolute', the pathes
are absolute; if it's `tree', then directories and their
recursive contents are represented by sub-lists whose car is the
name of the directory and cdr the contents.  (But the car of the
top-level list is not DIRECTORY.)  If NOSORT is non-nil, no
sorting is done on directory contents.  See `directory-files'."
  (funcall (if (eq format 'tree) #'identity #'flatten)
           (directory-files-rec1 directory format nosort "")))

(defun directory-files-rec1 (directory format nosort path)
  (let ((files (directory-files directory
                                (eq format 'absolute)
                                directory-files-no-dot-files-regexp
                                nosort)))
    (let ((default-directory
            (file-name-as-directory (expand-file-name directory))))
      (mapcar
       (lambda (file)
         (let ((path (concat path file)))
           (if (not (file-directory-p file))
               path
             (cons
              path
              (directory-files-rec1
               file format nosort
               (cl-case format
                 ((nil relative) (file-name-as-directory path))
                 ((absolute tree) "")
                 (otherwise
                  (error "Bad `directory-files-rec' format: %S" format))))))))
       files))))

(provide 'taylan-directory-files-rec)
;;; taylan-directory-files-rec.el ends here
