;;; taylan-term.el --- Augmentations for term

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: terminals

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
  (require 'shell))

(require 'term)

(defun term-with-shell ()
  "Start `term' with a shell in it and rename the buffer
appropriately."
  (interactive)
  (term (or explicit-shell-file-name (getenv "ESHELL") shell-file-name))
  (rename-buffer "*shell-term*" t))

(defun term-send-quoted ()
  "Send the next read character to the term."
  (interactive)
  (term-send-raw-string (char-to-string (read-char))))

(defadvice term-send-backspace (around c-h activate)
  "Send a ^H to the term. Works in more cases than ^?."
  (interactive)
  (term-send-raw-string "\C-h"))

(defun term-send-tab ()
  "Send a ^I to the term. Can be bound to <tab> in a GUI frame."
  (interactive)
  (term-send-raw-string "\C-i"))

(provide 'taylan-term)
;;; taylan-term.el ends here
