;;; taylan-shell-quote.el --- Generate quoted sh snippets

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, unix

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

(defun shell-string-quote (string)
  "Return a string which, when parsed according to POSIX shell
grammar, would yield a \"TOKEN\" with the value STRING.

In less technical terms, this sanitizes a string to be injected
into a shell command.  For example it could be used like:

 (shell-command (concat \"grep -e \" (shell-string-quote str)))

The above is sure to pass the string STR directly to the ARGV of
grep.  It is safe, I swear.  Note however that the position in
which you inject the resulting string can still change its
meaning; e.g. the following will not work as expected,

 (shell-command (concat \"grep -e\" (shell-string-quote str)))

because there is no white-space between the `-e' and the string
in STR, in the resulting concatenated string."
  (concat "'" (replace-regexp-in-string "'" "'\\\\''" string) "'"))

(defun shell-quasiquote-part (part)
  "Process part of a `shell-quasiquote' body."
  (cond
   ((symbolp part) (symbol-name part))
   ((stringp part) part)
   ((numberp part) (number-to-string part))
   (t (error "Bad part: %S" part))))

(defmacro shell-quasiquote (&rest parts)
  "Create a shell command safe against injection.

This works somewhat akin to ` aka quasi-quote, but is more
complex.  Every element of PARTS must be one of:

A symbol, evaluating to its name.
A string, evaluating to itself.
A number, evaluating to its decimal representation.

`,x', where x must evaluate to a symbol, string, or number, and
will be interpreted as above and then passed through
`shell-string-quote'.

`,@x', where x must be a list whose elements will each be
interpreted like the x in `,x' and spliced into the results.

`,,x', where x will be interpreted like in `,x' but not
passed through `shell-string-quote'.

`,,@x', where x must be a list whose elements will each be
interpreted like the x in `,,x' and spliced into the results.

All resulting strings are concatenated with separating
white-space."
  `(mapconcat
    #'identity
    (list
     ,@(mapcar
        (lambda (part)
          (if (not (consp part))
              (shell-quasiquote-part part)
            (cond
             ((eq (car part) '\,)
              (let ((part (cadr part)))
                (cond
                 ((and (consp part) (eq (car part) '\,))
                  `(shell-quasiquote-part ,(cadr part)))
                 ((and (consp part) (eq (car part) '\,@))
                  `(mapconcat #'shell-quasiquote-part ,(cadr part) " "))
                 (t
                  `(shell-string-quote (shell-quasiquote-part ,part))))))
             ((eq (car part) '\,@)
              `(mapconcat
                (lambda (part)
                  (shell-string-quote (shell-quasiquote-part part)))
                ,(cadr part)
                " "))
             (t
              (error "Plain list not allowed: %S" part)))))
        parts))
    " "))

(provide 'taylan-shell-quote)
;;; taylan-shell-quote.el ends here
