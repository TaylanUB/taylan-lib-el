;;; taylan-term-programs.el --- Launch programs in term

;; Copyright (C) 2014  Taylan Ulrich Bayirli/Kammer

;; Author: Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;; Keywords: extensions, processes, terminals, unix

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

(require 'term)
(require 'shell-quasiquote)

(defun rtorrent ()
  "Switch to the rtorrent buffer, creating it if it doesn't
exist."
  (interactive)
  (switch-to-buffer (make-term "rtorrent" "rtorrent"))
  (term-char-mode))

(defvar mplayer-executable "mpv")

(defun mplayer (file &optional arguments)
  "Start a terminal-emulator with MPlayer playing FILE.
When used interactively, the prefix argument tells to prompt the
user for a complete shell command, such that arbitrary arguments
can be added.  When used non-interactively, ARGUMENTS must be a
list of strings."
  (interactive "fFile: \nP")
  (let* ((file (if (or (file-name-absolute-p file)
                       (string-match-p "^[a-z:]*://" file))
                   file
                 (expand-file-name file)))
         (partial-shell-command
          (shqq (,mplayer-executable ,file)))
         (shell-command
          (if (and (called-interactively-p 'any) arguments)
              (read-shell-command "Shell command: " partial-shell-command)
            (shqq (,,partial-shell-command ,@arguments)))))
    (switch-to-buffer (make-term "MPlayer" "sh" nil "-c" shell-command))
    (term-mode)
    (term-char-mode)))

(defun youtube-get-formats (uri)
  "Get the available formats for the YouTube video at URI."
  (shell-command-to-string (shqq (youtube-dl -F ,uri))))
(defun youtube-get-real-uri (uri &optional format)
  "Get the URI for the YouTube video at URI."
  (shell-command-to-string
   (shqq (youtube-dl -qg ,uri ,@(if format `(-f ,format))))))
(defun youtube (uri &optional mplayer-arguments)
  "This is like the `mplayer' command but takes a YouTube URI."
  (interactive "sURI: \nP")
  (let* ((format (read-string (concat (youtube-get-formats uri) "\nFormat: ")))
         (uri (youtube-get-real-uri uri (and (not (string= "" format))
                                             format))))
    (mplayer uri mplayer-arguments)))

(provide 'taylan-term-programs)
;;; taylan-term-programs.el ends here
