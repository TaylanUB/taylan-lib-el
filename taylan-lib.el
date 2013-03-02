(eval-when-compile
  (require 'cl))


;;; Anaphora

(defmacro aif (test then &rest else)
  "Anaphoric if."
  `(let ((it ,test)) (if it ,then ,@else)))

(defmacro acond (&rest clauses)
  "Anaphoric cond."
  `(catch done
     (mapc
      (lambda (clause)
        (let ((it (eval (car clause))))
          (when it
            (throw 'done (eval `(progn ,@(cdr clause)))))))
      ',clauses)))


;; Alists

(defun aput (alist-sym key val)
  (let ((cons (assoc key (symbol-value alist-sym))))
    (if cons
        (setcdr cons val)
      (push (cons key val) (symbol-value alist-sym)))))

(defun aget (alist key &optional ignore)
  (let ((entry (assoc key alist)))
    (if entry (cdr entry) nil)))


;;; Compose

(defun compose (&rest functions)
  "Return the composition of functions in the list FUNCTIONS.
The functions must all be unary."
  (lexical-let ((functions functions))
    (lambda (arg)
      (dolist (function functions)
        (setq arg (funcall function arg)))
      arg)))


;;; Destructuring

(defmacro destructuring-map (map-function args sequence &rest body)
  "Helper macro for `destructuring-mapcar' and
`destructuring-map'."
  (declare (indent 3))
  (let ((elt (make-symbol "element")))
    `(,map-function (lambda (,elt)
                      (destructuring-bind ,args ,elt
                        ,@body))
                    ,sequence)))

(defmacro destructuring-mapcar (args sequence &rest body)
  "Evaluate BODY for each element of SEQUENCE, with the variables
specified in ARGS bound to the corresponding values in each
element, and make a list of the results."
  (declare (indent 2))
  `(destructuring-map mapcar ,args ,sequence ,@body))

(defmacro destructuring-mapc (args sequence &rest body)
  "Evaluate BODY for each element of SEQUENCE for side-effects
only, with the variables specified in ARGS bound to the
corresponding values in each element."
  (declare (indent 2))
  `(destructuring-map mapc ,args ,sequence ,@body))

(defmacro destructuring-mapcar-genprogn (args sequence &rest body)
  "This is a helper for creating macros.
Generate a `progn' expression that would execute BODY for each
element of SEQUENCE, with the variables specified in ARGS bound
to the corresponding values in each element."
  (declare (indent 2))
  `(cons 'progn
         (destructuring-mapcar ,args ,sequence ,@body)))


;;; Replace symbol

(defun replace-symbol (from-symbol to-symbol &optional delimited start end)
  (interactive "sReplace symbol: \nsReplace symbol with: ")
  (while (re-search-forward
          (rx-to-string `(: symbol-start ,from-symbol symbol-end)))
    (replace-match to-symbol)))


;;; Match

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
         (cl-flet (($ (num) (match-string num ,str)))
           ,@body)
         (setq ,str (substring ,str (match-end 0)))))))


;;; Plist merge

(defun plist-merge (old new)
  "Merge the plist NEW into the plist OLD.
Overlapping values in NEW overwrite values in OLD.  The merged
plist is returned; use `(setq x (plist-merge x y))' to be sure to
use the new value.  The plist OLD is modified by side-effects."
  (while new
    (setq old (plist-put old (car new) (cadr new)))
    (setq new (cddr new)))
  old)


;;; Boolean variable functions

(defmacro toggle (&rest symbols)
  "Set each SYMBOL to (not SYMBOL)."
  (destructuring-mapcar-genprogn symbol symbols
    `(set ',symbol (not ,symbol))))
(defmacro enable (&rest symbols)
  "Set each SYMBOL to t."
  (destructuring-mapcar-genprogn symbol symbols
    `(set ',symbol t)))
(defmacro disable (&rest symbols)
  "Set each SYMBOL to nil."
  (destructuring-mapcar-genprogn symbol symbols
    `(set ',symbol nil)))

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
    (setf (symbol-function (toggler-symbol variable))
          `(lambda (&optional value)
             ,(concat "Toggle `" name "' between nil and non-nil.")
             (interactive)
             (toggle ,variable)))
    (setf (symbol-function (enabler-symbol variable))
          `(lambda ()
             ,(concat "Set `" name "' to t.")
             (interactive)
             (enable ,variable)))
    (setf (symbol-function (disabler-symbol variable))
          `(lambda ()
             ,(concat "Set `" name "' to nil.")
             (interactive)
             (disable ,variable)))))

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


;;; Sysfiles and dir abstractions

(defun sysfile (name &rest path-components)
  "Return the pathname of the system file denoted by the symbol NAME.
If PATH-COMPONENTS are given, treat all components up to the last
one, including the system file NAME, as directories, and
concatenate them to make a path."
  (cl-reduce (lambda (path file) (expand-file-name file path))
             (cons (getenv (replace-regexp-in-string
                            "-" "_" (upcase (symbol-name name))))
                   path-components)))

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
  (destructuring-mapcar-genprogn (name dir) specs
    (let* ((name (symbol-name name))
           (dir-var (intern (concat name "-dir")))
           (file-fn (intern (concat name "-file")))
           (dir-fn (intern (concat name "-dir"))))
      `(progn
         (defvar ,dir-var ,dir
           ,(concat "Path of the " name " directory."))
         (defun ,file-fn (&rest path-components)
           ,(concat "Return a file under `" (symbol-name dir-var)
                    "' by concatenating PATH-COMPONENTS.")
           (apply 'concat ,dir-var (maplist
                                    (lambda (list)
                                      (if (cdr list)
                                          (file-name-as-directory (car list))
                                        (car list)))
                                    path-components)))
         (defun ,dir-fn (&rest path-components)
           ,(concat "Like `" (symbol-name file-fn) "' but returns a directory.")
           (file-name-as-directory (apply ',file-fn path-components)))))))


;;; Recursive directory traversal

(defun directory-files-rec (directory &optional full nosort)
  "Return a list representation of the file hierarchy rooted at
DIRECTORY.  Regular files are represented by their names.
Directories are represented by a cons cell whose car is the name
of the directory and cdr the result of recursively calling
`directory-files-rec' on it.  If FULL is non-nil, all returned
file names are absolute paths.  If NOSORT is non-nil, no sorting
is done on directory contents.  See `directory-files'."
  (mapcar (lambda (file)
            (let ((default-directory directory))
              (if (file-directory-p file)
                  (cons file
                        (directory-files-rec file full nosort))
                file)))
          (directory-files directory
                           full
                           directory-files-no-dot-files-regexp
                           nosort)))


;;; Color conversions

(defun color-name-to-hex (color)
  "Return the hex representation of the color NAME."
  (apply 'color-rgb-to-hex (color-name-to-rgb color)))

(defun color-term-to-name (num)
  "Return a string that is the name of the color NUM in the
terminal colorspace."
  (concat "color-" (number-to-string num)))

(defun color-term-to-hex (num)
  "Return the hex representation of the color NUM in the terminal
colorspace."
  (color-name-to-hex (color-term-to-name num)))

(defun color-term-to-rgb (num)
  "Return the rgb representation of the color NUM in the terminal
colorspace."
  (color-name-to-rgb (color-term-to-name num)))


;;; Convenience plurals

(defmacro set-face-attributes (&rest dict)
  "DICT is an alist mapping faces to a plist like the ARGS
parameter of `set-face-attributes'."
  (destructuring-mapcar-genprogn (face &rest attrs) dict
    `(progn
       (defface ,face nil nil)
       (set-face-attribute ',face nil ,@attrs))))

(defmacro define-keys (kmap &rest definitions)
  "Define several keys for KMAP.
DEFINITIONS should be an alist mapping KEYSs as understood by
`kbd' to DEFs as understood by `define-key'."
  (declare (indent 1))
  (destructuring-mapcar-genprogn (keys def) definitions
    `(define-key ,kmap (kbd ,keys) ,def)))

(defmacro modify-syntax-entries (table &rest entries)
  "Modify several entries in the syntax table.
Pass nil to alter the current syntax table.  ENTRIES is an alist
mapping CHARs to NEWENTRYs. See `modify-syntax-entry'."
  (declare (indent 1))
  (destructuring-mapcar-genprogn (char newentry) entries
    `(modify-syntax-entry ,char ,newentry ,table)))


;;; Syntax-table convenience functions

(defun syntax-table-add-paren-pair (open-char close-char &optional table)
  "Add OPEN and CLOSE to the syntax table as a parenthesis pair.
OPEN and CLOSE can be chars or strings containing one char."
  (destructuring-mapc (char) '(open-char close-char)
    `(if (stringp ,char) (setq ,char (elt ,char 0))))
  (modify-syntax-entries table
    (open-char (string ?\( close-char))))

(defun syntax-table-add-quote-char (char &optional table)
  (modify-syntax-entries table
    (char "\"")))


;;; Declarative configuration

(defmacro require-lib (path &rest requires)
  "Add (concat emacs-lib-dir path) to load-path and require
all requires."
  `(progn
     (add-to-list 'load-path (emacs-lib-dir ,path))
     ,@(mapcar (lambda (r) `(require ',r)) requires)))


;;; With string buffer

(defmacro with-string-buffer (initial-contents &rest body)
  "Evaluate BODY like `progn' in temporary buffer, return contents.
INITIAL-CONTENTS is evaluated before the temporary buffer is
created, and inserted if non-nil."
  (declare (indent 1))
  (let ((content (make-symbol "initial-contents")))
    `(let ((,content ,initial-contents))
       (with-temp-buffer
         (if ,content (insert ,content))
         ,@body
         (buffer-string)))))


;;; Dired

(defun dired-view-file-other-window ()
  "In Dired, examine a file in view mode, returning to Dired when done.
When file is a directory, show it in this buffer if it is inserted.
Otherwise, display it in another window."
  (interactive)
  (cl-flet ((view-file (file) (view-file-other-window file)))
    (dired-view-file)))


;;; Shell commands

;; We want 9 shell commands:
;; (normal on-region on-string) × (normal to-string to-kill-ring)
;; Some of these already exist.

(defvar shell-command-remove-trailing-newlines t
  "Whether shell-command functions returning a string or saving
to the kill-ring should remove trailing newlines from their
output.")
(defsubst shell-command--maybe-remove-trailing-newlines (string)
  (if shell-command-remove-trailing-newlines
      (replace-regexp-in-string (rx (+ "\n") eot) "" string)
    string))

;; `shell-command' exists

;; `shell-command-on-region' exists

(defun shell-command-on-string (string command)
  "Execute string COMMAND in inferior shell with STRING as input."
  (with-temp-buffer
    (insert string)
    (shell-command-on-region (point-min) (point-max) command)))

;; `shell-command-to-string' exists
(defadvice shell-command-to-string (around remove-trailing-newlines activate)
  "Remove trailing newlines from the output if
`shell-command-remove-trailing-newlines' is non-nil."
  (setq ad-return-value
        (shell-command--maybe-remove-trailing-newlines ad-do-it)))

(defun shell-command-on-region-to-string (start end command)
  "Execute string COMMAND in inferior shell with region as input
and return its output as a string.  Trailing newlines are removed
if `shell-command-remove-trailing-newlines' is non-nil."
  (shell-command-on-string-to-string (buffer-substring start end) command))

(defun shell-command-on-string-to-string (string command)
  "Execute string COMMAND in inferior shell with STRING as input
and return its output as a string.  Trailing newlines are removed
if `shell-command-remove-trailing-newlines' is non-nil."
  (shell-command--maybe-remove-trailing-newlines
   (with-string-buffer string
     (shell-command-on-region (point-min) (point-max) command nil t))))

(defun shell-command-to-kill-ring (command)
  "Execute string COMMAND in inferior shell and save its output
in the kill-ring.  Trailing newlines are removed if
`shell-command-remove-trailing-newlines' is non-nil."
  (interactive (list (read-shell-command "Shell command: ")))
  (kill-new (shell-command-to-string command)))

(defun shell-command-on-region-to-kill-ring (start end command)
  "Execute string COMMAND in inferior shell with region as input
and save its output in the kill-ring.  Trailing newlines are
removed if `shell-command-remove-trailing-newlines' is non-nil."
  (interactive (list (region-beginning) (region-end)
                     (read-shell-command "Shell command: ")))
  (kill-new (shell-command-on-region-to-string start end command)))

(defun shell-command-on-string-to-kill-ring (string command)
  "Execute string COMMAND in inferior shell with STRING as input
and save its output in the kill-ring.  Trailing newlines are
removed if `shell-command-remove-trailing-newlines' is non-nil."
  (with-temp-buffer
    (insert string)
    (shell-command-on-region-to-kill-ring
     (region-beginning)
     (region-end)
     command)))


;;; Shell string quote

(defun shell-string-quote (string)
  "Return a string which, when parsed according to POSIX shell
grammar, would yield a \"TOKEN\" with the value STRING."
  (concat "'" (replace-regexp-in-string "'" "'\\\\''" string) "'"))


;;; Xclip

(defun xclip-region-to-primary (start end)
  "Pipe region to xclip, using primary selection."
  (interactive (list (region-beginning) (region-end)))
  (shell-command-on-region start end "xclip -i -selection primary"))
(defun xclip-region-to-clipboard (start end)
  "Pipe region to xclip, using clipboard selection."
  (interactive (list (region-beginning) (region-end)))
  (shell-command-on-region start end "xclip -i -selection clipboard"))
(defun xclip-primary-to-kill-ring ()
  "Get primary selection from xclip and put it into the
kill-ring."
  (interactive)
  (shell-command-to-kill-ring "xclip -o -selection primary"))
(defun xclip-clipboard-to-kill-ring ()
  "Get clipboard selection from xclip and put it into the
kill-ring."
  (interactive)
  (shell-command-to-kill-ring "xclip -o -selection clipboard"))


;;; Toggle X clipboard usage

(defun toggle-x-clipboard-usage ()
  (interactive)
  (message "%S" (toggle x-select-enable-clipboard)))


;;; Pastebin yank

(defvar pastebin-yank--yank-function
  (lambda (&rest args)
    (funcall (key-binding (kbd "C-y")) args))
  "The function used to replicate yanking.
This is passed to `funcall'.")

(defvar pastebin-yank--pastebin-command "sprunge"
  "The command for pastebinning.
This is executed as a shell command.")

(defun pastebin-yank--pastebin (string)
  (shell-command-on-string-to-string string pastebin-yank--pastebin-command))

(defun pastebin-yank (&rest args)
  "Yank a pastebin link that contains what a yank would have yanked.
Actually, a call to `pastebin-yank--yank-function' determines the
pastebin content.  Arguments are forwarded to this function.  The
default value imitates pressing C-y.  The pastebin used depends
on `pastebin-yank--pastebin-command'."
  (interactive)
  (insert (pastebin-yank--pastebin
           (with-string-buffer nil
             (apply pastebin-yank--yank-function args)))))


;;; Saner indentation defaults

(defadvice open-line (after indent activate)
  "Fix indentation after the inserted newline."
  (save-excursion
    (forward-line)
    (indent-according-to-mode)))

(defadvice newline-and-indent (before block-opening activate)
  "Append a newline first if the cursor is between { and }."
  (when (and (not (nth 8 (syntax-ppss)))
             (looking-back "{\s*")
             (looking-at "\s*}"))
    (save-excursion
      (newline)
      (indent-according-to-mode))))


;;; Temporary buffer

(defun create-temporary-buffer ()
  "Create a temporary buffer and switch to it."
  (interactive)
  (switch-to-buffer (generate-new-buffer "*temporary-buffer*"))
  (fundamental-mode))


;;; Terminal and shell

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


;;; Mouse position

(defun point-at-mouse ()
  (interactive)
  (destructuring-bind (_ x . y) (mouse-pixel-position)
    (when (and x y)
      (posn-point (posn-at-x-y x y)))))


;;; Point at row/column

(defun point-at-row-and-column (row column)
  (save-excursion
    (goto-char (window-start))
    (forward-visible-line row)
    (+ (point) column)))


;;; SSH

(defun ssh (host &optional user)
  "Run SSH in a term-mode buffer for HOST."
  (interactive "sHost: \nsUser: ")
  (let ((address (concat (if (< 0 (length user)) (concat user "@")) host)))
    (switch-to-buffer
     (make-term (concat "SSH for " address) "ssh" nil address))
    (term-char-mode)))

(defun ssh-dired (host &optional user)
  "Visit home directory at HOST via tramp/ssh."
  (interactive "sHost: \nsUser: ")
  (find-file
   (concat "/ssh:" (if (< 0 (length user)) (concat user "@")) host ":~")))


;;; List disabled commands

(defun list-disabled-commands ()
  "List all disabled commands."
  (interactive)
  (switch-to-buffer (get-buffer-create "*disabled-commands*"))
  (erase-buffer)
  (mapatoms
   (lambda (a)
     (when (and (commandp a) (get a 'disabled))
       (insert (symbol-name a) "\n")))
   obarray))


;;; Programs

(defun rtorrent ()
  "Switch to the rtorrent buffer, creating it if it doesn't
exist."
  (interactive)
  (switch-to-buffer (make-term "rtorrent" "rtorrent"))
  (term-char-mode))

(defvar mplayer-executable "mplayer")

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
          (concat mplayer-executable " " (shell-string-quote file) " "))
         (shell-command
          (if (and (called-interactively-p 'any) arguments)
              (read-shell-command "Shell command: " partial-shell-command)
            (apply 'concat partial-shell-command
                   (mapcar (lambda (string)
                             (concat " " (shell-string-quote string)))
                           arguments)))))
    (switch-to-buffer (make-term "MPlayer" "sh" nil "-c" shell-command))
    (term-mode)
    (term-char-mode)))

(defun youtube-get-formats (uri)
  "Get the available formats for the YouTube video at URI."
  (shell-command-to-string (concat "youtube-dl -qF " (shell-string-quote uri))))
(defun youtube-get-real-uri (uri &optional format)
  "Get the URI for the YouTube video at URI."
  (shell-command-to-string
   (concat "youtube-dl -qg " (shell-string-quote uri)
           (and format (concat " -f " (shell-string-quote format))))))
(defun youtube (uri &optional mplayer-arguments)
  "This is like the `mplayer' command but takes a YouTube URI."
  (interactive "sURI: \nP")
  (let* ((format (read-string (concat (youtube-get-formats uri) "\nFormat: ")))
         (uri (youtube-get-real-uri uri (and (not (string= "" format))
                                             format))))
    (mplayer uri mplayer-arguments)))

(defvar shellplayer-playlist-file (sysfile 'shellplayer-tmpdir "playlist")
  "Playlist file of shellplayer.")

(defun shellplayer-edit-playlist ()
  (interactive)
  (find-file shellplayer-playlist-file))

(provide 'taylan-lib)
