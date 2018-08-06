;;; emacs-clang-rename.el --- Renaming symbol at point or qualified name. -*-

;; Keywords: tools, c, c++, clang, cpp, cxx

;;; Copyright: Nils Deppe 2018

;;; Commentary:

;; Requires the json Emacs module in order to read the
;; compile_commands.json file.

;; To install emacs-clang-rename.el check that the directory of this file
;; is in your 'load-path', and add
;;   (require 'emacs-clang-rename)
;; to your Emacs init file.
;;
;; You can also use use-package:
;;
;;

;; There are a few user-customizable variables:
;; - emacs-clang-rename-binary: the path to the clang-rename binary
;; - emacs-clang-rename-extensions: list of file extensions on which to
;;                                  perform transformations when running on all
;;                                  files in the project.
;; - emacs-clang-rename-compile-commands-file: use this file instead of
;;                                             auto-detecting the
;;                                             compile_commands.json file
;; - emacs-clang-rename-temp-buffer-name: buffer where to write output.  The
;;                                        default is "*emacs-clang-rename*"

;;; Code:

(require 'json)

(defgroup emacs-clang-rename nil
  "Integration with emacs-clang-rename"
  :group 'c)


(defcustom emacs-clang-rename-binary "clang-rename"
  "Path to clang-rename executable."
  :type '(file :must-match t)
  :group 'emacs-clang-rename)


(defcustom emacs-clang-rename-temp-buffer-name "*emacs-clang-rename*"
  "The name of the temporary buffer output will be printed to."
  :type 'string
  :group 'emacs-clang-rename)


(defcustom emacs-clang-rename-compile-commands-file nil
  "Path to the compile_commands.json file.  If nil auto-detected."
  :type 'string
  :group 'emacs-clang-rename)


(defun emacs-clang-rename--find-compile-commands (start-dir)
  "Search from START-DIR to '/' for a compile_commands.json file."
  ;; emacs-clang-rename-compile-commands-file is nil if not set,
  ;; which is not a string so file-exists-p would throw an error.
  (if (and emacs-clang-rename-compile-commands-file
           (file-exists-p emacs-clang-rename-compile-commands-file))
      (setq start-dir emacs-clang-rename-compile-commands-file)
    (progn
      (while
          (and (not (equal "/" start-dir))
               (not (file-exists-p (concat start-dir "compile_commands.json"))))
        (setq start-dir (file-name-directory(substring start-dir 0 -1))))
      (if (file-exists-p (concat start-dir "compile_commands.json"))
          (setq start-dir (concat start-dir "compile_commands.json"))
        (setq start-dir "NotFound")))))


;; Current emacs-clang-rename-extensions has no effect
(defcustom emacs-clang-rename-extensions
  '("cpp" "cc" "cxx" "c" "m" "mm" "C" "CPP" "c++")
  "List of extensions of files to perform rename on."
  :type 'alist
  :group 'emacs-clang-rename)


;; Function is kept around to be used if/when clang-rename
;; supports multiple TUs properly.
(defun emacs-clang-rename--get-files-list (compile-commands-file)
  "Gets the list of C++ and C files from the COMPILE-COMMANDS-FILE."
  (setq json-array-type 'list)
  (setq json-key-type 'string)
  (setq json-object-type 'alist)
  (let ((compile-commands
         (json-read-file compile-commands-file)))
    (let ((file-list '()))
      (dolist (command compile-commands)
        ;; Convert the list into a hash table so we can have to assume order
        ;; of them in the JSON file, since that's not guaranteed
        (let ((command-hash (make-hash-table :test 'equal)))
          (dolist (element command)
            (puthash (car element) (cdr element) command-hash))
          ;; add C/C++ files to list, make sure the file paths are absolute
          (when
              (member
               (file-name-extension
                (gethash "file" command-hash)) emacs-clang-rename-extensions)
            (add-to-list
             'file-list (expand-file-name
                         (gethash "file" command-hash)
                         (gethash "directory" command-hash))))))
      ;; The compile_commands.json file can have the same file multiple times
      (delete-dups file-list))))

(defun emacs-clang-rename--get-build-dir-of (file-name compile-commands-file)
  "Gets the build directory of FILE-NAME from the COMPILE-COMMANDS-FILE."
  (setq json-array-type 'list)
  (setq json-key-type 'string)
  (setq json-object-type 'alist)
  (if (not (equal compile-commands-file "NotFound"))
      (let ((compile-commands
             (json-read-file compile-commands-file)))
        (let ((result-dir "nil"))
          (dolist (command compile-commands)
            ;; Convert the list into a hash table so we can have to assume order
            ;; of them in the JSON file, since that's not guaranteed
            (let ((command-hash (make-hash-table :test 'equal)))
              (dolist (element command)
                (puthash (car element) (cdr element) command-hash))
              ;; add C/C++ files to list, make sure the file paths are absolute
              (when (equal file-name (expand-file-name
                                      (gethash "file" command-hash)
                                      (gethash "directory" command-hash)))
                (setq result-dir (gethash "directory" command-hash)))))
          (if (equal result-dir "nil")
              (file-name-directory file-name)
            result-dir)))
    (file-name-directory file-name)))


(defalias 'emacs-clang-rename--bufferpos-to-filepos
  (if (fboundp 'bufferpos-to-filepos)
      'bufferpos-to-filepos
    ;; Emacs 24 doesn’t have ‘bufferpos-to-filepos’, simulate it using
    ;; ‘position-bytes’.
    (lambda (position &optional _quality _coding-system)
      (1- (position-bytes position)))))


(defun emacs-clang-rename--execute-rename (new-name old-name-or-offset)
  "Rename all instances of OLD-NAME-OR-OFFSET (expected to be a string \
of the form '-qualified-name=blah' or '-offset=1234') to NEW-NAME."
  (let ((output-buffer (get-buffer-create emacs-clang-rename-temp-buffer-name)))
    (with-current-buffer output-buffer (goto-char (point-max)))
    (with-current-buffer output-buffer (insert "\nRunning clang-rename:\n"))
    (let ((compile-commands-file (emacs-clang-rename--find-compile-commands
                                  (file-name-directory buffer-file-name))))
      (let ((exit-code
             (let ((cmd
                    (format "cd %s && %s %s -new-name=%s %s -i %s"
                            (emacs-clang-rename--get-build-dir-of
                             buffer-file-name compile-commands-file)
                            emacs-clang-rename-binary
                            old-name-or-offset
                            new-name
                            (if (not (equal compile-commands-file "NotFound"))
                                (format "-p=%s" compile-commands-file)
                              (format ""))
                            buffer-file-name
                            )))
               (call-process-shell-command cmd nil
                                           output-buffer nil))))
        (if (and (integerp exit-code) (zerop exit-code))
            ;; successfully ran clang-rename:
            (progn
              ;; (kill-buffer output-buffer)
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Failed running clang-rename:
          (let ((out-msg (format "clang-rename failed with %s %s\n"
                                 (if (integerp exit-code)
                                     "exit status"
                                   "signal")
                                 exit-code)))
            (with-current-buffer output-buffer (goto-char (point-max)))
            (with-current-buffer output-buffer (insert ?\n  out-msg ?\n))
            (display-buffer output-buffer)))))))


(defun emacs-clang-rename-at-point (new-name)
  "Rename all instances of the symbol at point in the file to NEW-NAME."
  (interactive "sEnter a new name: ")
  (save-some-buffers :all)
  ;; clang-rename should not be combined with other operations when undoing.
  (undo-boundary)
  (emacs-clang-rename--execute-rename
   new-name
   (format
    "-offset=%d"
    ;; clang-rename wants file (byte) offsets, not
    ;; buffer (character) positions.
    (emacs-clang-rename--bufferpos-to-filepos
     ;; Emacs treats one character after a symbol
     ;; as part of the symbol, but clang-rename
     ;; doesn’t. Use the beginning of the current
     ;; symbol, if available, to resolve the
     ;; inconsistency.
     (or (car (bounds-of-thing-at-point 'symbol))
         (point))
     'exact))))


(defun emacs-clang-rename-qualified-name (qualified-name new-name)
  "Rename all instances of QUALIFIED-NAME in file to NEW-NAME."
  (interactive "sEnter qualified name to rename: \nsEnter a new name: ")
  (save-some-buffers :all)
  ;; clang-rename should not be combined with other operations when undoing.
  (undo-boundary)
  (emacs-clang-rename--execute-rename
   new-name
   (format "-qualified-name=%s" qualified-name)))

(defun emacs-clang-rename-qualified-name-print (qualified-name new-name)
  "Print command to rename instances of QUALIFIED-NAME to NEW-NAME."
  (interactive "sEnter qualified name to rename: \nsEnter a new name: ")
  (save-some-buffers :all)
  ;; clang-rename should not be combined with other operations when undoing.
  (undo-boundary)
  (let ((output-buffer (get-buffer-create emacs-clang-rename-temp-buffer-name)))
    (with-current-buffer output-buffer (goto-char (point-max)))
    (with-current-buffer output-buffer (insert "\nRunning clang-rename:\n"))
    (let ((compile-commands-file (emacs-clang-rename--find-compile-commands
                                  (file-name-directory buffer-file-name))))
      (let ((output-string ""))
        (if  (equal compile-commands-file "NotFound")
            (progn
              (with-current-buffer output-buffer (goto-char (point-max)))
              (with-current-buffer output-buffer
                (insert "Could not find a compile_commands.json file.")))
          (progn
            (setq output-string
                  (concat output-string
                          (format "\nTo rename run:\n%s " emacs-clang-rename-binary)))
            ;; print out the flag to the compile commands file
            (setq output-string
                  (concat output-string (format "-p=%s " compile-commands-file)))
            ;; print out qualified names and list of all files
            (setq output-string
                  (concat output-string
                          (format "-qualified-name=%s -new-name=%s -i "
                                  qualified-name new-name)))
            (setq output-string
                  (concat output-string
                          (format "source0 [source1...]")))

            (with-current-buffer output-buffer (goto-char (point-max)))
            (with-current-buffer output-buffer
              (insert (format "%s\n" output-string)))))))
    (display-buffer output-buffer)))

(provide 'emacs-clang-rename)
;;; emacs-clang-rename.el ends here
