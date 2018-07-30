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
  :group 'emacs-clang-rename-extensions)


(defun emacs-clang-rename--find-compile-commands (start-dir)
  "Search from START-DIR to '/' for a compile_commands.json file."
  (while
      (and (not (equal "/" start-dir))
           (not (file-exists-p (concat start-dir "compile_commands.json"))))
    (setq start-dir (file-name-directory(substring start-dir 0 -1))))
  (if (file-exists-p (concat start-dir "compile_commands.json"))
      (setq start-dir (concat start-dir "compile_commands.json"))
    (setq start-dir "NotFound")))


;; Current emacs-clang-rename-extensions has no effect
(defcustom emacs-clang-rename-extensions
  '("cpp" "cc" "cxx" "c" "m" "mm" "C" "CPP" "c++")
  "List of extensions of files to perform rename on."
  :type 'alist
  :group 'emacs-clang-rename-extensions)


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
      (message "before: %s" file-list)
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
            (if (file-exists-p (concat "/" (gethash "file" command-hash)))
                (add-to-list 'file-list (gethash "file" command-hash))
              (add-to-list 'file-list
                           (expand-file-name
                            (concat (gethash "directory" command-hash) "/"
                                    (gethash "file" command-hash))))))))
      ;; The compile_commands.json file can have the same file multiple times
      (delete-dups file-list))))


(defalias 'emacs-clang-rename--bufferpos-to-filepos
  (if (fboundp 'bufferpos-to-filepos)
      'bufferpos-to-filepos
    ;; Emacs 24 doesn’t have ‘bufferpos-to-filepos’, simulate it using
    ;; ‘position-bytes’.
    (lambda (position &optional _quality _coding-system)
      (1- (position-bytes position)))))


(defun emacs-clang-rename-at-point (new-name)
  "Rename all instances of the symbol at point in the file to NEW-NAME."
  (interactive "sEnter a new name: ")
  (save-some-buffers :all)
  ;; clang-rename should not be combined with other operations when undoing.
  (undo-boundary)
  (let ((output-buffer (get-buffer-create emacs-clang-rename-temp-buffer-name)))
    (with-current-buffer output-buffer (goto-char (point-max)))
    (with-current-buffer output-buffer (insert "\nRunning clang-rename:\n"))
    (let ((compile-commands-file (emacs-clang-rename--find-compile-commands
                                  (file-name-directory buffer-file-name))))
      (let ((exit-code
             (if (not (equal compile-commands-file "NotFound"))
                 (call-process emacs-clang-rename-binary nil
                               output-buffer nil
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
                                 'exact))
                               (format "-new-name=%s" new-name)
                               (format "-p=%s" compile-commands-file)
                               "-i"
                               (buffer-file-name))
               (call-process emacs-clang-rename-binary nil
                             output-buffer nil
                             (format
                              "-offset=%d"
                              ;; clang-rename wants file (byte) offsets, not
                              ;; buffer (character) positions.
                              (emacs-clang-rename--bufferpos-to-filepos
                               ;; Emacs treats one character after a symbol as
                               ;; part of the symbol, but clang-rename doesn’t.
                               ;; Use the beginning of the current symbol, if
                               ;; available, to resolve the inconsistency.
                               (or (car (bounds-of-thing-at-point 'symbol))
                                   (point))
                               'exact))
                             (format "-new-name=%s" new-name)
                             "-i"
                             (buffer-file-name)))))
        (if (and (integerp exit-code) (zerop exit-code))
            ;; successfully ran clang-rename:
            (progn
              ;; (kill-buffer output-buffer)
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Failed running clang-rename:
          (let ((message (format "clang-rename failed with %s %s\n"
                                 (if (integerp exit-code)
                                     "exit status"
                                   "signal")
                                 exit-code)))
            (with-current-buffer output-buffer (goto-char (point-max)))
            (with-current-buffer output-buffer (insert ?\n  message ?\n))
            (display-buffer output-buffer)))))))


(defun emacs-clang-rename-qualified-name (qualified-name new-name)
  "Rename all instances of QUALIFIED-NAME in file to NEW-NAME."
  (interactive "sEnter qualified name to rename: \nsEnter a new name: ")
  (save-some-buffers :all)
  ;; clang-rename should not be combined with other operations when undoing.
  (undo-boundary)
  (let ((output-buffer (get-buffer-create emacs-clang-rename-temp-buffer-name)))
    (with-current-buffer output-buffer (goto-char (point-max)))
    (with-current-buffer output-buffer (insert "\nRunning clang-rename:\n"))
    (let ((compile-commands-file (emacs-clang-rename--find-compile-commands
                                  (file-name-directory buffer-file-name))))
      (let ((exit-code
             (if (not (equal compile-commands-file "NotFound"))
                 (call-process emacs-clang-rename-binary nil
                               output-buffer nil
                               (format "-qualified-name=%s" qualified-name)
                               (format "-new-name=%s" new-name)
                               "-i"
                               (buffer-file-name))
               (call-process emacs-clang-rename-binary nil
                             output-buffer nil
                             (format "-p=%s" compile-commands-file)
                             (format "-qualified-name=%s" qualified-name)
                             (format "-new-name=%s" new-name)
                             "-i"
                             (buffer-file-name)))))
        (if (and (integerp exit-code) (zerop exit-code))
            ;; successfully ran clang-rename:
            (progn
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Failed running clang-rename:
          (let ((output-message
                 (format "clang-rename failed with %s %s\n"
                         (if (integerp exit-code) "exit status"
                           "signal")
                         exit-code)))
            (with-current-buffer output-buffer (goto-char (point-max)))
            (with-current-buffer output-buffer (insert output-message))
            (display-buffer output-buffer)))))))

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
