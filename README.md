# emacs-clang-rename

**emacs-clang-rename** allows you to use
[clang-rename](https://clang.llvm.org/extra/clang-rename.html) inside Emacs to:
- Rename symbol at point inside the file and included header files.
- Rename qualified symbol inside the file and included header files.

**emacs-clang-rename** supports:
- Automatically searching for a `compile_commands.json` file up the directory
  hierarchy.
- Specifying a `compile_commands.json` file as a directory local variable.
- Correctly execute the rename regardless from which directory that Emacs was
  started in.
- Choose the buffer name where diagnostics are printed.
- Specifying a custom `clang-rename` binary at a global and directory local
  level.

## Installation

If you're using `use-package` you can add the following to your Emacs init file:
```elisp
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/nilsdeppe/emacs-clang-rename/master/emacs-clang-rename.el"
     "~/.emacs.d/plugins/emacs-clang-rename.el"))
(if (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el")
    (use-package emacs-clang-rename
      :bind (("C-c c p" . emacs-clang-rename-at-point)
             ("C-c c q" . emacs-clang-rename-qualified-name)
             ("C-c c a" . emacs-clang-rename-qualified-name-all))))
```

If you're not using `use-package` you can add the following to your Emacs init
file:
```elisp
(if (not (file-directory-p "~/.emacs.d/plugins/"))
    (make-directory "~/.emacs.d/plugins/"))
(if (not (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el"))
    (url-copy-file
     "https://raw.githubusercontent.com/nilsdeppe/emacs-clang-rename/master/emacs-clang-rename.el"
     "~/.emacs.d/plugins/emacs-clang-rename.el"))
(if (file-exists-p "~/.emacs.d/plugins/emacs-clang-rename.el")
    (require 'emacs-clang-rename))
```

## Customization

To specify the `clang-rename` binary set the variable
`emacs-clang-rename-binary`.

To specify the buffer name where the diagnostics are printed set the variable
`emacs-clang-rename-temp-buffer-name`.

To specify the location of the compile commands file you can specify the
following in the `.dir-locals.el`:
```elisp
(nil . ((emacs-clang-rename-compile-commands-file . "/path/to/compile_commands.json")))
```

## Why not the `clang-rename.el` bundled with LLVM?

The extension bundled with LLVM was used as inspiration for this
extension. However, it does not find compile commands when calling clang-rename
and therefore is not useful in many cases.

## License

Boost Software License, v1.0. See the included `LICENSE.md` file.

## Contributing

Contributions are more than welcome! :)
