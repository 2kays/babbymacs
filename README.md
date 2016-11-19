### Babbymacs

A small emacs-like terminal text editor, in Common Lisp, backed by the
[cl-charms](https://github.com/HiTECNOLOGYs/cl-charms) ncurses binding.

*Only tested with SBCL so far. C-c handling requires implementation-specific
SIGINT condition handlers.*

To use, load the `babbymacs` ASDF system and eval:

```lisp
(babbymacs:main)
```

or

```lisp
(babbymacs:main "~/file/to/edit")
```

Basic Emacs movement (C-f/b/p/n, C-v, M-v) and editing (C-d, <backspace>)
keybinds apply. M-x currently takes you to an evaluator prompt, though a true
command system may be introduced in the future.

---

Work in progress. MIT licensed. Contributions welcome.
