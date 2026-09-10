A basic Emacs configuration for users accustomed to modern editors like Kate, VS Code, Notepad++, Sublime Text, PyCharm, and many others. This config tries to make vanilla Emacs feel more like one of those editors. It gives you a familiar starting point for your own Emacs configuration adventures without having to unlearn years of muscle memory.

![Screenshot](screenshot.png)
Orange: CTRL
Green: ALT

# Try it out!
(on Emacs 31)
```
cd ~
rm -rf .emacs.d/
git clone https://github.com/BenediktChlopik/mortal-mode.git .emacs.d
```

I’ve used this config for a few months, and it’s becoming really close to the modern text editor experience. Contributions are welcome, but I’ll keep this config vanilla.


Here's the full set of keybindings from `mortal-map`. Note that all `C-*`, `M-*`, and `C-M-*` keys are first explicitly unbound (`#'undefined`), and then a subset is rebound — so the table below reflects the **final, effective** bindings after that rebinding.

| Key | Command |
|---|---|
| `<escape>` | quit out of anything |
| `<backspace>` | deleting all whitespaces before deleting one char |
| `<delete>` | same as above |
| `<tab>` | smartly indent |
| `<backtab>` | smartly unindent |
| `RET` | newline and indent smartly |
| `<f1>` | `ctl-x-map` (prefix) |
| `<f2>` | `help-map` (prefix) |
| `M-x` | execute extended command |
| `C-g` | `goto-map` (prefix) |
| `C-f` | `search-map` (prefix) |
| `M-<left>` | switch to previous tab |
| `M-<right>` | switch to next tab |
| `C-w` | close-tab |
| `C-b` | speedbar |
| `M-1` … `M-9` | select tab N |
| `C-a` | mark whole buffer !without moving point! |
| `M-p` | previous line |
| `M-n` | next line |
| `M-f` | forward char |
| `M-b` | backward char |
| `M-<up>` | move line/region up |
| `M-<down>` | move line/region down |
| `C-k` | delete-line |
| `C-x` | cut line or region |
| `C-c` | copy line or region |
| `C-v` | plain paste |
| `C-s` | save buffer |
| `C-o` | find file |
| `C-n` | new tab menu |
| `C-z` | undo |
| `C-y` | redo |
| `C-<return>` | insert line below |
| `C-r` | replace |
| `C-+` | text-scale-increase |
| `C--` | text-scale-decrease |
| `C-=` | text-scale-set |
| `C-;` | comment line or region |
| `C-<left>` |  nice move backward |
| `C-<right>` | nice move forward |
| `C-S-<left>` | mark-n-backward |
| `C-S-<right>` | mark-n-forward |
| `<page-up>` | next which key page |
| `<page-down>` | previous which key page |


All of this was written bit by bit with the help of Claude and ChatGPT. If you want to add or change features without knowing Elisp, use those tools too! It's really fun. 

