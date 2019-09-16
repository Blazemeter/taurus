# Console Reporter

This reporter module shows full screen dashboard with some KPIs and even graphs in ASCII-art.
Check this out:

![Console Screen](console-rsz.png)

This reporter is enabled by default.

Console reporter has the following settings:

```yaml
modules:
  console:
    # disable console reporter
    disable: false  # default: auto
    
    # configure screen type
    screen: console
    # valid values are:
    # - console (ncurses-based dashboard, default for *nix systems)
    # - gui (window-based dashboard, default for Windows, requires Tkinter)
    # - dummy (text output into console for non-tty cases)
    
    dummy-cols: 140  # width for dummy screen
    dummy-rows: 35   # height for dummy screen 
```
Tkinter dashboard (gui mode) can be scaled with `ctrl`+`Mouse-Scroll`

You can also disable this reporter by using [command-line](CommandLine.md) `-o` switch:
```bash
bzt config.yml -o modules.console.disable=true
```
There is 'auto' value for the `disable` parameter - console will be turned off if tty isn't found on the STDOUT side (e.g. output is intended for CI system)

On Windows, Console Screen is shown in separate window and users may change font size by holding
Ctrl key and using mouse wheel. Two additional options are `dummy-cols` and `dummy-rows`, they
affect the size of _dummy_ screen that is used by `dummy` screen.
