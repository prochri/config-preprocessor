Fun project allowing additional functionality by preprocessing [i3](https://i3wm.org/) configuration files or other config files.

This project is rather old and no longer used or maintained.

## Building

Only tested under Linux. Install `opam` and `ocamlfind` via `opam`.
Then build the program via `make`

## Usage Examples

For more examples and usage, see `config-preprocessor --help`:
```config
set $a $b                     -- recursive assingments possible
include $p                    -- include other files as if they are in this file
{ifdef a}some text{else}other text{endif}
                              -- check if variable is assigned
{ifmatch $a=value}...{endif}  -- simple string comparisons
$mod+{a,b} {do_a,do_b}        -- List expansion
#!ifdef
line
#!endif                       -- Multi Line if
${shellcommand}               -- execute shell command and put output here
$${a + $c}                    -- calculate with basic integer operations
```

The preprocessor will track all variables and process all control flow,
producing a new file on `stdout`.
Environment variables are treated as normal variables in the file and can be ignored using `--ignore-env`.
