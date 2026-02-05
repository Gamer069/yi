# TODOs

- [x] have a compiled executable

- [x] start making yi std

- [x] fix identifiers

- [x] make it so you can specify type for variables

- [x] make it so the examples/ast.yi example properly works - something is wrong with the types

- [x] make it so ALL errors are in ukrainian INCLUDING IR generation

- [x] current examples work

- [x] add an if statement 

- [x] add NotEq to BinOp

- [x] deal with scopes properly rather than blindly assuming the scope is global as soon as a function ends (nested functions break)

- [x] make it just друклн not друклн_і64 (un-uglify)

- [x] put stuff in different files so its not that messy

- [x] modulo operator

- [x] unary operator

- [x] elifs

- [ ] MORE EXAMPLES

- [ ] custom types (structs &| unions, unions could be ditched tho)

- [ ] add text-like boolean operators (і, або, ітд.) (optional)

- [ ] make a package manager and call it BORSHCH

- [ ] builtin package importing


- [ ] arrays

- [ ] make an `англ {}` block

- [ ] add switch statement (`побачим x { якщо 1: верни ні; якщо 2: верни так; взагалі: верни хтозна; }`)

- [ ] add ranges

- [ ] add for loops (`для x в 0..=10 { }`)

- [ ] add while loops (`поки x { друклн_стр("щось") }`)

- [ ] add break/continue for loops

- [ ] add VARARGS

- [ ] string formatting

- [ ] proper warnings

- [ ] better error system - report more clearly what the user did wrong, use less expect()s

- [ ] optimize the final executable

- [ ] make an LSP for yi called yilsp

- [ ] make a linter for yi

- [ ] make a formatter for yi

- [ ] put future linter and formatter in the same project, don't separate

- [ ] more useful std functions

- [ ] more options

- [ ] experiment with cranelift JIT to perhaps make an IDLE similar to what python has - not sure

- [ ] finish this TODO list

- [ ] inline assembly..? idk tho

- [ ] bugfixes

- [ ] less hardcoding

- [ ] streamline LITERALLY EVERYTHING


## yi_std

- [x] make an alias for StdString which is a *const i8, so you can pass strings from yi to std without having to have messy arguments

- [x] make macros to make the dev process easier (like std_function! {)

- [x] manual mangling

- [x] a good  mapping

- [ ] file management

- [ ] math functions (sqrt, pow, etc.)

- [ ] string manipulation functions (or an actual string wrapper that you/the compiler can instantiate to call .split() on it)

- [ ] add collections to std, etc.

- [ ] wrap existing language constructs
