# TODOs

- [x] have a compiled executable

- [x] start making yi std

- [x] fix identifiers

- [x] make it so you can specify type for variables

- [x] make it so the examples/ast.yi example properly works - something is wrong with the types

- [x] make it so ALL errors are in ukrainian INCLUDING IR generation

- [x] current examples work

- [x] add an if statement 

- [ ] MORE EXAMPLES

- [ ] put stuff in different files so its not that messy

- [ ] custom types (structs &| unions, unions could be ditched tho)

- [ ] add text-like boolean operators (і, або, ітд.) (optional)

- [ ] make a package manager and call it BORSHCH

- [ ] builtin package importing

- [ ] modulo operator

- [ ] arrays

- [ ] make an `англ {}` block

- [ ] add NotEq to BinOp

- [ ] add switch statement (`побачим x { якщо 1: верни ні; якщо 2: верни так; взагалі: верни хтозна; }`)

- [ ] add a while statement (`поки x { друклн_стр("щось") }`)

- [ ] add ranges

- [ ] add for loops (`для x в 0..=10 { }`)

- [ ] add break/continue for loops

- [ ] add VARARGS

- [ ] string formatting

- [ ] proper warnings

- [ ] better error system - report more clearly what the user did wrong, use less expect()s

- [ ] optimize the final executable

- [ ] deal with scopes properly rather than blindly assuming the scope is global as soon as a function ends (nested functions break)

- [ ] make an LSP for yi called yilsp

- [ ] make a linter for yi

- [ ] make a formatter for yi

- [ ] put future linter and formatter in the same project, don't separate

- [ ] more useful std functions

- [ ] more options

- [ ] experiment with cranelift JIT to perhaps make an IDLE similar to what python has - not sure

- [ ] finish this TODO list

- [ ] make it just друклн not друклн_і64 (un-uglify)

- [ ] inline assembly..? idk tho

- [ ] bugfixes

- [ ] less hardcoding

- [ ] streamline LITERALLY EVERYTHING


## yi_std

- [ ] file management

- [ ] manual mangling

- [ ] a good  mapping

- [ ] math functions (sqrt, pow, etc.)

- [ ] string manipulation functions (or an actual string wrapper that you/the compiler can instantiate to call .split() on it)

- [ ] make macros to maek the dev process easier (like std_function! {)

- [ ] make an alias for StdString which is a *const i8, so you can pass strings from yi to std without having to have messy arguments

- [ ] add collections to std, etc.

- [ ] wrap existing language constructs
