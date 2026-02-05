# Usage of `yi`

To use `yi`, you must first `cargo build --release` the compiler or download one from github releases (when i will eventually put binaries there)

## Preparing yi_std
to prepare yi_std for using Yi, you must `cd` into it, `cargo build --release`, and finally
`cp target/release/libyi_std.<lib extension> <PROJECT ROOT>`

then, to run the examples, do `cargo test --release`. If it says `ok` for everything, the compiler works.
