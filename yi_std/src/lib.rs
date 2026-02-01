#[unsafe(no_mangle)]
pub extern "C" fn drukln() -> i32 {
    println!();
    0
}
