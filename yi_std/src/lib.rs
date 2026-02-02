#[unsafe(no_mangle)]
pub extern "C" fn drukln() -> i32 {
    println!();
    0
}

#[unsafe(no_mangle)]
pub extern "C" fn drukln_i64(val: i64) -> i32 {
    println!("{}", val);
    0
}
