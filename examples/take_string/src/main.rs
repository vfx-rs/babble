use std::os::raw::*;

#[repr(C)]
pub struct std_string {
    _unused: [u8; 0],
}

#[repr(C)]
pub struct Test_1_0_Class {
    _unused: [u8; 0],
}

extern "C" {

pub fn std_string_ctor(result: *mut *mut std_string) -> c_int;

pub fn Test_1_0_Class_take_string(result: *const std_string, self_: *const Test_1_0_Class, s: *const std_string) -> c_int;

}

pub fn main() {
    println!("Hello, world!");
}