#![allow(dead_code)]

mod cpu;
mod mmu;
mod opcode;
mod registers;
mod util;

use log::info;

fn main() {
    env_logger::init();
    info!("Starting up");

    println!("Hello, world!");

    info!("Shutting down");
}
