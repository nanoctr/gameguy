#![allow(dead_code)]

mod cpu;
mod instruction;
mod mmu;
mod op_parsing;
mod registers;
mod util;

use log::info;

fn main() {
    env_logger::init();
    info!("Starting up");

    println!("Hello, world!");

    info!("Shutting down");
}
