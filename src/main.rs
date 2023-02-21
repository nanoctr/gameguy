use log::info;

mod cpu;
mod registers;
mod mmu;

fn main() {
    env_logger::init();
    info!("Starting up");

    println!("Hello, world!");

    info!("Shutting down");
}