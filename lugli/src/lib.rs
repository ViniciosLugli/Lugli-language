mod ast;
mod environment;
mod interpreter;
mod parser;
mod stdlib;
mod token;

pub use interpreter::interpret;
