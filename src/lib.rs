#![deny(rust_2018_idioms)]

pub mod ast;
mod auxiliary;
pub mod declare;
pub mod diagnostic;
pub mod inference;
pub mod instantiation;
pub mod lexer;
pub mod lifetime;
pub mod link;
pub mod lowering;
pub mod lowering_ctx;
pub mod parser;
pub mod pass;
pub mod resolution;
pub mod semantics;
pub mod substitution;
pub mod symbol_table;
pub mod types;
pub mod unification;
pub mod visit;
