#![feature(try_trait)]

extern crate xml;
extern crate osm_primitives;

pub mod reader;

pub struct DocumentMetadata {
  pub version: Option<String>,
  pub generator: Option<String>,
}
