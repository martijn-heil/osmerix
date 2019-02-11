use std::io::prelude::*;
use std::iter::Peekable;

use xml::reader::{EventReader, XmlEvent};
use xml::attribute::OwnedAttribute;

use osm_primitives::*;

fn metadata<'a, T>(attributes: T) -> Result<ElementMetadata, std::option::NoneError> where T: Iterator<Item = &'a OwnedAttribute> {
  let mut id: Option<i64> = None;
  let mut user: Option<String> = None;
  let mut uid: Option<u64> = None;
  let mut version: Option<u32> = None;
  let mut changeset: Option<u64> = None;
  let mut timestamp: Option<String> = None;

  for attr in attributes {
    let name = &(attr.name.local_name);
    let value = &attr.value;

    match name.as_str() {
      "id"        => { id = Some(value.parse::<i64>().unwrap()); }
      "user"      => { user = Some(value.to_owned()); }
      "uid"       => { uid = Some(value.parse::<u64>().unwrap()); }
      "version"   => { version = Some(value.parse::<u32>().unwrap()); }
      "changeset" => { changeset = Some(value.parse::<u64>().unwrap()); }
      "timestamp" => { timestamp = Some(value.to_owned()); }
      _ => {}
    }
  }

  Ok(ElementMetadata {
    id: id?,
    user: user?,
    uid: uid?,
    version: version?,
    changeset: changeset?,
    timestamp: timestamp?
  })
}

pub struct DocumentMetadata {

}

/// Type definition may change over time, but unless a breaking change is made the struct is always
/// guaranteed to have (at least) the following two members: `row: u64` and `column: u64`.
type TextPosition = xml::common::TextPosition;

/// The relevance area or position of an error
pub enum ErrorPosition {
  /// Guaranteed exact first and last position of error relevance.
  Exact {
    /// Inclusive position of the character where the error relevance begins.
    first_char: TextPosition,

    /// Inclusive position of the character where the error relevance ends.
    last_char: TextPosition,
  },

  /// Best-effort position of error, not guaranteed to have any kind of accuracy.
  Rough(TextPosition),
}

pub enum Error {
  MissingRelationMemberType(ErrorPosition),
  MissingRelationMemberRole(ErrorPosition),
  MissingRelationMemberRef(ErrorPosition),
}

struct ReaderIterator<T: Read>(EventReader<T>);

pub struct Reader<T: Read> {
  parser: EventReader<T>,
  metadata: Option<DocumentMetadata>,
}

impl<T: Read> Reader<T> {
  fn new(r: T) -> Self {
    Reader {
      parser: EventReader::new(r),
    }
  }

  fn metadata(&mut self) -> DocumentMetadata {

  }
}

pub fn read<T: Read>(r: T) /*-> Iterator<Item = Element>*/ {
  let parser = EventReader::new(r);

  let mut parser = parser.into_iter().peekable();
  fn next<T: Iterator<Item = xml::reader::Result<XmlEvent>>>(parser: &mut Peekable<T>) -> Result<Option<Element>, Error> {
    let next = parser.next();
    if next.is_none() { return Ok(None); }
    match next.unwrap() {
      Ok(XmlEvent::StartElement { name, attributes, .. }) => {
        match name.local_name.as_str() {
          "node" => {
            let meta = metadata(attributes.iter()).unwrap();

            let mut lat: Option<f64> = None;
            let mut lon: Option<f64> = None;


            for attr in attributes.iter() {
              let name = &(attr.name.local_name);
              let value = &attr.value;

              match name.as_str() {
                "lat"       => { lat = Some(value.parse::<f64>().unwrap()); }
                "lon"       => { lon = Some(value.parse::<f64>().unwrap()); }
                _ => {}
              }
            }

            return Ok(Some(Element::Node(
              Node {
                lat: lat.unwrap(),
                lon: lon.unwrap(),
                tags: Vec::new(),
                metadata: meta,
              }
            )));
          }

          "way" => {
            let meta = metadata(attributes.iter()).unwrap();

            let mut way = Way {
              tags: Vec::new(),
              metadata: meta,
              nodes: Vec::new()
            };

            // Read all node references and tags
            loop {
              match parser.next().unwrap() { // Can't have an empty way anyway..
                Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                  match name.local_name.as_str() {
                    "nd" => {
                      way.nodes.push(
                        ReferencedNode {
                          id: attributes.iter()
                          .find(|attr| attr.name.local_name == "ref").unwrap()
                          .value.parse::<i64>().unwrap()
                        }
                      );

                      // Make sure to read EndElement for <nd ref=".." />
                      match parser.next().unwrap() {
                        Ok(XmlEvent::EndElement { .. }) => {}
                        _ => { panic!(); /* TODO error */ }
                      }
                    }

                    "tag" => {
                      let mut key: Option<String> = None;
                      let mut value: Option<String> = None;
                      for attr in attributes { // TODO optimize
                        match attr.name.local_name.as_str() {
                          "k" => { key = Some(attr.value); }
                          "v" => { value = Some(attr.value); }
                          _ => {}
                        }
                      }
                      way.tags.push(Tag { key: key.unwrap(), value: value.unwrap() });
                      // Make sure to read EndElement for <tag />
                      match parser.next().unwrap() {
                        Ok(XmlEvent::EndElement { .. }) => {}
                        _ => { panic!(); /* TODO error */ }
                      }
                    }
                    _ => { panic!(); /* TODO error */ }
                  }
                }

                Ok(XmlEvent::EndElement { .. }) => {
                  break; // Encountered </way>, exit from node & tag reading loop
                }

                _ => {/* TODO */ }
              }
            }

            return Ok(Some(Element::Way(way)));
          }

          "relation" => {
            let mut relation = Relation {
              metadata: metadata(attributes.iter()).unwrap(),
              members: Vec::new(),
              tags: Vec::new(),
            };

            loop {
              if let Ok(XmlEvent::EndElement { .. }) = parser.peek().unwrap() { break; }
              match parser.next().unwrap() {
                Ok(XmlEvent::StartElement { name, attributes, .. }) => {
                  match name.local_name.as_str() {
                    "member" => {
                      let mut kind_maybe: Option<String> = None;
                      let mut ref_id_maybe: Option<i64> = None;
                      let mut role_maybe: Option<String> = None;

                      for attr in attributes.into_iter() {
                        match attr.name.local_name.as_str() {
                          "type" => { kind_maybe = Some(attr.value); }
                          "ref" => { ref_id_maybe = Some(attr.value.parse::<i64>().unwrap()); }
                          "role" => { role_maybe = Some(attr.value); }
                          _ => {}
                        }
                      }

                      let kind    = if let Some(kind)   = kind_maybe    { kind }    else { return Err(Error::MissingRelationMemberType());  };
                      let ref_id  = if let Some(ref_id) = ref_id_maybe  { ref_id }  else { return Err(Error::MissingRelationMemberRef());   };
                      let role    = if let Some(role)   = role_maybe    { role }    else { return Err(Error::MissingRelationMemberRole());  };

                      let element = match kind.as_str() {
                        "node"      => ReferencedElement::Node(ref_id),
                        "way"       => ReferencedElement::Way(ref_id),
                        "relation"  => ReferencedElement::Relation(ref_id),
                        _ => { panic!(); /* TODO error */ }
                      };

                      relation.members.push(RelationMember {
                        role: role,
                        element: element,
                      });

                      // Make sure to close <member /> off
                      match parser.next().unwrap() {
                        Ok(XmlEvent::EndElement { .. }) => {  }
                        _ => { panic!(); /* TODO error */ }
                      }
                    }

                    "tag" => {
                      let mut key: Option<String> = None;
                      let mut value: Option<String> = None;
                      for attr in attributes.into_iter() {
                        match attr.name.local_name.as_str() {
                          "k" => { key = Some(attr.value) }
                          "v" => { value = Some(attr.value) }
                          _ => { if key.is_some() && value.is_some() { break; } }
                        }
                      }
                      relation.tags.push(Tag { key: key.unwrap(), value: value.unwrap() });
                      // Make sure to close <way /> off
                      match parser.next().unwrap() {
                        Ok(XmlEvent::EndElement { .. }) => {}
                        _ => { panic!(); /* TODO error */ }
                      }
                    }

                    &_ => {
                      // Invalid tag in relation
                      // TODO error handling
                      panic!();
                    }
                  }
                }

                _ => { /* TODO error */ }
              }
            }
          }

          _ => {}
        }
      }

      _ => { panic!(); /* TODO error */ }
    }
  }
}

//fn osm_write<T: Write>(w: T) -> Result<_, &str>
