use std::io::prelude::*;

use xml::reader::{EventReader, XmlEvent};
use xml::attribute::OwnedAttribute;
use xml::common::Position;

use osm_primitives::*;

// TODO proper error handling with our custom Error type
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
      "id"        => { id         = Some(value.parse::<i64>().unwrap()); }
      "user"      => { user       = Some(value.to_owned());              }
      "uid"       => { uid        = Some(value.parse::<u64>().unwrap()); }
      "version"   => { version    = Some(value.parse::<u32>().unwrap()); }
      "changeset" => { changeset  = Some(value.parse::<u64>().unwrap()); }
      "timestamp" => { timestamp  = Some(value.to_owned());              }
      _ => {} // Can safely be ignored.
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
  version: Option<String>,
  generator: Option<String>,
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
  MissingAttribute(ErrorPosition, &'static str),
  UnexpectedElement { position: ErrorPosition, expected: Option<Vec<String>>, got: String },
  ParserError(ErrorPosition, xml::reader::Error),
  UnexpectedCData(ErrorPosition),
  UnexpectedCharacters(ErrorPosition),
  InvalidRelationMemberType(ErrorPosition, String)
}

struct ReaderIterator<T: Read>(EventReader<T>);

/// Non-validating [OSM XML](https://wiki.openstreetmap.org/wiki/OSM_XML) parser.
///
/// You can be certain that all elements will be served in the following order, as guaranteed by the OSM XML format:
/// 1. All nodes
/// 2. All ways
/// 3. All relations
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

impl<T: Read> Iterator<Item = Result<Element, Error>> for Reader<T> {
  fn next(&mut self) -> Option<Self::Item> {
    let parser = &mut self.parser;

    let next = parser.next();
    if next.is_none() { return Ok(None); } // Last element has already been served, we are now at the end.
    // TODO handle closing osm element

    match next.unwrap() {
      Ok(XmlEvent::StartElement { name, attributes, .. }) => {
        match name.local_name.as_str() {
          "node"      => { return Some(parse_node      (parser, attributes).into()); }
          "way"       => { return Some(parse_way       (parser, attributes).into()); }
          "relation"  => { return Some(parse_relation  (parser, attributes).into()); }

          _ => {}
        }
      }

      _ => { panic!(); /* TODO error */ }
    }
  }
}

/// Advances an iterator until a XmlEvent::EndElement is consumed, skipping any whitespace, comments and ProcessingInstruction's.
/// It returns an error if any other kind of XmlEvent is encountered.
fn expect_end_element<T: Read>(parser: &mut EventReader<T>) -> Option<Result<Element, Error>> {
  loop { // loop until either unexpected thing or closing tag is encountered.
    match parser.next().unwrap() {
      Ok(XmlEvent::EndElement { .. })             => { break; }
      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::Characters { .. })             => { return Some(Err(Error::UnexpectedCharacters(  ErrorPosition::Rough(parser.position()))));       }
      Ok(XmlEvent::CData { .. })                  => { return Some(Err(Error::UnexpectedCData(       ErrorPosition::Rough(parser.position()))));       }
      Ok(XmlEvent::StartElement { name, .. })     => { return Some(Err(Error::UnexpectedElement { position: ErrorPosition::Rough(parser.position()), expected: None, got: name.local_name }));       }
      Err(err)                                    => { return Some(Err(Error::ParserError(           ErrorPosition::Rough(parser.position()), err)));  }
    }
  }
}

fn parse_node<T>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Node, Error> {
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

  if let None = lat { return Err(Error::MissingAttribute(ErrorPosition::Rough(parser.position()), "lat")); }
  if let None = lon { return Err(Error::MissingAttribute(ErrorPosition::Rough(parser.position()), "lon")); }

  expect_end_element(parser)?;
  return Ok(Some(Element::Node(
    Node {
      lat: lat.unwrap(),
      lon: lon.unwrap(),
      tags: Vec::new(),
      metadata: meta,
    }
  )));
}

fn parse_way<T>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Way, Error> {
  let mut way = Way {
    metadata: metadata(attributes.iter()).unwrap(),
    tags: Vec::new(),
    nodes: Vec::new()
  };

  loop { // Read all node references and tags
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
            expect_end_element(parser)?;
          }

          "tag" => {
            let mut key: Option<String> = None;
            let mut value: Option<String> = None;
            for attr in attributes { // TODO optimize
              match attr.name.local_name.as_str() {
                "k" => { key = Some(attr.value); }
                "v" => { value = Some(attr.value); }
                _ => {} // Can be safely ignored
              }
            }
            way.tags.push(Tag { key: key.unwrap(), value: value.unwrap() });

            // Make sure to read EndElement for <tag />
            expect_end_element(parser)?;

          }
          _ => { return Err(Error::UnexpectedElement { position: ErrorPosition::Rough(parser.position()), expected: None, got: name.local_name }); }
        }
      }

      Ok(XmlEvent::EndElement { .. }) => { break; } // Encountered </way>, exit from node & tag reading loop

      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(  ErrorPosition::Rough(parser.position())));       }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(       ErrorPosition::Rough(parser.position())));       }
      Err(err)                                    => { return Err(Error::ParserError(           ErrorPosition::Rough(parser.position()), err));  }
    }
  }

  return Ok(way);
}

fn parse_relation<T>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Relation, Error> {
  let mut relation = Relation {
    metadata: metadata(attributes.iter()).unwrap(),
    members: Vec::new(),
    tags: Vec::new(),
  };

  loop { // Loop reading all members, tags etc
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

            let kind    = if let Some(kind)   = kind_maybe    { kind }    else { return Err(Error::MissingRelationMemberType(ErrorPosition::Rough(parser.position()))); };
            let ref_id  = if let Some(ref_id) = ref_id_maybe  { ref_id }  else { return Err(Error::MissingRelationMemberRef(ErrorPosition::Rough(parser.position()))); };
            let role    = if let Some(role)   = role_maybe    { role }    else { return Err(Error::MissingRelationMemberRole(ErrorPosition::Rough(parser.position()))); };

            let element = match kind.as_str() {
              "node"      => ReferencedElement::Node(ref_id),
              "way"       => ReferencedElement::Way(ref_id),
              "relation"  => ReferencedElement::Relation(ref_id),
              other => { return Err(Error::InvalidRelationMemberType(ErrorPosition::Rough(parser.position()), kind)); }
            };

            relation.members.push(RelationMember {
              role: role,
              element: element,
            });

            // Make sure to close <member /> off
            expect_end_element(parser)?;
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
            expect_end_element(parser)?;
          }

          &_ => { /* ignore unrecognized element within <relation>...</relation>*/ }
        }
      }


      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(  ErrorPosition::Rough(parser.position())));       }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(       ErrorPosition::Rough(parser.position())));       }
      Err(err)                                    => { return Err(Error::ParserError(           ErrorPosition::Rough(parser.position()), err));  }

      // Encountered </relation> end element
      Ok(XmlEvent::EndElement { .. })             => { break; }
    }
  }

  return Ok(relation);
}
