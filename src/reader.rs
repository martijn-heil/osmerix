use std::io::prelude::*;

use xml::reader::{EventReader, XmlEvent};
use xml::attribute::OwnedAttribute;
use xml::common::Position;

use osm_primitives::*;

use crate::DocumentMetadata;

fn metadata<'a, T>(attributes: T) -> Result<ElementMetadata, Error> where T: Iterator<Item = &'a OwnedAttribute> {
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
    id: id.unwrap(),
    user: user.unwrap(),
    uid: uid.unwrap(),
    version: version.unwrap(),
    changeset: changeset.unwrap(),
    timestamp: timestamp.unwrap() // TODO proper error handling
  })
}

/// Type definition may change over time, but unless a breaking change is made the struct is always
/// guaranteed to have (at least) the following two members: `row: u64` and `column: u64`.
pub type TextPosition = xml::common::TextPosition;

#[derive(Debug)]
pub enum Error {
  MissingRelationMemberType(TextPosition),
  MissingRelationMemberRole(TextPosition),
  MissingRelationMemberRef(TextPosition),
  MissingAttribute(TextPosition, &'static str),
  UnexpectedElement { position: TextPosition, expected: Option<Vec<String>>, got: String },
  ParserError(TextPosition, xml::reader::Error),
  UnexpectedCData(TextPosition),
  UnexpectedCharacters(TextPosition),
  UnexpectedEndElement(TextPosition),
  InvalidRelationMemberType(TextPosition, String),
  OsmElementNotFound(),
  UnexpectedStartDocument(TextPosition),
}

/// Non-validating [OSM XML](https://wiki.openstreetmap.org/wiki/OSM_XML) parser.
///
/// You can be certain that all elements will be served in the following order, as guaranteed by the OSM XML format:
/// 1. All nodes
/// 2. All ways
/// 3. All relations
///
/// However, if the file doesn't have them in proper order, you won't receive them in proper order either.
pub struct Reader<T: Read> {
  parser: EventReader<T>,
  pub metadata: DocumentMetadata,
}

fn get_document_metadata<T: Read>(parser: &mut EventReader<T>) -> Result<DocumentMetadata, Error> {
  loop {
    let next = parser.next();
    match next {
      Ok(XmlEvent::StartElement { name, attributes, .. }) => {
        if name.local_name == "osm" {
          let mut metadata = DocumentMetadata {
            generator: None,
            version: None,
          };

          for attr in attributes.iter() {
            match attr.name.local_name.as_str() {
              "generator" => { metadata.generator = Some(attr.value.to_owned()); }
              "version" => { metadata.version = Some(attr.value.to_owned()); }
              _ => {}
            }
          }
          return Ok(metadata);
        } else {
          return Err(Error::UnexpectedElement { position: parser.position(), expected: Some(vec![String::from("osm")]), got: name.local_name });
        }
      }

      Ok(XmlEvent::EndElement { .. })             => { return Err(Error::UnexpectedEndElement(parser.position())); }
      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore
      Ok(XmlEvent::StartDocument { .. })          => {} // Ignore.. for now TODO maybe we should make sure that we only receive this the first time and only once?

      Ok(XmlEvent::EndDocument { .. })            => { return Err(Error::OsmElementNotFound());                                                        }
      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(  parser.position()));      }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(       parser.position()));      }
      Err(err)                                    => { return Err(Error::ParserError(           parser.position(), err)); }
    }
  }
}

impl<T: Read> Reader<T> {
  pub fn new(r: T) -> Result<Self, Error> {
    let mut parser = EventReader::new(r);

    Ok(Reader {
      metadata: get_document_metadata(&mut parser)?,
      parser: parser,
    })
  }
}

impl<T: Read> Iterator for Reader<T> {
  type Item = Result<Element, Error>;

  fn next(&mut self) -> Option<Self::Item> {
    let parser = &mut self.parser;

    loop {
      let next = parser.next();
      // TODO handle closing osm element

      match next {
        Ok(XmlEvent::StartElement { name, attributes, .. }) => {
          match name.local_name.as_str() {
            "node"      => { return Some(parse_node      (parser, attributes).map(|okval| okval.into())); }
            "way"       => { return Some(parse_way       (parser, attributes).map(|okval| okval.into())); }
            "relation"  => { return Some(parse_relation  (parser, attributes).map(|okval| okval.into())); }

            _ => {
              match expect_end_element(parser) {
                Err(err) => { return Some(Err(err)); }
                _ => {}
              }
            }
          }
        }

      Ok(XmlEvent::EndElement { name, .. }) => {
        match name.local_name.as_str() {
          "osm" => { return None; }
          _ => { return Some(Err(Error::UnexpectedEndElement(parser.position()))); }
        }
      }
      Ok(XmlEvent::StartDocument { .. })          => { return Some(Err(Error::UnexpectedStartDocument(parser.position()))); }
      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::Characters { .. })             => { return Some(Err(Error::UnexpectedCharacters(parser.position()))); }
      Ok(XmlEvent::CData { .. })                  => { return Some(Err(Error::UnexpectedCData(parser.position())));     }
      Err(err)                                    => { return Some(Err(Error::ParserError(parser.position(), err)));    }
      } // end match
    } // end loop
  }
}

/// Advances an iterator until a XmlEvent::EndElement is consumed, skipping any whitespace, comments and ProcessingInstruction's.
/// It returns an error if any other kind of XmlEvent is encountered.
fn expect_end_element<T: Read>(parser: &mut EventReader<T>) -> Result<(), Error> {
  loop { // loop until either unexpected thing or closing tag is encountered.
    match parser.next() {
      Ok(XmlEvent::EndElement { .. })             => { return Ok(()); }
      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(parser.position())); }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(parser.position())); }
      Ok(XmlEvent::StartElement { name, .. })     => { return Err(Error::UnexpectedElement { position: parser.position(), expected: None, got: name.local_name }); }
      Ok(XmlEvent::StartDocument { .. })          => { return Err(Error::UnexpectedStartDocument(parser.position())); }
      Err(err)                                    => { return Err(Error::ParserError(parser.position(), err)); }
    }
  }
}

fn parse_node<T: Read>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Node, Error> {
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

  if let None = lat { return Err(Error::MissingAttribute(parser.position(), "lat")); }
  if let None = lon { return Err(Error::MissingAttribute(parser.position(), "lon")); }

  let mut node = Node {
    lat: lat.unwrap(),
    lon: lon.unwrap(),
    tags: Vec::new(),
    metadata: meta,
  };

  loop { // Read all tags
    match parser.next() {
      Ok(XmlEvent::StartElement { name, attributes, .. }) => {
        match name.local_name.as_str() {
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
            node.tags.push(Tag { key: key.unwrap(), value: value.unwrap() });

            // Make sure to read EndElement for <tag />
            expect_end_element(parser)?;

          }
          // TODO be able to ignore unrecognized elements?
          _ => { return Err(Error::UnexpectedElement { position: parser.position(), expected: None, got: name.local_name }); }
        }
      }

      Ok(XmlEvent::EndElement { .. })             => { break; } // End of node
      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::StartDocument { .. })          => { return Err(Error::UnexpectedStartDocument(parser.position())); }
      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(parser.position())); }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(parser.position())); }
      Err(err)                                    => { return Err(Error::ParserError(parser.position(), err)); }
    }
  }

  Ok(node)
}

fn parse_way<T: Read>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Way, Error> {
  let mut way = Way {
    metadata: metadata(attributes.iter()).unwrap(),
    tags: Vec::new(),
    nodes: Vec::new()
  };

  loop { // Read all node references and tags
    match parser.next() { // Can't have an empty way anyway..
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
          _ => { return Err(Error::UnexpectedElement { position: parser.position(), expected: None, got: name.local_name }); }
        }
      }

      Ok(XmlEvent::EndElement { .. }) => { break; } // Encountered </way>, exit from node & tag reading loop

      Ok(XmlEvent::Comment { .. })                => {} // Ignore
      Ok(XmlEvent::Whitespace { .. })             => {} // Ignore
      Ok(XmlEvent::ProcessingInstruction { .. })  => {} // Ignore

      // Don't think this is possible, considering it should throw an EOF error from the parser.
      Ok(XmlEvent::EndDocument { .. })            => { panic!("Unexpected XmlEvent::EndDocument, this should not even be possible!"); }

      Ok(XmlEvent::StartDocument { .. })          => { return Err(Error::UnexpectedStartDocument(parser.position())); }
      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(parser.position())); }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(parser.position())); }
      Err(err)                                    => { return Err(Error::ParserError(parser.position(), err)); }
    }
  }

  return Ok(way);
}

fn parse_relation<T: Read>(parser: &mut EventReader<T>, attributes: Vec<OwnedAttribute>) -> Result<Relation, Error> {
  let mut relation = Relation {
    metadata: metadata(attributes.iter()).unwrap(),
    members: Vec::new(),
    tags: Vec::new(),
  };

  loop { // Loop reading all members, tags etc
    match parser.next() {
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

            let kind    = if let Some(kind)   = kind_maybe    { kind }    else { return Err(Error::MissingRelationMemberType(parser.position())); };
            let ref_id  = if let Some(ref_id) = ref_id_maybe  { ref_id }  else { return Err(Error::MissingRelationMemberRef(parser.position()));  };
            let role    = if let Some(role)   = role_maybe    { role }    else { return Err(Error::MissingRelationMemberRole(parser.position())); };

            let element = match kind.as_str() {
              "node"      => ReferencedElement::Node(ref_id),
              "way"       => ReferencedElement::Way(ref_id),
              "relation"  => ReferencedElement::Relation(ref_id),
              _other => { return Err(Error::InvalidRelationMemberType(parser.position(), kind)); }
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
      Ok(XmlEvent::StartDocument { .. })          => { return Err(Error::UnexpectedStartDocument(parser.position())); }

      Ok(XmlEvent::Characters { .. })             => { return Err(Error::UnexpectedCharacters(  parser.position()));       }
      Ok(XmlEvent::CData { .. })                  => { return Err(Error::UnexpectedCData(       parser.position()));       }
      Err(err)                                    => { return Err(Error::ParserError(           parser.position(), err));  }

      // Encountered </relation> end element
      Ok(XmlEvent::EndElement { .. })             => { break; }
    }
  }

  return Ok(relation);
}
