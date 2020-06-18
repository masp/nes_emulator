extern crate enum_string;

use enum_string::FromString;
use std::collections::HashMap;
use unicase::UniCase;

#[derive(FromString, PartialEq, Debug, Copy, Clone)]
enum Test {
    ABC,
    DFG,
    HIJ
}

#[test]
fn converts_to_string() {
    assert_eq!(Test::ABC.to_string(), "ABC");
    assert_eq!(Test::DFG.to_string(), "DFG");
}

#[test]
fn converts_from_string() {
    assert_eq!(Test::from_string("ABC"), Some(Test::ABC));
    assert_eq!(Test::from_string("BAD"), None);
}