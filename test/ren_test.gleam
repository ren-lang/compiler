import gleeunit

/// Gleeunit and Gleam's test runner work by doing ✨ magical things ✨ to detect
/// all the files in the project that end in `_test.gleam`. To do that we need to
/// create a dummy Gleam program that kicks things off.
///
/// ❗️ Don't put tests in this module! Tests should go live in either the `unit/`
/// or `e2e/` directories depending on what kind of test they are.
///
pub fn main() {
  gleeunit.main()
}
