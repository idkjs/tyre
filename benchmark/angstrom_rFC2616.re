[@ocaml.warning "-44"];
/* HTTP "parser", copied from angstrom. */
open Angstrom;

module P = {
  let is_space =
    fun
    | ' '
    | '\t' => true
    | _ => false;

  let is_eol =
    fun
    | '\r'
    | '\n' => true
    | _ => false;

  let is_hex =
    fun
    | '0' .. '9'
    | 'a' .. 'f'
    | 'A' .. 'F' => true
    | _ => false;

  let is_digit =
    fun
    | '0' .. '9' => true
    | _ => false;

  let is_separator =
    fun
    | ')'
    | '('
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '\\'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '='
    | '{'
    | '}'
    | ' '
    | '\t' => true
    | _ => false;

  let is_token =
    /* The commented-out ' ' and '\t' are not necessary because of the range at
     * the top of the match. */
    fun
    | '\000' .. '\031'
    | '\127'
    | ')'
    | '('
    | '<'
    | '>'
    | '@'
    | ','
    | ';'
    | ':'
    | '\\'
    | '"'
    | '/'
    | '['
    | ']'
    | '?'
    | '='
    | '{'
    | '}' /* | ' ' | '\t' */ => false
    | _ => true;
};

let token = take_while1(P.is_token);
let digits = take_while1(P.is_digit);
let spaces = skip_while(P.is_space);

let lex = p => p <* spaces;

let version =
  string("HTTP/")
  *> lift2((major, minor) => (major, minor), digits <* char('.'), digits);

let uri = take_till(P.is_space);

let meth = token;
let eol = string("\r\n");

let request_first_line =
  lift3(
    (meth, uri, version) => (meth, uri, version),
    lex(meth),
    lex(uri),
    version,
  );

let response_first_line =
  lift3(
    (version, status, msg) => (version, status, msg),
    lex(version),
    lex(take_till(P.is_space)),
    take_till(P.is_eol),
  );

let header = {
  let colon = char(':') <* spaces;
  lift2((key, value) => (key, value), token, colon *> take_till(P.is_eol));
};

let request =
  lift2(
    ((meth, uri, version), headers) => (meth, uri, version, headers),
    request_first_line <* eol,
    many(header <* eol) <* eol,
  );

let response =
  lift2(
    ((version, status, msg), headers) => (version, status, msg, headers),
    response_first_line <* eol,
    many(header <* eol) <* eol,
  );
