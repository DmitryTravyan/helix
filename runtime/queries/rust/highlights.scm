; Identifier conventions

; Assume all-caps names are constants
((identifier) @constant
 (#match? @constant "^[A-Z][A-Z\\d_]+$'"))

; Assume builtin algebraic types Ok, Err, Some, None
((identifier) @enum.builtin
 (#match? @enum.builtin "^Ok$"))
((identifier) @enum.builtin
 (#match? @enum.builtin "^Err$"))
((identifier) @enum.builtin
 (#match? @enum.builtin "^Some$"))
((identifier) @enum.builtin
 (#match? @enum.builtin "^None$"))

; Assume butltin types
((type_identifier) @variable.builtin
 (#match? @variable.builtin "^Self"))
((type_identifier) @variable.builtin
 (#match? @variable.builtin "^String$"))
((type_identifier) @type.builtin
 (#match? @type.builtin "^Option$"))
((type_identifier) @type.builtin
 (#match? @type.builtin"^Result$"))

; Assume bultin traits
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^Send$"))
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^Sync$"))
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^Copy$"))
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^From$"))
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^Into$"))
((type_identifier) @trait.builtin
 (#match? @trait.builtin"^AsRef"))

; Assume crate, super
(crate) @use.builtin
(super) @use.builtin

; Assume trait items
(trait_item (type_identifier) @trait)
(trait_bounds (type_identifier) @trait)
(trait_bounds (generic_type (type_identifier) @trait))

; Assume that uppercase names in paths are types
((scoped_identifier
  path: (identifier) @type)
 (#match? @type "^[A-Z]"))
((scoped_identifier
  path: (scoped_identifier
    name: (identifier) @type))
 (#match? @type "^[A-Z]"))
((scoped_type_identifier
  path: (identifier) @type)
 (#match? @type "^[A-Z]"))
((scoped_type_identifier
  path: (scoped_identifier
    name: (identifier) @type))
 (#match? @type "^[A-Z]"))

; Assume use list identifier
(use_list (identifier) @use.type (#match? @use.type "^[A-Z]"))
(use_list (scoped_identifier (identifier) @use.type (#match? @use.type "^[A-Z]")))
(use_list (use_as_clause) @use.as_clause)
(use_declaration) @use

; Assume struct constructors
(struct_expression (type_identifier) @constructor)

; Assume other uppercase names are enum constructors
; ((identifier) @constructor
;  (#match? @constructor "^[A-Z]"))

; Assume all qualified names in struct patterns are enum constructors. (They're
; either that, or struct names; highlighting both as constructors seems to be
; the less glaring choice of error, visually.)
(struct_pattern
  type: (scoped_type_identifier
    name: (type_identifier) @constructor))

; Function calls

(call_expression
  function: (identifier) @function)
(call_expression
  function: (field_expression
    field: (field_identifier) @function.method))
(call_expression
  function: (scoped_identifier
    "::"
    name: (identifier) @function))

(generic_function
  function: (identifier) @function)
(generic_function
  function: (scoped_identifier
    name: (identifier) @function))
(generic_function
  function: (field_expression
    field: (field_identifier) @function.method))

(macro_invocation
  macro: ((identifier) @variable.builtin
 (#match? @variable.builtin "panic")) "!" @variable.builtin)

(macro_invocation
  macro: (identifier) @function.macro
  "!" @function.macro)

; Scoped identifiers
(call_expression (scoped_identifier) @function)

; Function definitions

(function_item (identifier) @function)
(function_signature_item (identifier) @use)

; Other identifiers

(scoped_type_identifier (scoped_identifier) @use)
(type_identifier) @type
(primitive_type) @type.builtin
(field_identifier) @property

(line_comment) @comment
(block_comment) @comment

(type_arguments
  "<" @punctuation.bracket
  ">" @punctuation.bracket)
(type_parameters
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

(parameter (identifier) @variable.parameter)

(lifetime (identifier) @label)

"as" @keyword
"async" @keyword
"await" @keyword
"break" @keyword
"const" @keyword
"continue" @keyword
"dyn" @keyword
"else" @keyword
"enum" @keyword
"extern" @keyword
"fn" @keyword
"for" @keyword
"if" @keyword
"impl" @keyword
"in" @keyword
"let" @keyword
"loop" @keyword
"macro_rules!" @keyword
"match" @keyword
"mod" @keyword
"move" @keyword
"pub" @keyword
"ref" @keyword
"return" @keyword
"static" @keyword
"struct" @keyword
"trait" @keyword
"type" @keyword
"union" @keyword
"unsafe" @keyword
"use" @keyword
"where" @keyword
"while" @keyword
(mutable_specifier) @keyword
(use_list (self) @keyword)
(scoped_use_list (self) @keyword)
(scoped_identifier (self) @keyword)

(self) @variable.builtin

(char_literal) @string
(string_literal) @string
(raw_string_literal) @string

(boolean_literal) @constant.builtin
(integer_literal) @constant.builtin
(float_literal) @constant.builtin

(const_item) @constant

(escape_sequence) @escape

(attribute_item) @attribute
(attribute_item "[" @attribute "]" @attribute)
(attribute_item (attribute (token_tree "(" @attribute ")" @attribute)))
(inner_attribute_item) @attribute
(inner_attribute_item "[" @attribute "]" @attribute)
(inner_attribute_item (attribute (token_tree "(" @attribute ")" @attribute)))

"*" @operator
"&" @operator
"'" @operator

"(" @punctuation.bracket
")" @punctuation.bracket
"[" @punctuation.bracket
"]" @punctuation.bracket
"{" @punctuation.bracket
"}" @punctuation.bracket
"::" @punctuation.delimiter
":" @punctuation.delimiter
"." @punctuation.delimiter
"," @punctuation.delimiter
";" @punctuation.delimiter
