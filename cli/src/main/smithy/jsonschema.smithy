namespace jsonschema

use smithy4s.api#discriminated
use smithy4s.api#untagged

service SynthService { operations: [SynthOp] }
operation SynthOp { input: Schema }


structure Schema {
  @required
  definitions: Definitions
}

map Definitions {
  key: String,
  value: Definition
}

structure Definition {
  description: String,
  type: Tpe,
  anyOf: DefinitionList,
  properties: Properties,
  const: String,
  items: Definition,
  @jsonName("$ref")
  ref: String,
}

list DefinitionList { member: Definition }

@enum([
  {value: "boolean", name: "BOOLEAN"},
  {value: "null", name: "NULL"},
  {value: "array", name: "ARRAY"},
  {value: "number", name: "NUMBER"},
  {value: "object", name: "OBJECT"},
  {value: "string", name: "STRING"},
])
string DefinitionType

map Properties {
  key: String,
  value: Definition
}

@untagged union Tpe {
  simple: DefinitionType,
  arr: Types
}

list Types { member: DefinitionType }
//below: more generic schema

// structure Schema {
//   definitions: Definitions,
//   properties: Properties,
//   required: Strings,
//   type: SchemaType,
//   const: String,
//   @jsonName("$ref")
//   ref: Ref,
//   @jsonName("$id")
//   id: String,
//   @jsonName("$schema")
//   schema: String,
//   @jsonName("$comment")
//   comment: String,
//   title: String,
//   description: String,
//   enum: Strings,
//   items: Items
// }

// @untagged
// union Items {
//   schema: Schema,
//   schemas: SchemaArray
// }

// string Ref

// list Strings { member: String }

// map Definitions {
//   key: DefinitionName,
//   // technically value is a Schema but recursion inside maps seems to be problematic
//   value: Document
//   // value: Document
// }

// // synthetic
// string DefinitionName


// @untagged
// union SchemaType {
//   simple: SimpleType,
//   simpleArray: SimpleTypeArray,
//   array: SchemaArray,
//   anyOf: SchemaArray
// }

// // structure

// @length(min: 1)
// list SimpleTypeArray {
//   member: SimpleType
// }

// @length(min: 1)
// list SchemaArray {
//   member: Schema
// }

// map Properties {
//   key: PropName,
//   value: Property
// }

// @untagged
// union Property {
//   schema: Schema,
//   // In the case of `default`.
//   // now I really wish we had mixins
//   bool: Boolean
// }

// string PropName

// @enum([
//   {value: "boolean", name: "BOOLEAN"},
//   {value: "integer", name: "INTEGER"},
//   {value: "null", name: "NULL"},
//   {value: "number", name: "NUMBER"},
//   {value: "object", name: "OBJECT"},
//   {value: "string", name: "STRING"},
// ])
// string SimpleType
