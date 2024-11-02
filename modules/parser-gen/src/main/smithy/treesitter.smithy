$version: "2"

namespace treesittersmithy

list NodeTypes {
    member: NodeType
}

structure NodeType {
    @required
    @jsonName("type")
    tpe: TypeName

    @required
    named: Boolean

    @required
    fields: NodeFields = {}

    children: FieldInfo

    @required
    subtypes: NodeTypes = []
}

string TypeName

map NodeFields {
    key: FieldName
    value: FieldInfo
}

string FieldName

structure FieldInfo {
    @required
    multiple: Boolean

    @required
    required: Boolean

    @required
    types: TypeList
}

list TypeList {
    member: TypeInfo
}

// https://github.com/disneystreaming/smithy4s/issues/1618
structure TypeInfo {
    @required
    @jsonName("type")
    tpe: TypeName

    @required
    named: Boolean
}
