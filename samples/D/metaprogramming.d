enum GenStruct(string name, string fieldName) =
    "struct " ~ name ~ "{ int " ~ fieldName ~ "; }";

// Equivalent to:  struct Foo { int bar; }
mixin(GenStruct!("Foo", "bar"));

void main() {
    Foo f;
    f.bar = 10;
}
