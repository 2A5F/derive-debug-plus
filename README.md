# derive-debug

This crate implements a more customizable version of `#[derive(Debug)]`.

### ***This is a fork of [derive-debug](https://github.com/Rob2309/derive-debug)***  
#### The difference
- alias allow expr
- expr option

# Usage
The usage is very similar to `#[derive(Debug)]` with a few extra customization options.

## Deriving a struct
```rust
use derive_debug::Dbg;

#[derive(Dbg)]
struct Foo {
    field_a: u32,
    #[dbg(placeholder = "...")]
    field_b: Vec<u32>, // will be printed as "field_b: ..."
    #[dbg(skip)]
    field_c: bool, // will be left out
    #[dbg(alias = "my_string")]
    field_d: u32, // will be printed as "my_string: 42"
    #[dbg(fmt = "{:#06X}")]
    field_e: u32, // will be printed with the specified format
}
```

# Deriving an enum
```rust
use derive_debug::Dbg;

#[derive(Dbg)]
enum Foo {
    VariantA(u32, u32, u32),
    #[dbg(skip)]
    VariantB{a: u32, b: bool}, // Will be printed as "VariantB"

    // same options available as for struct fields
    VariantC{a: u32, #[dbg(placeholder = "...")] b: bool}
}
```

## Detailed options
### Field Options
- `#[dbg(skip)]` completely omits a field in the output
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo {
        field_a: bool,
        #[dbg(skip)]
        field_b: u32,
    }

    // Outputs: Foo { field_a: true }
```
- `#[dbg(placeholder = "xyz")]` will print `xyz` instead of the actual contents of a field
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo {
        field_a: bool,
        #[dbg(placeholder = "...")]
        field_b: u32,
    }

    // Outputs: Foo { field_a: true, field_b: ... }
```
- `#[dbg(alias = "some_alias")]` will print `some_alias` as field name instead of the real name
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo {
        field_a: bool,
        #[dbg(alias = "not_field_b")]
        field_b: u32,
        #[dbg(alias = &format("expr_{}", "b"))]
        field_c: u32,
    }

    // Outputs: Foo { field_a: true, not_field_b: 42, expr_b: 137 }
```
- `#[dbg(fmt = "{:#06X}")]` will print the field with the specified format
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo {
        field_a: bool,
        #[dbg(fmt = "{:#06X}")]
        field_b: u32,
    }

    // Outputs: Foo { field_a: true, field_b: 0x002A }
```
- `#[dbg(formatter = "my_func")]` will print the field using the specified function.  
The function has to return a type that can be formatted using "{}"
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo(u32, #[dbg(formatter = "fmt_not_zero")] u32);

    fn fmt_not_zero(v: &u32) -> &'static str {
        if *v == 0 {
            "0"
        } else {
            "not 0"
        }
    }

    // Outputs: Foo(42, not 0)
```
- `#[dbg(expr = &"123")]` will pass the expression directly into
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    struct Foo {
        field_a: bool,
        #[dbg(expr = &"123")]
        field_b: u32,
    }

    // Outputs: Foo { field_a: true, field_b: "123" }
```

### Enum Variant Options
- `#[dbg(skip)]` only prints the name of the variant and omits its contents
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    enum Foo {
        #[dbg(skip)]
        SomeVariant{a: bool, b: u32},
    }

    // Outputs: SomeVariant
```
- `#[dbg(alias = "some_alias")]` will use `some_alias` as variant name instead of the real name
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    enum Foo {
        #[dbg(alias = "NotSomeVariant")]
        SomeVariant{a: bool, b: u32},
    }

    // Outputs: NotSomeVariant { a: true, b: 42 }
```

### struct Options
- `#[dbg(alias = "MyAlias")]` will use `MyAlias` as struct name instead of the real name
```rust
    use derive_debug::Dbg;

    #[derive(Dbg)]
    #[dbg(alias = "NotFoo")]
    struct Foo {
        field_a: bool,
        field_b: u32,
    }

    // Outputs: NotFoo { field_a: true, not_field_b: 42 }
```
