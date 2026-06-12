# Language Idea

## References

```rust
enum Access {
    Shared,
    Unique,
}

enum Mutability {
    Immutable,
    Mutable,
}

// &'a shared imm T
// The same as rusts `&UnsafeCell<T>`
//
// &'a shared mut T
// The same as rusts `&UnsafeCell<T>`
//
// &'a unique imm T
// Unique is the wrong term for this, because there can be many other immutable references existing alongside it, just not any mutable references
// The same as rusts `&T`
//
// &'a unique mut T
// the same as rusts `&mut T`
//
// Any unique reference can be converted to a shared reference
// `&_ _ T` can be used for type inference/automatic genericness
type Ref['a, type T, const ACCESS: Access, const MUTABILITY: Mutability] = ...

// &'a own T
// An owning reference that drops the T when its dropped
// similar to rusts `Box` except it doesnt manage an allocation
type OwnRef['a, type T] = ...

// &'a out T
// An out reference that cannot be dropped, it can only be written to or split into many sub-out-references (for writing to individual members/array elements)
// writing to it consumes the reference
// The compiler and unsafe code can rely on the value being written to after the 'a lifetime ends
type OutRef['a, type T] = ...
```

## Enums

```rust
// unlike rust, enums with data use the same syntax as structs
enum Option[T] {
    some: T,
    none, // : Unit can be inferred
}

let foo = Option { some: 5 } // construction syntax also looks like making a struct
let bar = Option { none: Unit {} }
let baz = Option::none {} // same as above, syntax sugar for constructing a variant and specifying the members of the variant type

// NOTE: reassigning enums requires having a unique mutable reference,
// because reassigning them could potentially invalidate references that exist pointing to their active variant
//
// There may be an attribute added later for relaxing that requirement so that any mutable reference can reassign the enum,
// but that will come at the cost of not being able to make references to the variant data without having a unique (mutable or immutable) reference
```

## Traits

```rust
// "traits" are just regular structs
struct Clone[T] {
    // `self` isnt a special name here
    clone: fn(self: &shared imm T) -> T,
}
```
```rust
struct MyStruct {}

// the `using` makes this value the default for Clone[MyStruct]
using Clone[MyStruct] { // this is just making a value of the Clone struct, you could put a variable or constant here instead of the struct literal
    clone: fn(self: &shared imm MyStruct) -> MyStruct {
        MyStruct {} // return a new struct here, this could be any copying logic
    }
}

let foo = MyStruct {}
let bar = foo.clone() // because a value of Clone[MyStruct] is `using`ed, you can just call the function in the struct like this

let clone_impl: Clone[MyStruct] = _ // this will find the default value in scope for this type and try to move it into this variable
let baz = clone_impl.clone(&foo) // this also works for calling the method explicitly
```

## Implicit parameters

```rust
// anything in `[]` is implicit parameters
fn foo[x: I32]() {
    // ...
}

// explicitly passing a value
foo[5]()

let value: I32 = 6
using value // make this the default value in scope

foo() // this will pass `value` for the implicit parameter
```

## Runtime

Unlike rust, `const fn` does not exist, instead all functions are "usable" at compile time and there is a `Runtime` type to replace the concept


Doing things like FFI, accessing static variables, etc can only happen at runtime,
so the `Runtime` value is required if you want to do those things, to make sure you cant do those things at compile time

```rust
// this is an empty struct, but it cannot be constructed manually
// its kinda like a "runtime effect" from some languages, but here its just a regular value
type Runtime = ...

// this print function takes value of Runtime, because it can only be called at runtime
fn print[runtime: Runtime](value: I32) {
    // whatever platform-specific printing stuff, does not matter
}

// the only way to get an initial value of Runtime is to get it passed to the main function, magic'ed into existence by the language
fn main(using runtime: Runtime) {
    // this works because the runtime value is `using`ed
    print(5)

    // the runtime value can be copied as many times as you want if you want to do multiple runtime-only things
    print(10)
}
```

## Compiletime

```rust
// similar to Runtime, but with a lifetime attached so that it cant be kept around into runtime
type Compiletime['a] = ...

// a const block gives you a Compiletime value that is `using`ed, and with a lifetime so it cannot be used after this scope
const {
    // you could do stuff in here like using a compile-time-only allocator, etc
}
```

## Generic function pointers

```rust
// this generic function pointer must be a constant so that the compiler can monomorphise it
// if its not constant then the function pointer is useless and cant be called
fn call_function(const f: fn[type T](value: T) -> T) {
    let foo: I32 = 5
    let bar: I32 = f(foo) // works

    let baz: I64 = 6
    _ = f(baz) // this works too, it can be called with any type
}

fn identity[type T](value: T) -> T {
    value
}
call_function(identity) // passing a generic function as a parameter
```
