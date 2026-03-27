> [!Note]
> Everything from here will slowly be moved into `./specs/` whenever I have enough data in one document. I will keep the same structure, but each section will become a document on its own.

# Atlas77 v0.8.0-alpha Specifications and Design
---

# 1 Lexical structure

## 1.1 Source Encoding (UTF-8)
Well... It needs to be a valid UTF-8 string (potentially just ASCI, though I guess having variables in other alphabet could be interesting)

## 1.2 Tokens
I'll list the tokens here later, but the current set of tokens can be found in [`src/atlas_c/atlas_frontend/lexer/token.rs`](./src/atlas_c/atlas_frontend/lexer/token.rs)

## 1.3 Keywords and reserved words
Like for the tokens, you can find them there. Any keywords start with `Kw` in the token list.

## 1.4 Identifiers
Needs to start with a letter or an underscore, than it can be anything from letter, numbers & underscore. I will potentially make it possible for other alphabet to be used if anyone request it.

## 1.5 Literals
TODO: 1.5.1 Integer Literals, 1.5.2 Float Literals, 1.5.3 Boolean Literals, 1.5.4 Character Literals, 1.5.5 String Literals, 1.5.6 Array Literals, 1.5.7 Object Literals

## 1.6 Operators and punctation

## 1.7 Comments

---

# Type System 

## 2.1 Primitive Types 
I list them here, I ain't explaining them rn
### 2.1.1 Integer Types (int8, int16, int32, int64, uint8, uint16, uint32, uint64)
### 2.1.2 Float Types (float32, float64)
### 2.1.3 Boolean Type
### 2.1.4 Character Type (char, Unicode scalar value)
### 2.1.5 Unit Type (void equivalent)
### 2.1.6 Never Type (for diverging functions)
represented by a BANG (`!`)

## 2.2 Compound Types 
### 2.2.1 Tuple Types
It might potentially never exists and only be made through some kind of 
```cpp
struct Tuple<T...> {
    t: T...;
}
```
but that would require me to add variadic argument, which I am not fully fond of.
### 2.2.2 Fixed-Size Array Types (`[T; N]`)
### 2.2.3 Struct Types (layout, alignment, padding)
> [!Note]
> I might replace structs with class and keep struct as plain old data object with no behaviour. Not sure.
### 2.2.4 Union Types (tagged vs untagged, safety implications)
It's an unsafe type by default, so sucks to be you if you find it unsafe
### 2.2.5 Enum Types (C-style)
Just a value
### 2.2.6 Sum Types / Tagged Unions (ADTs, exhaustiveness)
> [!Note]
> I've put this here, but I am not even sure we'll need it. Though it can be useful and safer than manual tagged unions


## 2.3 Pointer and Reference Types
### 2.3.1 Raw Pointer Types
Currently it's `ptr<T>`, but like said early, this needs to be investigated to see how to make it "less convenient" to use compare to potential `rc_ptr<T>` or `unique_ptr<T>`.
### 2.3.2 Reference Types (`&T`, `&const T`)
Can't be null, and I might just go the Rust route of making them const by default and mutable if you ask for it. So you need to specifically ask for added, more "unsafe" behaviour.
### 2.3.3 Pointer Arithmetic (rules, safety, UB conditions)
No idea for now, but that will be fun
### 2.3.4 Null Pointer Semantics
Same as in C I guess, in theory you shouldn't deal with it by default, because you should always prefer using safe stuff
### 2.3.5 Pointer Provenance Model
idk, I'll see about that
### 2.3.6 Fat Pointers (slice pointers, vtable pointers)
There will 

## 2.4 Function Types
Everything for this will exist, I am just not sure how to do it...
### 2.4.1 Function Pointer Types
### 2.4.2 Closure Types
### 2.4.3 Calling Conventions

## 2.5 Type Inference
### 2.5.1 Local Type Inference
### 2.5.2 Inference Limitations
Will be dumb because I am too dumb to implement a proper one
### 2.5.3 Type Holes (explicit `_` inference requests)
That's an idea from Rust, idk if I'll take it, because it might be annoying to manage

## 2.6 Type Classification
I don't know yet

## 2.8 Type Coercions and Conversions
TBD, I want to avoid implicit behaviour as much as possible tbf...
### 2.8.1 Implicit Coercions (what is allowed without cast)
### 2.8.2 Explicit Casts
### 2.8.3 Numeric Conversions (widening, narrowing, truncation)
### 2.8.4 Pointer Casts

---

# 3 Variables and Bindings

## 3.1 Variable Declarations (`let`, `const`)
## 3.2 Binding Patterns
## 3.3 Shadowing Rules
As of now I removed the shadowing rules, but I want it back because it's very useful
## 3.4 Scope Rules
The very simple one ngl
## 3.5 Initialization Requirements (must initialize before use?)
## 3.6 Constants (`const`) - compile-time evaluated
## 3.7 Static Variables - lifetime and safety implications
## 3.8 Destructuring
I would love to have some kind of destructuring syntaxic sugar like Rust does, but I might be too stupid to do it properly...

---

# 4 Expressions and Statements

## 4.1 Expressions
### 4.1.1 Literal Expressions
### 4.1.2 Variable Expressions
### 4.1.3 Block Expressions
### 4.1.4 Operator Expressions

#### Arithmetic
  #### Bitwise
  #### Logical
  #### Comparison
  #### Assignment
  #### Compound Assignment
### 4.1.5 Address-Of Expressions (`&expr`, context-dependent behavior)
### 4.1.6 Dereference Expressions (`*expr`)
### 4.1.7 Field Access Expressions
### 4.1.8 Index Expressions
### 4.1.9 Range Expressions
### 4.1.10 Cast Expressions
### 4.1.11 If Expressions
Yes this needs to become an expression so you can put it anywhere and get its result directly where you want it to be like in Rust.
### 4.1.12 Match Expressions
Same for this
### 4.1.13 Loop Expressions
Same for this-ish
### 4.1.14 Closure Expressions

## 4.2 Statements
### 4.2.1 Expression Statements
### 4.2.2 Let Statements
### 4.2.3 Return Statements
### 4.2.4 Break/Continue Statements
### 4.2.5 Delete Statements (destructor invocation, heap deallocation)
This will be one of the thing I'll absolutely need to get well... A delete is used not to free something from the heap, but just call the destructor, but if it was called on a heap object, it should potentially free it?

## 4.3 🔬 Constructor Expressions
### 4.3.1 Stack Construction (`Foo(args)`)
### 4.3.2 Heap Construction (`new Foo(args)`)
this is mostly `new <expr>` which means to put the right side of the new expression on the heap and "register it to be freed at the end of the scope" so you can technically do: `let five: ptr<int64> = new 5;`
### 4.3.3 Struct Literal Construction (`Foo { .field: value }`)
### 4.3.4 Array Construction (`[T; N]`)
Honestly, I don't know how to do array construction... because `[T; N]` is always a type... ig we'll see. 

## 4.4 Operator Overloading
### 4.4.1 Overloadable Operators
All:tm: operators, I even want to have the `->` operator so I can also overload it for field access on "pointer" like struct. e.g.: `std::rc_ptr<T>(stuff)->field_from_stuff`.
### 4.4.2 Operator Traits/Interfaces
I keep this here, but I think it won't use any traits/interfaces, but mostly `operator+(a: &This, b: &This) -> This`.
### 4.4.3 Overloading Implications for Safety (surprising behaviors)
Avoid being able to do `std::cout << "Hello World" << std::endl;` lmao. But the main part would be to somewhat make people unable to do more than the intended operator job.

---

# 5 Functions

## 5.1 Function Declarations
## 5.2 Function Parameters
## 5.3 Return Types
Force a check to ensure a function returns. I don't care about the whatever demonstration that you can prove a program will return in every branch. C++ is stupid to not force function to finish by a "return" or "throw". It's that easy... Yes it might lead to UB in 0.000000000001% of the cases, but fuck you C++
## 5.4 Parameter Passing Semantics (by value, by reference, ownership transfer)
## 5.5 Variadic Functions
## 5.6 Function Inlining (`#[inline]`, `#[inline(always)]`)
## 5.7 `const` Functions (compile-time evaluation)
## 5.8 Generic Functions (see section 8)
## 5.9 Method Syntax
Redo this section probably
  ### 5.9.1 `&const this` (immutable receiver)
  ### 5.9.2 `&this` (mutable receiver)
  ### 5.9.3 `this` (consuming receiver)
## 5.10 Function Overloading Rules
I'll keep it here, because function overloading can be interesting, but at the same time, I don't really like it.
## 5.11 Diverging Functions (never return, `-> !`)
## 5.12 Closures and Captures

---

# 6 Control Flow

## 6.1 `if` / `else if` / `else`
## 6.2 `match` and Pattern Matching
  ### 6.2.1 Exhaustiveness Checking
  ### 6.2.2 Pattern Syntax
  ### 6.2.3 Guard Clauses
  would be genuinely great to have
  ### 6.2.4 Binding in Patterns
  would be genuinely great to have
## 6.3 `while` Loops
## 6.4 `for` Loops and Iterators
## 6.5 `loop` (infinite loop)
sugar syntax for while(true)
> Requires a "break" statement in its body or a function call that returns Never in it.
## 6.6 `break` with Values
potentially a `break my_var` so you can use loops in expression to have them put their result and continue execution. Like if/else & match in Rust
## 6.7 `continue`
## 6.8 `return`
## 6.9 `defer` (if implemented - scoped cleanup)
I do like the idea of Zig for that ngl. But I ain't sure
## 6.10 Labeled Blocks
Just for funsies, but it will probably never be implemented.

---

# 7 Memory Model

> [!Note]
> I do have some ideas, but this entire section is entirely empty because I don't really know what to do.

---

# 8 Ownership and Lifetime Model

> [!Note]
> I do have some ideas, but this entire section is entirely empty because I don't really know what to do.

My current ideas are only:
* Ownership with value types that can be copied by default. 
* RAII enforced.
* Everything is cleaned up at the end of the scope
* Moving is non destructible like in Rust. It only invalidates the object (though that's up for debate). But a move needs to be an action you manually take through `std::move(foo)`.
* There are the `std::copyable`, `std::moveable`, `std::default` and their counter parts `std::non_copyable`, `std::non_moveable`, `std::non_default` constraints/attributes to enforce how the object will behave.
* Objects all have custom destructors (by default the compiler will generate one for you). The idea is to let you manage the destruction logic easily. Even potential struct as Plain Old Data will get access to default constructor, copy constructor, move constructor, and a destructor, to ensure their behaviours work as best as you want them to work. They just can't carry methods, implements traits or operators.

---

# 9 Type System Advanced Features

> [!Note]
> I do have some ideas, but this entire section is entirely empty because I don't really know what to do.

Right now, I do want to have generics with constraints, potentially lifetimes, but for sure constant literals generics. e.g.: `struct Array<T, const N: uint64>` so we can set the size at compile time with `N`. There will also be constraints/built-in traits with:
* `std::copyable`/`std::non_copyable`
* `std::moveable`/`std::non_moveable`
* `std::default`/`std::non_default`
* `std::destructible`/`std::non_destructible`
* `std::comparable`
* `std::hashable`
* `std::sync`/`std::send`
> Those are considered built-ins for now, but they might be implemented later in the standard library manually. The generic system needs to be expressive enough for that. I don't want to do something as complex as templates, but I still want to be able to cleanly express everything.
I want to have some kind of `const_expr()` expression to be ran at compile time while building all those generic stuff so we can really customize and make all these types tailor made based on what's needed/passed in arguments. There might be some specific syntax to be passed in const_expr, like a different kind of if or whatever so people can clearly see when it will be run.

---

# 10 Error Handling
Everything will be done through `expected<T, E>` & `optional<T>`. Though there will be `panic()` or other methods to cleanly crash your program and report to the console/logs. There will never be any exception in Atlas77.

---

# 11 Concurrency Model
---

# 12 Unsafe Operations
As safety is opt-in, I do want to NOT have `unsafe { }` blocks, but at the same time it's not that bad of an idea ig. Idk, we'll see.

---

# 13 Modules and Namespaces
`package my::package::file` at the top of your file. It needs to follow the tree back to the root package of whatever namespace/module you want. e.g. for std::optional: `package std::optional`, and that file can be anywhere it wants and you can also have multiple files for it. A file without a package item at the top of it won't get read by the compiler.
`import my::package::file` when you want to import features/namespaces it's with the `import` item.

---

# 14 Standard Library 
> [!Warning]
> Does not represent the final product, I just put everything I thought about in here.

## 14.1 Core (--no-std equivalent)
### 14.1.1 Primitive Operations
### 14.1.2 `optional<T>`
### 14.1.3 `expected<T, E>`
### 14.1.4 Basic Traits (copyable, moveable, default, drop)
### 14.1.5 Core Memory Operations (`std::move`, `std::copy`, `std::swap`, `std::take`)
### 14.1.6 `std::mem` package (size_of, align_of, transmute)
### 14.1.7 Atomic Types

## 14.2 Collections
### 14.2.1 `Vec<T>` (growable array)
### 14.2.2 `Array<T, N>` (fixed-size stack array)
### 14.2.3 `Slice<T>` (non-owning view)
### 14.2.4 `String` (owned UTF-8 string)
### 14.2.5 `str` (string view, typedef-ed from `Slice<char>`)
### 14.2.6 `HashMap<K, V>`
### 14.2.7 `HashSet<T>`

## 14.3 Memory Management
> Not the name I'll keep, don't worry, it's only temporary
### 14.3.1 Allocator Interface
### 14.3.2 `Box<T>` (heap-allocated unique value)
### 14.3.3 `Rc<T>` (reference-counted, single-thread)
### 14.3.4 `Arc<T>` (reference-counted, thread-safe)

## 14.4 I/O
### 14.4.1 Standard I/O
### 14.4.2 File I/O
### 14.4.3 Formatted Output

## 14.5 Concurrency Primitives
### 14.5.1 🔬 `Mutex<T>`
### 14.5.2 🔬 `RwLock<T>`
### 14.5.3 🔬 Thread Spawn/Join
### 14.5.4 🔬 Channels

---

# 15 Foreign Function Interface (FFI)

> [!Note]
> This will be done only once everything works lmao, if it ever works

---

# 16 Attributes and Intrinsics

## 16.1 Attribute Syntax (`#[attribute]`)
## 16.2 Built-in Attributes
  ### 16.2.1 `#[repr(...)]` (layout control)
  ### 16.2.2 `#[inline]` / `#[inline(always)]` / `#[inline(never)]`
  ### 16.2.3 `#[unsafe]`
  ### 16.2.4 `#[no_std]` (disable standard library)
  > Not sure about this one... for me it should just be a flag in the compiler.
  ### 16.2.5 `#[deprecated]`
  ### 16.2.6 `#[must_use]`
  ### 16.2.7 `#[std::non_copyable]`/`#[std::copyable]`
  ### 16.2.8 `#[std::non_moveable]`/`#[std::moveable]`
  ### 16.2.9 `#[packed(...)]` 
  > forcing breaking ABI changes to guarantee smallest possible representation
## 16.3 Compiler Intrinsics
  ### `sizeof<T>()`
  ### `alignof<T>()`
  ### `typeof<T>()`
  ### `offset_of<T, field>()`
  ### `bit_cast<T, U>()`

---

# 16 Build System and Toolchain

idk... too far in the future. But better than C++
