# Atlas77 Roadmap

This document outlines the future direction of the Atlas77 programming language.

---

> [!NOTE]
> This roadmap is subject to change as development progresses. The focus areas and features listed are planned but may evolve based on community feedback and technical considerations. Last updated: **January 2026**

---

## Table of Contents

- [Overview](#overview)
- [Version Timeline](#version-timeline)
- [Current Focus: v0.7.x Covenant](#current-focus-v07x-covenant)
- [Upcoming: v0.8.x](#upcoming-v08x)
- [Future Directions](#future-directions)
- [Contributing](#contributing)

---

## Overview

Atlas77 is a programming language under active development. Current development focuses on:

| Focus Area | Description |
|------------|-------------|
| **Memory Safety** | Ownership and move/copy semantics |
| **Error Reporting** | Clear error messages and diagnostics |
| **Runtime Performance** | VM architecture improvements |
| **Standard Library** | Core library expansion |

---

## Version Timeline

| Version | Codename | Status | Theme |
|---------|----------|--------|-------|
| v0.7.0 | *Covenant* | Released | Ownership & Move/Copy Foundation |
| v0.7.1 | *Covenant* | Released | Copy Constructor Rework |
| v0.7.2 | *Covenant* | Released | Stabilization |
| v0.7.3 | *Covenant* | In Progress | Generic Constraints |
| v0.7.4 | *Covenant* | Planned | Error Recovery & Diagnostics |
| v0.7.5 | *Covenant* | Planned | Method-Level Generics |
| v0.8.x | *TBD* | Planned | VM Redesign & Async |
| v0.9.x | *TBD* | Future | Package System |
| v0.10.x | *TBD* | Future | Optimizations |
| v0.11.x | *TBD* | Future | Advanced Features |
| v1.0.x | *TBD* | Future | Production Ready |

---

## Current Focus: v0.7.x *Covenant*

The v0.7.x series represents a major milestone in Atlas77's evolution, introducing foundational safety features that will shape the language's future.

---

### Release Breakdown

#### v0.7.0 — Foundation Release
> **Status:** Released

The initial release establishing ownership and move/copy semantics.

**Key Changes:**
- Introduction of move semantics for automatic resource management
- Foundation for ownership tracking in the compiler
- Based on [PR #141](https://github.com/atlas77-lang/atlas77/pull/141)

**Technical Details:**
- Variables are now moved by default when passed to functions
- Compiler tracks ownership throughout the program's lifetime
- Automatic cleanup when values go out of scope

---

#### v0.7.1 — Copy Semantics Rework
> **Status:** Released

Complete rework of how copying works in Atlas77.

**Key Changes:**
- Complete overhaul of `_copy` constructor semantics
- Introduction of explicit copy behavior flags

**New Features:**

| Flag | Purpose | Behavior |
|------|---------|----------|
| `#[std::copyable]` | Force a struct to be copyable | Compiler attempts to make the struct copyable if possible |
| `#[std::non_copyable]` | Prevent copying | Struct cannot be copied even if all fields are copyable |

**Example Usage:**
```atlas77
import "std/io";

// This struct must be copyable - compiler will enforce this
#[std::copyable]
struct Point {
public:
    x: int64;
    y: int64;
    Point(x: int64, y: int64) {
        this.x = x;
        this.y = y;
    }
}

// This struct cannot be copied, even though it could be
#[std::non_copyable]
struct UniqueHandle {
public:
    id: int64;
    UniqueHandle(id: int64) {
        this.id = id;
    }
}
```

**Rules:**

* A struct is only considered `std::copyable` if all its fields are also `std::copyable`. If any field is not copyable, the compiler will raise an error.
* A struct with no flag is only copyable if all its fields are copyable AND there is no destructor defined.
> Why? Because destructors imply resource management that copying could violate.

---

#### v0.7.2 — Stabilization
> **Status:** Released

Focus on stability and bug fixes.

**Key Changes:**
- Bug fixes for move/copy semantics edge cases
- The copy constructor is now callable directly like this: `let b = new Point(&a);`
- Blocks have been added to control variable scope more explicitly

---

#### v0.7.3 — Generic Constraints
> **Status:** Currently in Development

Introduction of constraint system for generic programming.

**Key Feature: Method-Level Constraints**

Allow methods on generic structs to have constraints based on the generic parameters:
> You can also add constraints to the copy constructor itself.

```atlas77
struct Vector<T> {
public:
    data: [T];
    length: uint64;
    
    Vector(data: [T]) {
        this.data = data;
        this.length = len(&data);
    }
    
    // This method only exists if T is copyable
    fun clone(&this) -> Vector<T> 
      where T: std::copyable 
    {
        // ... implementation
    }
    
    // This method is always available
    fun length(&this) -> uint64 {
        return this.length;
    }
}
```

**Benefits:**
- More expressive generic programming
- Compile-time enforcement of type requirements
- Better error messages when constraints are not met
- Enables more advanced standard library features

---

### Other Planned v0.7.x Features

These features are planned for the v0.7.x series but are not tied to a specific sub-release:

| Feature | Description | Complexity | Notes |
|---------|-------------|------------|-------|
| **Error Recovery** | Compiler continues after certain errors | High | Parser needs full rework for this |
| **Improved Diagnostics** | Better error messages with suggestions | Medium | Ongoing effort |
| **Method-Level Generics** | Generic parameters on methods | Medium | Builds on v0.7.3 work |

> [!WARNING]
> **Error Recovery Limitation:** The parser will likely be the only pass that cannot recover from errors. A full parser rework is required to enable this capability, which may be scheduled for a future release.

---

## Upcoming: v0.8.x
> No codename yet

The v0.8.x series represents a major architectural shift focused on runtime and execution model improvements.

### Primary Goal: VM Redesign

The virtual machine will be completely rebuilt from scratch with better design principles.

#### New VM Architecture

The new VM will feature:
- Typed instruction set with type information
- Register-based execution model (vs current stack-based)
- Stack allocation for primitives and structs
- Improved heap allocation and garbage collection
- Native async/await support
- Closure and lambda support

### Planned Features

| Feature | Description | Impact |
|---------|-------------|--------|
| **Typed VM** | Instructions carry type information | Better optimization potential |
| **Register-Based** | Moving from stack-based to register-based | Improved performance |
| **Stack Allocation** | Primitives and structs on the stack | Reduced heap pressure |
| **Improved Heap Model** | Better allocation and collection | More predictable performance |
| **async/await** | First-class async support | Modern concurrency patterns |
| **Closures & Lambdas** | Anonymous functions with captures | Required for async, enables functional patterns |
| **LIR Introduction** | Low-level Intermediate Representation | Better optimization target |

### New Compilation Pipeline

The compilation pipeline will be extended with a new Low-level Intermediate Representation (LIR):

1. Source Code (.atlas)
2. Lexer
3. Parser  
4. HIR (High-level IR - existing)
5. LIR (NEW: Low-level IR)
6. Bytecode
7. New VM

### Why LIR?

The introduction of LIR (Low-level Intermediate Representation) provides several benefits:

1. **Optimization Passes** — Easier to implement optimizations at a lower level
2. **Target Flexibility** — Could potentially target native code in the future
3. **Cleaner Separation** — Clear boundary between high-level and low-level concerns
4. **Better Analysis** — Enables more sophisticated program analysis

> [!IMPORTANT]
> v0.7 and v0.8 are the only releases currently **clearly scoped and planned** in this order. Everything beyond is subject to change.

---

## Future Directions

> Everything after v0.8 is not set in stone, both in scope and in scheduling. These are directions rather than commitments.

### v0.9.x — Package Ecosystem

Focus: Building a package management system

| Feature | Description |
|---------|-------------|
| **Package Manager** | First-party tool for managing dependencies |
| **Namespacing** | Full namespace support (`import "std/collections/vector";`) |
| **Expanded Standard Library** | More comprehensive built-in functionality |
| **Package Registry** | Central repository for Atlas77 packages |

**Envisioned Package Structure:**
```
my_project/
├── atlas.toml          # Project manifest
├── atlas.lock          # Dependency lock file
├── src/
│   └── main.atlas
└── packages/           # Downloaded dependencies
    └── ...
```

---

### v0.10.x — Optimization

Focus: Compiler and runtime performance improvements

| Optimization | Description | Expected Impact |
|--------------|-------------|-----------------|
| **Dead Code Elimination (DCE)** | Remove unreachable code | Smaller binaries |
| **Constant Folding** | Evaluate constants at compile time | Faster execution |
| **Inlining** | Inline small functions | Reduced call overhead |
| **Loop Unrolling** | Optimize tight loops | Better cache utilization |
| **Escape Analysis** | Stack allocate escaped values when safe | Reduced heap allocations |
| **Tail Call Optimization** | Optimize tail-recursive calls | Prevent stack overflows |

> These optimizations will likely be introduced incrementally across multiple releases.

---

### v0.11.x — Advanced Language Features

Focus: Higher-level language expressiveness

| Feature | Description | Inspiration |
|---------|-------------|-------------|
| **Operator Overloading** | Custom operators for user types | Rust, C++ |
| **Concepts** | Shared behavior definitions | Rust traits, Go interfaces |
| **Generic Methods** | Type parameters on individual methods | Modern generics |
| **Advanced Constraints** | Complex type bounds and where clauses | Rust, Haskell |

**Example of Future Syntax (Conceptual):**
```atlas77
concept Comparable<T> {
    fun compare(&this, other: &T) -> Ordering;
}

struct Point implements Comparable<Point> {
public:
    x: int64;
    y: int64;
    Point(x: int64, y: int64) {
        this.x = x;
        this.y = y;
    }
    fun compare(&this, other: &Point) -> Ordering {
        // ...
    }
}
```

---

### v1.0.x — Production Ready

The path to v1.0 is not yet decided. Potential directions include:

| Direction | Description | Considerations |
|-----------|-------------|----------------|
| **Self-Hosting / Bootstrapping** | Compiler written in Atlas77 | Major undertaking, proves language maturity |
| **Stability Guarantees** | Semver compliance, deprecation policies | Important for production use |
| **Tooling Ecosystem** | IDE support, debugger, profiler | Developer experience |
| **Performance Parity** | Competitive with established languages | Benchmarking, optimization |

---

## Contributing

Contributions to Atlas77 are welcome. Areas of interest include:

- **Bug Fixes** — Help identify and fix bugs
- **Features** — Implement roadmap items
- **Documentation** — Improve guides and examples
- **Testing** — Expand test coverage
- **Ideas** — Propose new features or improvements

Please check our [Contributing Guidelines](CONTRIBUTING_GUIDELINES.md) before opening issues or pull requests.

---

### Progress Overview

| Milestone | Status | Progress |
|-----------|--------|----------|
| v0.7.x Covenant | In Progress | 80% |
| v0.8.x VM Redesign | Planned | 0% |
| v0.9.x Package System | Future | 0% |
| v1.0.x Release | Future | 0% |

This roadmap is maintained by the Atlas77 team and community (Gipson62 alone in both cases 🥸).
