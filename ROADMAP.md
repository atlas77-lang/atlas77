# Atlas77 Roadmap

This document outlines the future direction of the Atlas77 programming language.

---

> [!NOTE]
> This roadmap is subject to change as development progresses. The focus areas and features listed are planned but may evolve based on community feedback and technical considerations. Last updated: **January 2026**

---

### Release Breakdown

#### v0.7.x — Covenant (series summary)
> Status: Closing

Key additions in the v0.7.x series:
- Initial move semantics (implicit moves introduced; to be reworked in v0.8)
- Copy-constructor overhaul and copy flags (`#[std::copyable]`, `#[std::non_copyable]`)
- Stabilization fixes across the 0.7 releases
- Generic constraints and method-level generics (ongoing in v0.7.3)

More granular technical details remain in the PRs and release notes for each micro-release.

---

#### v0.7.3 — Generic Constraints
> **Status:** Released

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

**Note:** v0.7.x added implicit move semantics and expanded generics (including method-level constraints). The implicit, always-on ownership model proved too intrusive for the language's safety-opt-in philosophy and will be reworked in v0.8 to prefer explicit moves and clearer constructor semantics.

---

## Upcoming: v0.8.x
> No codename yet

v0.8.x will focus on reworking the memory model introduced in v0.7.x. The goal is to restore the language's safety-opt-in philosophy by making moves explicit, stabilizing constructor semantics, and improving ergonomics for library authors and the standard library.

### Primary Goals

- Rework ownership/move semantics: explicit moves, default-copy by convention
- Stabilize constructors: default, copy, move, and `take` semantics
- Update standard library collections to the new semantics

### Memory Model Rework Highlights

- **Explicit moves via `std::move()`** — Moving a value will require explicit intent; copying remains the default when available.
    - Signature (conceptual): `fun move<T: std::moveable>(dying_obj: &T) -> T`
    - Collections will be migrated to use `std::move` to avoid unsafe implicit transfers.

- **Move constructors** — Constructors that build values from a dying object and invalidate the source.
    - Syntax: `Foo(dying_obj: &Foo)`
    - New constraints: `std::moveable` / `std::non_moveable` alongside copy constraints.
    - Move constructors are invoked via `std::move()`, not via `new Foo(&dying_foo)` to avoid ambiguity with copy constructors.

- **`std::take(dst: &T)`** — Replace `dst` with its default and return the previous value.
    - Signature: `fun take<T: std::default>(dst: &T) -> T`

- **Default constructors & `std::default`** — Support generated `T::default()` where possible and `#[std::non_default]` opt-out.
    - Field defaults supported inline: `field: uint64 = 42;`

- **Reference lifetime tracking remains strict** — Maintain strong lifetime checks (see v0.7.3) while improving ergonomics.

---

## Future Directions

> Everything after v0.8 is not set in stone, both in scope and in scheduling. These are directions rather than commitments.

### v0.9.x — Native Backend & Package Ecosystem

Focus: Native code generation backends and package tooling.

- **Native compilation targets** — Integrate a native backend (Cranelift or LLVM) to generate high-performance code.
    - Rationale: enables bindings for engines and libraries (OpenGL/SDL/raylib), better performance for real-world projects, and a path toward bootstrapping.
    - No garbage collector: the native backend will target predictable, low-overhead memory models.

- **Package manager & namespacing** — Provide a first-party dependency manager, `atlas.toml` manifest, and lockfile support.
- **Package registry & distribution** — Lightweight community registry for discovering and publishing Atlas77 packages.

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
| **Concurrency** | Async/await or channels or coroutines | Rust, Go, Js |
| **Closures / Lambdas** | First-class function literals | Many modern languages |

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
| v0.7.x Covenant | Closing | ~95% |
| v0.8.x Memory Rework | In Progress | 5% |
| v0.9.x Native Backend & Package System | Planned | 0% |
| v1.0.x Release | Future | 0% |

This roadmap is maintained by the Atlas77 team and community (Gipson62 alone in both cases 🥸).
