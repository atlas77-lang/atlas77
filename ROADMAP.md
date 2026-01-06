## Atlas77 Roadmap Update
> [!NOTE]
> This roadmap is subject to change as development progresses. The focus areas and features listed are planned but may evolve based on community feedback and technical considerations.
### v0.7.x — *Covenant*
This is the next major milestone and is actively being worked on.
**Focus areas**:
* Introduction of **move/copy semantics** to enable safer code and automatic scope cleanup
* Rework of the `_copy` constructor semantics
* Expansion and stabilization of parts of the standard library
* Introduction of `memcpy<T>(&T) -> T` for explicit shallow copies
* Addition of a **HIR pretty printer** to inspect the output of the compiler after the last HIR pass.
### v0.8.x
This release will focus on **runtime and execution model changes**, including:
* Reworking the VM from **stack-based to register-based**
* Moving toward a **typed VM**
* Improving the heap with more realistic allocation behavior
> v0.7 and v0.8 are the only releases that are currently clearly scoped and planned in this order.
### Beyond v0.8.x
> Everything after v0.8 is **not set in stone**, both in scope and in scheduling. These are directions rather than commitments:
* v0.9.x: package system improvements, namespacing (`std::foo::bar`), larger std, package manager
* v0.10.x: optimization passes (DCE, constant folding, inlining, loop unrolling, etc.), likely introduced incrementally
* v0.11.x: higher-level language features (operator overloading, traits/interfaces, generic methods, constraints)
* v1.0.x: undecided; possibly bootstrapping, possibly something else entirely