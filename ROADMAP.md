# Potential Atlas77 Roadmap

> NB: Names are not final

## v0.8.0

**First thing to do:**
- [x] Bitwise copy-by-default, explicit move, explicit copy for types with a destructor
- [x] Remove references
- [x] Finalize syntax

**Compiler:**
- [ ] Type check before monomorphization
- [x] Ownership pass rewrite (warn on move, error on delete)
- [ ] C backend separation
- [x] Intrinsics system
- [ ] Core C bridge (libc bindings)
- [ ] Never type (`!`)

**Language:**
- [ ] Operator overloading
- [x] Generic methods
- [ ] Const generics
- [x] Attributes system (`#[stuff]`)
> Somewhat, it's still fairly locked behind what the compiler allows

**Stdlib:**
- [x] Core utilities (``move``, ``copy``, ``swap``, ``panic``, ``assert``)
> NB: Missing "std::assert"
- [x] ``std::optional<T>``
- [x] ``std::expected<T, E>``
- [x] ``std::string``
- [x] ``std::vector<T>``
- [ ] ``std::array<T, N>``
- [ ] ``std::ptr<T>``

**Tooling:**
- [x] Basic build system

---

## v0.9.0

**Language:**
- [ ] Variadic generic arguments
- [ ] Concepts

**Stdlib:**
- [ ] ``std::shared_ptr<T>``, ``std::unique_ptr<T>``
- [ ] ``std::hash_map<K, V>``, ``std::hash_set<T>``
- [ ] File I/O
- [ ] Iterators
- [ ] `std::variant<T...>`, `std::either<A, B>`, `std::Pair<A, B>`
- [ ] `std::hashable`, `std::sortable`, ...

**Tooling:**
- [ ] Testing framework
- [ ] Documentation generator
- [ ] Better error messages

---

## v1.0.0

**Language:**
- [ ] Pattern matching
- [ ] Closures
- [ ] References with lifetime tracking 
> NB: Not sure of its usefulness. But if they exist, they need to be easier and safer to use than `std::ptr<T>`.
> Why? So it "*forces*" people to use the safer and easier alternative than raw pointers.

**Stdlib:**
- [ ] All containers (`std::queue<T>`, `std::stack<T>`, `std::list<T>`)
- [ ] Math library
- [ ] Time/Date (`std::duration`, `std::instant`, `std::date`)
- [ ] Async/Thread (`std::thread`, `std::future`, `async`?, )
- [ ] Networking

**Graphics lib:**
> Will potentially be added as a separate package and not released directly with the 1.0.0
- [ ] `Vec2`, `Vec3`, `Vec4`
- [ ] `Matrix` 
- [ ] OpenGL, Vk, Dx12/11 support
> Not sure about which one to actually support at first

**Tooling:**
- [ ] Package manager
- [ ] Language server (LSP)
- [ ] Debugger integration