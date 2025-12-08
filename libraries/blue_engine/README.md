# Blue Engine Library

Blue Engine is the experimental FFI testbed connecting Atlas77 to Rust.
It serves as one of the core libraries shipped with Atlas77 and can be imported directly:

> ```
> import blue_engine::triangle;
> import blue_engine::Engine;
> import blue_engine::ObjectSettings;
> import blue_engine::call_update_function;
> import std::result;
> import std::time;
> 
> fun main() {
>   let engine = Engine::init();
>   triangle("my_triangle", ObjectSettings::init("default"), &engine.renderer, &engine.objects).expect("Failed to create triangle");
>   let start = Time::now();
>   call_update_function(&engine, "update", start);
> }
> 
> fun update(delta: Time, engine: &Engine) {
>   return;
> }
> ```

The objective is to replicate the public API and general usage patterns of the original Blue Engine crate while adapting
it to Atlas77’s execution model, type system, and VM. This library is under active development. Interfaces may change,
features may remain incomplete, and stability is not guaranteed.

Current scope: a safe Atlas77 wrapper layer built entirely on extern_ptr and extern_fn, exposing Rust engine components
without unsafe surface area in Atlas77.