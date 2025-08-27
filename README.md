<a id="readme-top"></a>

<!-- PROJECT SHIELDS -->

[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![MIT License][license-shield]][license-url]

<!-- PROJECT LOGO -->
<br />
<div align="center">
  <a href="https://github.com/atlas77-lang/Atlas77">
    <img src="images/logo.png" alt="Logo">
  </a>

<h3 align="center">Atlas77</h3>

  <p align="center">
    Functional Programming language with a strong interop with Rust,
    designed to be a functional scripting language.
    <br />
    <a href="https://atlas77-lang.github.io/atlas77-docs/docs/latest/index.html"><strong>Explore the docs »</strong></a>
    <br />
    <br />
    <a href="https://github.com/atlas77-lang/Atlas77">Playground (inexistant)</a>
    ·
    <a href="https://github.com/atlas77-lang/Atlas77/issues/new?labels=bug&template=bug-report---.md">Report Bug</a>
    ·
    <a href="https://github.com/atlas77-lang/Atlas77/issues/new?labels=enhancement&template=feature-request---.md">Request Feature</a>
  </p>
</div>

<!-- ABOUT THE PROJECT -->

## About The Project

Atlas77 is an experimental **statically typed systems language** designed around a **small core** and **strong interop with Rust**.  
It runs on a **custom VM** and aims to provide a clear, minimal foundation for building higher-level abstractions in libraries rather than in the compiler itself.  

The philosophy is simple: **keep the core language tiny, make everything else userland.**

<!-- GETTING STARTED -->

## Getting Started

### Prerequisites

* Rust Compiler
  ```sh
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
  ```

Or directly from their website: [Rust](https://www.rust-lang.org/tools/install)

### Installation

1. Install it from Cargo
    ```sh
    cargo install atlas_77
    ```

> [!Note]
> I recommend you to build it yourself, as the version on Cargo is not always up to date.
> There are also some bugs in the current version on Cargo.

2. Use it as a CLI
    ```sh
    atlas_77 --help
    ```
3. Enjoy!

<!-- USAGE EXAMPLES -->

## Usage

### Fibonacci Example

```
import "std/io"

fun fib(n: i64) -> i64 {
    if n <= 1 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}

fun main() {
    let n: i64 = 10;
    print(fib(n));
}
```

_For more examples, see the [examples folder](./examples/README.MD)_

<!-- ROADMAP -->

## Roadmap

### v0.3 "Foundation"

> Deprecated, if you want to know more about the v0.3.x, check the releases page.

- [v0.3 "Foundation"](https://github.com/atlas77-lang/Atlas77/releases/tag/v0.3)
- [v0.3.1](https://github.com/atlas77-lang/Atlas77/releases/tag/v0.3.1)

### v0.4 "Keystone"

> Deprecated, if you want to know more about the v0.4, check the releases page.

- [v0.4 "Keystone"](https://github.com/atlas77-lang/Atlas77/tag/v0.4)

### v0.5 "Phoenix"

The v0.5 was supposed to be a complete rewrite of Atlas77, but because of some major design issues in syntax, semantic, memory management, compiler and everything it got scraped after the v0.5.2 (which is still accessible).

> Deprecated, if you want to know more about the v0.5.x, check the releases page.

- [v0.5 "Phoenix"](https://github.com/atlas77-lang/Atlas77/tag/v0.5)
- [v0.5.1](https://github.com/atlas77-lang/Atlas77/tag/v0.5.1)
- [v0.5](https://github.com/atlas77-lang/Atlas77/tag/v0.5.2)

### v0.6.x No Codename yet

The **complete rewrite** of Atlas77 with a much smaller core, designed for correctness and long-term maintainability.

The runtime focuses on a simple GC (refcount + cycle breaking) and Rust-backed interop.

| Feature               | Status | Notes                                                 |
| --------------------- | ------ | ----------------------------------------------------- |
| Functions             | ✅      | First-class, user-defined functions                   |
| Variables             | ✅      | Immutable (`let`) and mutable (`var`)                 |
| Control Flow          | ✅      | `if/else`, `while`                                    |
| Imports               | ✅      | Only `std` imports for now                            |
| Basic `std`           | ✅      | IO, string, math, time, collections                   |
| Structs               | 🔧     | User-defined data types                               |
| Enums / Result\[T, E] | 🔧     | Lightweight ADTs for error handling                   |
| Generics              | 🔧     | Type parameters for reusable code                     |
| Arrays                | 🔧     | Core mutable collection with variadic initialization  |
| GC                    | ✅      | Reference counting + cycle detection                  |
| Rust FFI              | 🔧     | Core of the "everything else in libraries" philosophy |

> NB: This is not really up to date as the rewrite is still taking place

### Later (post v0.6.x)

- Package manager
- LSP integration
- Cranelift JIT
- Trait/typeclasses for shared behaviour

#### Stability and Refinement

> As the language is still in alpha (not 1.0 yet), I won't make "alpha"/"beta" build, it doesn't really make sense.

The beta phase (aka after 0.6.x and beyond) will focus on stabilizing the language. All features will be finalized, tested extensively, and optimized for real-world use. This phase will serve as a release candidate.

See the [open issues](https://github.com/atlas77-lang/Atlas77/issues) for a full list of proposed features (and known issues).


### Philosophy

Atlas77 is not about shipping a huge feature set in the compiler.

It’s about:

- Small core — only functions, variables, control flow, and basic types are truly built-in.
- Interop-first — everything else (collections, advanced types, game engine bindings, etc.) is either written in Atlas77 or delegated to Rust libraries.
- Pragmatic runtime — safety and performance without overengineering (simple GC, direct Rust calls).
- Game dev friendly — language design guided by the needs of writing a simple ECS and game engine in Atlas77.

### Long-Term Goals

- Bootstrapping the compiler in Atlas77 itself
- Building a minimal ECS in pure Atlas77
- Building a simple game engine with Vulkan bindings
- Providing a package manager written in Atlas77
- Ahead-of-time compilation with Cranelift

<!-- CONTRIBUTING -->

## Contributing

Contributions are what make the open source community such an amazing place to learn, inspire, and create. Any
contributions you make are **greatly appreciated**.

If you have a suggestion that would make this better, please fork the repo and create a pull request. You can also
simply open an issue with the tag "enhancement".
Don't forget to give the project a star! Thanks again!

1. Fork the Project
2. Create your Feature Branch (`git checkout -b feature/AmazingFeature`)
3. Commit your Changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the Branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

### Top contributors:

<a href="https://github.com/atlas77-lang/atlas77/graphs/contributors">
  <img src="https://contrib.rocks/image?repo=atlas77-lang/atlas77"/>
</a>

Made with [contrib.rocks](https://contrib.rocks).

<!-- LICENSE -->

## License

Distributed under the MIT License. See `LICENSE.txt` for more information.

<!-- CONTACT -->

## Contact

Your Name - [@Gipson62_8015](https://twitter.com/Gipson62_8015) - J.H.Gipson62@gmail.com

Project Link: [https://github.com/atlas77-lang/Atlas77](https://github.com/atlas77-lang/Atlas77)


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->

[contributors-shield]: https://img.shields.io/github/contributors/atlas77-lang/Atlas77.svg?style=for-the-badge

[contributors-url]: https://github.com/atlas77-lang/Atlas77/graphs/contributors

[forks-shield]: https://img.shields.io/github/forks/atlas77-lang/Atlas77.svg?style=for-the-badge

[forks-url]: https://github.com/atlas77-lang/Atlas77/network/members

[stars-shield]: https://img.shields.io/github/stars/atlas77-lang/Atlas77.svg?style=for-the-badge

[stars-url]: https://github.com/atlas77-lang/Atlas77/stargazers

[issues-shield]: https://img.shields.io/github/issues/atlas77-lang/Atlas77.svg?style=for-the-badge

[issues-url]: https://github.com/atlas77-lang/Atlas77/issues

[license-shield]: https://img.shields.io/github/license/atlas77-lang/Atlas77.svg?style=for-the-badge

[license-url]: https://github.com/atlas77-lang/Atlas77/blob/master/LICENSE.txt
