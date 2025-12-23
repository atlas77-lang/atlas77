# Standard Library

This folder contains the standard library for Atlas77. It is still a work in progress, and more libraries will be added in the future.

Currently, it contains two subfolders: `std` and `std/experimental`.

## `std`
- `std/optional`: An idiomatic implementation of `optional<T>` in Atlas77. It is similar to C++'s `std::optional<T>`.
- `std/experimental/expected`: An idiomatic implementation of `expected<T, E>` in Atlas77. It is similar to Rust's `Result<T, E>`.
> [!Note]
> Not everything from `std` will be listed here directly. Refer to the [documentation](https://atlas77-lang.github.io/atlas77-docs/docs/latest/std.html) for more details. (Not everything is up to date yet, e.g.: ``Result`` & ``Option`` have been dropped in favor of ``expected`` & ``optional`` respectively.)

## `std/experimental`

This folder contains all of the experimental library I wanna try out to know which features to add or not in Atlas77.

Currently, it contains:
- `std/experimental/size_of`: A library to get the size of a type at compile time. Similar to C++'s `sizeof(T)`.
- `std/experimental/smart_ptr`: Test implementation of smart pointers (`rc_ptr<T>`, `unique_ptr<T>`, ...) in Atlas77.
- `std/experimental/cast`: I still don't know tbf... I want to have casting like `const_cast<T>`, `static_cast<T>`, ... in Atlas77. And so I will try to implement them here first.
- `std/experimental/range`: An implementation of ranges in Atlas77. Similar to C++20's ranges.
- `std/experimental/sort`: An implementation of sorting algorithms in Atlas77.
- `std/experimental/linked_list`: An implementation of a singly linked list in Atlas77.
- `std/experimental/hashmap`: An implementation of a hash map in Atlas77.

> [!Warning]
> As of writing this, none of these libraries are working. They are still in development or event very early development and may not even compile.

> [!Note]
> Despite writing "idiomatic implementation", there might still be a lot of work to be done, as even I am not sure what is idiomatic in Atlas77 yet. So feel free to give feedbacks on these implementations!
>
> Also, even if it's a small detail, I am still not sure on what case to use for type names. Currently every structs/enums/unions should be in `PascalCase`, though maybe the std library should use `snake_case` instead?