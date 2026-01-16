# Documentation

## Imports

- std/vector
- std/optional
- std/string
- std/mem

<a id="struct-iter"></a>
## Struct: Iter

 An iterator over an array of `T`.
 Note: For non-copyable types, calling next() will copy the element.

**Fields**

- data: [T]
- index: uint64

**Constructor**

**Copy Constructor**

**Destructor**

**Methods**

- next(&this)

 Returns the next item in the iterator, or None if the end is reached.
 Note: For non-copyable types, this will copy the element.

- peek(&this)

 Peeks at the next item without advancing the iterator

- reverse_inplace(&this)

 Reverses the iterator in place by mutating its internal data array

- reversed(&const this)

 Create a new iterator that yields elements in reverse order
 T must be copyable

- rev(this)

 Consume the iterator and return a new one that yields elements in reverse order
 Delete the old one

- has_next(&this)

- from_string()

- from_str()

- from_vector()

- from_map()

- from_array()

 Mostly for consistency, though it's useless 
 because the constructor does the same

