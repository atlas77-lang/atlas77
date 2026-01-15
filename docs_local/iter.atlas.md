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

- **Mutable** next

 Returns the next item in the iterator, or None if the end is reached.
 Note: For non-copyable types, this will copy the element.

- **Mutable** peek

 Peeks at the next item without advancing the iterator

- **Mutable** reverse_inplace

 Reverses the iterator in place by mutating its internal data array

- **Const** reversed

 Create a new iterator that yields elements in reverse order
 T must be copyable

- **Consuming** rev

 Consume the iterator and return a new one that yields elements in reverse order
 Delete the old one

- **Mutable** has_next

- **Static** from_string

- **Static** from_str

- **Static** from_vector

- **Static** from_map

- **Static** from_array

 Mostly for consistency, though it's useless 
 because the constructor does the same

