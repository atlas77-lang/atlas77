# Atlas77 SSA (Static Single Assignment) Intermediate Representation

## Overview

This document describes a comprehensive SSA-form IR designed for Atlas77, to be used as a lowering target after all HIR passes (type checking, monomorphization, ownership analysis, dead code elimination).

## Design Goals

1. **Feature Complete**: Cover all Atlas77 language features post-HIR
2. **Optimization Friendly**: Enable standard SSA-based optimizations
3. **Memory Semantics Aware**: Preserve ownership/move/copy semantics from ownership pass
4. **Backend Agnostic**: Support lowering to VM bytecode, LLVM, or native code

---

## Type System

### Primitive Types

```
SsaType ::=
    | i64              // 64-bit signed integer
    | u64              // 64-bit unsigned integer  
    | f64              // 64-bit floating point
    | bool             // boolean
    | char             // 32-bit unicode code point
    | unit             // void/unit type
    | string           // heap-allocated string (object)
```

### Compound Types

```
SsaType ::=
    | [T]              // heap-allocated array of T
    | &T               // mutable reference to T
    | &const T         // read-only reference to T
    | struct(name)     // named struct type (fully monomorphized)
    | union(name)      // named union type (fully monomorphized)
    | ptr              // raw heap pointer (ObjectIndex)
    | fn(params) -> R  // function pointer type
```

### Type Notes

- All generic types are **fully monomorphized** at this stage
- Named types reference struct/union descriptors by their mangled name (e.g., `__atlas77__struct__Vector__int64`)
- References are raw pointers with semantic annotations

---

## Value Representation

### SSA Values

Every value has exactly one definition point (Single Static Assignment):

```
SsaValue ::= 
    | %<id>            // Virtual register (e.g., %0, %1, %temp)
    | @<name>          // Global/function reference (e.g., @main, @Vector_push)
    | #<const>         // Immediate constant (e.g., #42, #3.14, #"hello")
```

### Value Properties

```rust
struct SsaValue {
    id: ValueId,
    ty: SsaType,
    // Ownership tracking (preserved from HIR ownership pass)
    ownership: Ownership,
}

enum Ownership {
    Owned,              // Value owns its data, responsible for cleanup
    Moved,              // Value has been moved, invalid to use
    Borrowed,           // Temporary borrow, no ownership transfer
    BorrowedMut,        // Mutable borrow
    Copy,               // Bitwise copy (primitives)
}
```

---

## Program Structure

### Module

```
SsaModule {
    name: String,
    
    // Constant pool
    constants: Vec<SsaConstant>,
    
    // Type descriptors (monomorphized structs/unions)
    structs: Vec<SsaStructDescriptor>,
    unions: Vec<SsaUnionDescriptor>,
    
    // External function declarations
    externs: Vec<SsaExternFn>,
    
    // Function definitions
    functions: Vec<SsaFunction>,
    
    // Entry point
    entry: FunctionId,
}
```

### Struct Descriptor

```
SsaStructDescriptor {
    name: String,               // Mangled name
    fields: Vec<(String, SsaType)>,
    
    // Method references (already lowered to standalone functions)
    constructor: FunctionId,
    destructor: FunctionId,
    copy_fn: Option<FunctionId>,  // _copy method if copyable
    methods: Vec<FunctionId>,
}
```

### Union Descriptor

```
SsaUnionDescriptor {
    name: String,               // Mangled name
    variants: Vec<(String, SsaType)>,
    // All variants share the same memory location
}
```

### Function

```
SsaFunction {
    name: String,
    params: Vec<(String, SsaType)>,
    return_ty: SsaType,
    is_external: bool,
    
    // CFG represented as basic blocks
    blocks: Vec<SsaBasicBlock>,
    entry_block: BlockId,
}
```

### Basic Block

```
SsaBasicBlock {
    id: BlockId,
    
    // Phi nodes at block entry (for SSA form)
    phis: Vec<SsaPhi>,
    
    // Instructions (no control flow except terminator)
    instructions: Vec<SsaInstruction>,
    
    // Block terminator (exactly one)
    terminator: SsaTerminator,
}
```

---

## Instructions

### Core Operations

#### Constants & Loads

```
%result = const <type> <value>
    // Load immediate constant
    // Examples:
    //   %0 = const i64 42
    //   %1 = const f64 3.14159
    //   %2 = const bool true
    //   %3 = const string "hello"

%result = load <type> %ptr
    // Load value from memory through pointer/reference
    // Example: %val = load i64 %ref

store %ptr, %value
    // Store value to memory through pointer/reference
    // Example: store %arr_elem_ptr, %new_value
```

#### Arithmetic (type-specific, post type-checking)

```
%result = add.<type> %lhs, %rhs      // i64, u64, f64
%result = sub.<type> %lhs, %rhs
%result = mul.<type> %lhs, %rhs
%result = div.<type> %lhs, %rhs
%result = mod.<type> %lhs, %rhs
%result = neg.<type> %operand        // unary negation
```

#### Comparison

```
%result = eq.<type> %lhs, %rhs       // all comparable types
%result = ne.<type> %lhs, %rhs
%result = lt.<type> %lhs, %rhs       // i64, u64, f64
%result = le.<type> %lhs, %rhs
%result = gt.<type> %lhs, %rhs
%result = ge.<type> %lhs, %rhs
```

#### Logical (bool only)

```
%result = and %lhs, %rhs
%result = or %lhs, %rhs
%result = not %operand
```

#### Type Conversion

```
%result = cast %value to <target_type>
    // Primitive type conversion
    // Example: %f = cast %i to f64
```

### Memory Operations

#### Allocation

```
%result = alloc_obj <struct_name>
    // Heap allocate a struct instance
    // Returns: ptr to uninitialized struct
    // Example: %obj = alloc_obj Vector__int64

%result = alloc_array <elem_type>, %size
    // Heap allocate an array
    // Returns: ptr to array
    // Example: %arr = alloc_array i64, %n

%result = alloc_string %content
    // Heap allocate a string (from constant or runtime)
    // Example: %s = alloc_string #"hello"
```

#### Deallocation

```
free %ptr
    // Deallocate heap object (does NOT call destructor)
    // The ownership pass ensures destructors are called via explicit calls

delete %value
    // Full deletion: calls destructor then frees
    // This is what `delete expr;` in source becomes
    // For primitives: no-op
    // For objects: call destructor, then free
```

#### Field Access

```
%result = get_field_ptr %obj_ptr, <field_index>
    // Get pointer to struct field
    // Returns: &T where T is field type
    // Example: %name_ptr = get_field_ptr %person, 0

%result = get_field %obj_ptr, <field_index>
    // Load field value directly
    // Example: %name = get_field %person, 0
    
set_field %obj_ptr, <field_index>, %value
    // Store value to field
    // Example: set_field %person, 0, %new_name
```

#### Array/Index Access

```
%result = get_elem_ptr %arr_ptr, %index
    // Get pointer to array element
    // Returns: &T where T is element type
    
%result = get_elem %arr_ptr, %index
    // Load array element directly
    
set_elem %arr_ptr, %index, %value
    // Store to array element
    
%result = array_len %arr_ptr
    // Get array length (runtime intrinsic)
```

#### String Operations

```
%result = string_get %str_ptr, %index
    // Get character at index
    // Returns: char

string_set %str_ptr, %index, %char
    // Set character at index

%result = string_clone %str_ptr
    // Deep clone string
```

### Reference Operations

```
%result = ref %value
    // Create mutable reference to value
    // Semantics: takes address of local/field/element
    // Example: %r = ref %local_var

%result = const_ref %value
    // Create read-only reference

%result = deref %ref_ptr
    // Dereference a reference
    // Example: %val = deref %ref

store_indirect %ref_ptr, %value
    // Store through reference
```

### Ownership Operations

These instructions are inserted by the ownership pass and preserved in SSA:

```
%result = move %source
    // Transfer ownership from source
    // Source becomes invalid after this
    // For non-copyable types in ownership-consuming positions
    // Example: %owned = move %local_var

%result = copy %source, <copy_fn>
    // Create owned copy via copy function
    // Source remains valid
    // For copyable types (primitives: bitwise, structs: call _copy)
    // Example: %copied = copy %vec, @Vector__int64__copy

%result = memcpy %source
    // Shallow bitwise copy (intrinsic)
    // Used for moving elements out of containers
```

### Function Calls

```
%result = call @<function_name>(%arg0, %arg1, ...)
    // Direct function call
    // Example: %len = call @str_len(%str_ref)

%result = call_indirect %fn_ptr(%arg0, %arg1, ...)
    // Indirect call through function pointer

%result = call_extern @<extern_name>(%arg0, %arg1, ...)
    // Call to external (runtime-provided) function
    // Example: %s = call_extern @input()

%result = call_constructor @<struct_ctor>(%arg0, %arg1, ...)
    // Call struct constructor
    // Allocates + initializes
    // Example: %vec = call_constructor @Vector__int64__ctor(%data)

call_destructor @<struct_dtor>(%obj_ptr)
    // Call struct destructor (before free)
    // Example: call_destructor @Vector__int64__dtor(%vec)

%result = call_method @<method_name>(%this, %arg0, ...)
    // Sugar for method call (lowered to regular call)
    // The `this` parameter handling depends on method signature:
    //   - `this`: value, ownership transferred
    //   - `&this`: mutable reference
    //   - `&const this`: read-only reference
```

### Control Flow Terminators

```
br <block_id>
    // Unconditional branch
    // Example: br %loop_header

br_cond %condition, <true_block>, <false_block>
    // Conditional branch
    // Example: br_cond %cmp, %then_block, %else_block

ret %value
    // Return from function
    // Example: ret %result

ret_void
    // Return unit/void

switch %value, <default_block>, [<val1>: <block1>, <val2>: <block2>, ...]
    // Multi-way branch (for future match expressions)

unreachable
    // Marks unreachable code (optimization hint)
```

### Phi Nodes

For SSA form, phi nodes appear at the beginning of basic blocks with multiple predecessors:

```
%result = phi <type> [%val1, <block1>], [%val2, <block2>], ...
    // Select value based on incoming edge
    // Example: 
    //   %i.3 = phi i64 [%i.0, %entry], [%i.2, %loop_body]
```

---

## Control Flow Constructs

### If-Else (lowered to branches)

Source:
```atlas
if condition {
    then_body
} else {
    else_body
}
```

SSA:
```
entry:
    %cond = ...
    br_cond %cond, %then_block, %else_block

then_block:
    ; then_body instructions
    br %merge

else_block:
    ; else_body instructions
    br %merge

merge:
    %result = phi ... ; if needed
```

### While Loop (lowered to branches)

Source:
```atlas
while condition {
    body
}
```

SSA:
```
entry:
    br %loop_header

loop_header:
    %i = phi i64 [%i.init, %entry], [%i.next, %loop_body]
    %cond = ...
    br_cond %cond, %loop_body, %loop_exit

loop_body:
    ; body instructions
    %i.next = add.i64 %i, #1
    br %loop_header

loop_exit:
    ; continue...
```

### Break/Continue

```
break    ->  br %loop_exit
continue ->  br %loop_header
```

---

## Ownership Lowering Examples

### Move Semantics

Source (after ownership pass):
```atlas
let x = new Vector<int64>(data);
let y = move<>(x);  // x is now invalid
```

SSA:
```
%x = call_constructor @Vector__int64__ctor(%data)
%y = move %x
; %x is now marked as Moved, any use would be invalid
```

### Copy Semantics

Source (after ownership pass):
```atlas
let a = new Vector<int64>(data);
let b = copy<>(a);  // a remains valid
```

SSA:
```
%a = call_constructor @Vector__int64__ctor(%data)
%a_ref = const_ref %a
%b = call @Vector__int64___copy(%a_ref)
; %a is still Owned
```

### Destructor Insertion

Source (after ownership pass):
```atlas
fun example() {
    let v = new Vector<int64>(data);
    // ... use v ...
    delete v;  // inserted by ownership pass
}
```

SSA:
```
entry:
    %v = call_constructor @Vector__int64__ctor(%data)
    ; ... use %v ...
    call_destructor @Vector__int64__dtor(%v)
    free %v
    ret_void
```

---

## Union Access (Tagged Union Alternative)

Unions in Atlas77 are untagged (all variants share memory). Access is transparent:

```
%opt = alloc_obj optional__int64
; Union field access - all fields are at offset 0
%data_ptr = get_field_ptr %opt, 0      ; data field (union storage)
%value_ptr = get_field_ptr %data_ptr, 0 ; .value variant
store_indirect %value_ptr, %val
```

---

## External Function Interface

External functions (runtime-provided) are declared but not defined:

```
declare @println(value: any) -> unit
declare @input() -> string
declare @len<T>(data: &const [T]) -> u64
; ... etc
```

Calls use `call_extern`:
```
call_extern @println(%message)
%input = call_extern @input()
```

---

## Intrinsics

Built-in operations that may require special handling:

```
; Memory intrinsics
%result = intrinsic memcpy(%src)
%result = intrinsic array_len(%arr)

; Future: size_of, align_of, etc.
%size = intrinsic size_of<T>
%align = intrinsic align_of<T>
```

---

## Example: Complete Function

### Source (after all HIR passes)

```atlas
fun sum_vector(v: Vector<int64>) -> int64 {
    let total: int64 = 0;
    let i: uint64 = 0u;
    while i < v.length {
        let val: int64 = *(v.get(i));
        total = total + val;
        i = i + 1u;
    }
    delete v;
    return total;
}
```

### SSA Form

```
function @sum_vector(%v: struct(Vector__int64)) -> i64 {
entry:
    %total.0 = const i64 0
    %i.0 = const u64 0
    br %loop_header

loop_header:
    %total.1 = phi i64 [%total.0, %entry], [%total.2, %loop_body]
    %i.1 = phi u64 [%i.0, %entry], [%i.2, %loop_body]
    
    %length_ptr = get_field_ptr %v, 1   ; .length field
    %length = load u64 %length_ptr
    %cond = lt.u64 %i.1, %length
    br_cond %cond, %loop_body, %loop_exit

loop_body:
    ; let val = *(v.get(i))
    %v_ref = ref %v
    %elem_ref = call @Vector__int64__get(%v_ref, %i.1)
    %val = deref %elem_ref
    
    ; total = total + val
    %total.2 = add.i64 %total.1, %val
    
    ; i = i + 1u
    %one = const u64 1
    %i.2 = add.u64 %i.1, %one
    
    br %loop_header

loop_exit:
    ; delete v
    call_destructor @Vector__int64__dtor(%v)
    free %v
    
    ret %total.1
}
```

---

## Optimization Opportunities

With this SSA form, standard optimizations become possible:

1. **Dead Code Elimination** - Remove unused instructions
2. **Constant Propagation** - Fold constant operations
3. **Copy Propagation** - Eliminate redundant copies
4. **Common Subexpression Elimination** - Reuse computed values
5. **Loop Invariant Code Motion** - Hoist invariants out of loops
6. **Inlining** - Inline small functions
7. **Escape Analysis** - Stack allocate non-escaping objects
8. **Move Optimization** - Convert copy → move for last uses (already done in HIR, but can refine)

---

## Implementation Notes

### Instruction Selection Order

When lowering from HIR to SSA:

1. Lower all types first (build type descriptors)
2. Lower function signatures (create function stubs)
3. For each function:
   a. Create entry block
   b. Build SSA for each statement
   c. Handle control flow (create blocks, insert phis)
   d. Verify SSA properties

### SSA Construction

Recommended algorithm: **Cytron et al. SSA construction** using dominance frontiers, or simpler **Braun et al. algorithm** for on-the-fly construction.

### Phi Node Insertion

Insert phi nodes where:
- Multiple control flow paths merge
- A variable is defined differently on different paths

### Critical Edges

May need to split critical edges (edge from block with multiple successors to block with multiple predecessors) to ensure correct phi placement.

---

## Future Extensions

1. **SIMD Types**: `v4f32`, `v2f64`, etc.
2. **Exception Handling**: `invoke`, `landingpad`, `resume`
3. **Coroutines/Async**: `suspend`, `resume` points
4. **Attributes**: `#[inline]`, `#[cold]`, etc.
5. **Debug Info**: Source location metadata

---

## Summary

This SSA IR provides:

- ✅ Complete coverage of Atlas77 features post-HIR
- ✅ Explicit ownership semantics (move/copy/borrow)
- ✅ Type-specialized operations (no runtime type dispatch)
- ✅ Clear separation of allocation, initialization, destruction
- ✅ Standard SSA form for optimization
- ✅ Clean path to VM bytecode or native codegen

The design preserves all semantic information from the ownership pass while providing a clean intermediate form suitable for optimization and code generation.
