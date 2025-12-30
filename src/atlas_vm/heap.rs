use crate::atlas_vm::error::{RuntimeError, RuntimeResult};
use crate::atlas_vm::object::{Object, ObjectIndex, ObjectKind};
use std::fmt;
use std::fmt::{Display, Formatter};

pub const HEAP_DEFAULT_SIZE: usize = 8192; //In number of objects

/// Memory statistics for the heap
#[derive(Debug, Clone)]
pub struct HeapStats {
    pub total_allocations: usize,
    pub total_deallocations: usize,
    pub current_usage: usize,
    pub peak_usage: usize,
    pub heap_size: usize,
}

pub struct Heap {
    memory: Vec<Object>,
    pub free: ObjectIndex,
    pub used_space: usize,
    // Memory tracking
    allocations: usize,
    deallocations: usize,
    peak_usage: usize,
}

impl Heap {
    pub fn new(space: usize) -> Self {
        Self {
            free: ObjectIndex::new(0),
            memory: (0..space)
                .map(|x| Object {
                    kind: ObjectKind::Free {
                        next: ObjectIndex::new((x + 1) % space),
                    },
                })
                .collect(),
            used_space: 0,
            allocations: 0,
            deallocations: 0,
            peak_usage: 0,
        }
    }
    pub fn clear(&mut self) {
        for (idx, obj) in self.memory.iter_mut().enumerate() {
            obj.kind = ObjectKind::Free { next: self.free };
            self.free = ObjectIndex::new(idx);
        }
        self.used_space = 0;
        self.allocations = 0;
        self.deallocations = 0;
        self.peak_usage = 0;
    }

    pub fn put(&mut self, object: ObjectKind) -> Result<ObjectIndex, RuntimeError> {
        //println!("Allocating object: {:?}", object);
        let idx = self.free;
        let v = self.memory.get_mut(usize::from(self.free)).unwrap();
        let repl = std::mem::replace(v, Object { kind: object });

        match repl {
            Object {
                kind: ObjectKind::Free { next },
                ..
            } => {
                self.free = next;
                self.used_space += 1;
                self.allocations += 1;
                if self.used_space > self.peak_usage {
                    self.peak_usage = self.used_space;
                }
                Ok(idx)
            }
            _ => Err(RuntimeError::OutOfMemory),
        }
    }

    pub fn free(&mut self, index: ObjectIndex) -> RuntimeResult<()> {
        let next = self.free;
        let v = self.memory.get_mut(usize::from(index)).unwrap();

        let repl = std::mem::replace(
            v,
            Object {
                kind: ObjectKind::Free { next },
            },
        );
        let res = match repl {
            Object {
                kind: ObjectKind::Free { .. },
                ..
            } => Err(RuntimeError::NullReference(format!(
                "Double free detected at index {}",
                usize::from(index)
            ))),
            _ => {
                self.used_space = self.used_space.saturating_sub(1);
                self.deallocations += 1;
                Ok(())
            }
        };
        self.free = index;
        res
    }

    /// Get memory statistics
    pub fn get_stats(&self) -> HeapStats {
        HeapStats {
            total_allocations: self.allocations,
            total_deallocations: self.deallocations,
            current_usage: self.used_space,
            peak_usage: self.peak_usage,
            heap_size: self.memory.len(),
        }
    }

    /// Count currently allocated (non-free) objects
    pub fn count_allocated_objects(&self) -> usize {
        self.memory
            .iter()
            .filter(|obj| !matches!(obj.kind, ObjectKind::Free { .. }))
            .count()
    }

    /// Count objects by type
    pub fn count_objects_by_type(&self) -> (usize, usize, usize) {
        let mut strings = 0;
        let mut structures = 0;
        let mut lists = 0;

        for obj in &self.memory {
            match &obj.kind {
                ObjectKind::Free { .. } => {}
                ObjectKind::String(_) => strings += 1,
                ObjectKind::Structure(_) => structures += 1,
                ObjectKind::List(_) => lists += 1,
            }
        }

        (strings, structures, lists)
    }

    /// Print memory leak report
    pub fn print_memory_report(&self) {
        let stats = self.get_stats();
        let (strings, structures, lists) = self.count_objects_by_type();

        println!("\n=== MEMORY REPORT ===");
        println!("Total allocations:   {}", stats.total_allocations);
        println!("Total deallocations: {}", stats.total_deallocations);
        println!("Peak usage:          {} objects", stats.peak_usage);
        println!("Current usage:       {} objects", stats.current_usage);
        println!("  - Strings:         {}", strings);
        println!("  - Structures:      {}", structures);
        println!("  - Lists:           {}", lists);

        // String constants are expected to remain (they're literals)
        // Real leaks are structures/lists that weren't freed
        let real_leaks = structures + lists;
        if real_leaks > 0 {
            println!("⚠️  MEMORY LEAK: {} object(s) not freed!", real_leaks);
        } else if strings > 0 {
            println!(
                "✓ No object memory leaks! ({} string constants remain)",
                strings
            );
        } else {
            println!("✓ No memory leaks detected!");
        }
        println!("=====================\n");
    }

    #[inline(always)]
    pub fn get(&mut self, index: ObjectIndex) -> RuntimeResult<ObjectKind> {
        let obj = self.memory[usize::from(index)].kind.clone();
        Ok(obj)
    }

    #[inline(always)]
    pub fn get_mut(&mut self, index: ObjectIndex) -> RuntimeResult<&mut ObjectKind> {
        let kind = &mut self.memory[usize::from(index)].kind;
        Ok(kind)
    }

    #[inline(always)]
    pub fn raw(&self) -> &[Object] {
        &self.memory
    }

    #[inline(always)]
    pub fn raw_mut(&mut self) -> &mut [Object] {
        &mut self.memory
    }
}

impl Display for Heap {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for (i, obj) in self.memory.iter().enumerate() {
            if let Object {
                kind: ObjectKind::Free { .. },
                ..
            } = obj
            {
                continue;
            }
            writeln!(f, "\t{}: {}", i, obj)?;
        }
        Ok(())
    }
}
