/*
 * This file will contain the C codegen.
 * We will codegen to C from our LIR here.
 * Why C? It's easier to target than LLVM/Cranelift/etc.
 *
 * In the future, I'll potentially target actual backends, but for now, C is good enough.
 */
