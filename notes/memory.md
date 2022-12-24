# WebAssembly Memory

WebAssembly provides a linear memory that you can access using load and store instructions, but you are responsible for managing the memory and ensuring that you do not exceed the bounds of the memory or try to access memory that has not been allocated.

One way to manage memory for the stack is to use a stack pointer that points to the top of the stack. You can then push values onto the stack by decrementing the stack pointer and writing the value to the memory location pointed to by the stack pointer. Similarly, you can pop values off the stack by reading the value at the memory location pointed to by the stack pointer and then incrementing the stack pointer.

You will also need to ensure that you do not exceed the bounds of the memory. One way to do this is to keep track of the maximum stack pointer value, and check that the current stack pointer is within the bounds of the memory before performing any stack operations.

In addition to managing memory for the stack, you will also need to manage memory for any dynamically allocated data. One way to do this is to use a heap data structure, where you can allocate memory on the heap using malloc-style functions and free memory using free-style functions. You will need to ensure that you do not have any memory leaks by freeing any memory that is no longer needed, and that you do not try to access memory that has been freed.

It is also a good idea to add checks for out-of-bounds accesses and other error conditions in your compiler, to help catch and prevent any errors that may occur at runtime.

## Locals

In WebAssembly, "locals" are variables that are stored on the stack and are only accessible within a specific function. They are used to store intermediate values and function-specific data that is needed during the execution of the function.

Locals are defined at the beginning of a function, and each local has a type (e.g., i32 for a 32-bit integer) and a name. The type of a local determines the size of the memory that is allocated for the local on the stack. For example, a local of type i32 will occupy 4 bytes of memory on the stack.

To access a local in WebAssembly, you can use the `get_local` and `set_local` instructions. The `get_local` instruction reads the value of a local from the stack and pushes it onto the top of the stack, while the `set_local` instruction pops a value off the top of the stack and stores it in a local.

Locals are useful for storing intermediate values and function-specific data that is needed during the execution of the function, but they are not intended for storing large amounts of data or for storing data that needs to be accessed by multiple functions. For this, you should use WebAssembly's linear memory, which can be accessed using load and store instructions.

## Function-local data

If you want to pass function-specific data from one function to another as a reference, you can use WebAssembly's linear memory to store the data and pass a pointer to the data as an argument to the second function.

For example, let's say you have a function "first" that has a local variable called "data" that you want to pass to a function "second" as a reference. Here's how you could do it:

1. Allocate memory in WebAssembly's linear memory to store the data. You can use the `memory.grow` instruction to increase the size of the linear memory, and then use the `i32.const` instruction to store the address of the newly allocated memory in a local variable called "ptr".
2. Write the data to the memory location pointed to by "ptr". You can use the `i32.store` instruction to store the data at the specified address in the linear memory.
3. Pass the "ptr" local as an argument to the "second" function. You can use the call instruction to call the second function and pass "ptr" as an argument.
4. In the second function, you can use the `get_local` instruction to read the value of "ptr" from the stack and store it in a local variable. You can then use the `i32.load` instruction to read the data from the memory location pointed to by the local.

This way, you can pass the data from the first function to the second function as a reference and access it using the pointer stored in the "ptr" local.

Note that you will need to ensure that the memory location pointed to by "ptr" remains valid for as long as the second function needs to access the data. You may also want to add checks to ensure that the second function does not try to access memory outside of the bounds of the linear memory.
