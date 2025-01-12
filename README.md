# Run Programming Language

[Experimental] Object-oriented, compiled, VM-based language designed for optimal performance and portability across different environments.

---

## Quick Start

### 1. Clone the Repository

```bash
git clone <repository-url>
cd run-lang
```

### 2. Build the Compiler and VM

Ensure you have a C compiler (e.g., GCC or Clang) installed. Then run:

```bash
make
```

### 3. Run Your First Program

Create a file named `examples/example.run` and add the following code:

```run
fun returnSum(a, b) {
  return a + b;
}

print returnSum(2, 3);

fun fib(n) {
  if (n < 2) return n;
  return fib(n - 2) + fib(n - 1);
}

var start = clock();
print fib(4);
print clock() - start;
```

Run the program with:

```bash
./build/release/rvm examples/example.run
```

---

## 🛠 Local Development

### Prerequisites

- A C compiler (e.g., GCC, Clang)
- Make

### Build the Project

1. Clone the repository:
   ```bash
   git clone <repository-url>
   cd run-lang
   ```
2. Build the binaries:
   ```bash
   make
   ```

### Testing Changes

After making modifications, recompile and test your changes:

```bash
make clean && make
./build/release/rvm examples/main.run
```

### Code Structure

- **src/**: Contains the source code for the compiler and VM.
- **tests/**: Example `.run` files to validate the language's features.
- **Makefile**: Automates the build process.

---

## Contribute

1. Fork the repo.
2. Create a branch: `git checkout -b feature-name`.
3. Push changes: `git push origin feature-name`.
4. Open a pull request!

Don’t forget to ⭐ the repo if you find it interesting
