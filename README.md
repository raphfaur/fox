# Fox

Fox is a imperative language designed to be both interpreted and compiled.  

## Parser
The [src/Parser.hs](parser) is based on combinator parser. It does not rely on famous library like parsec or so.

## Interpreter 
The [src/Backend.hs](interpreter) is a monadic one. It is based on a State monad and supports partial Runtime Error such as undefined variables.

## Compiler
The [src/Codegen.hs](compiler) compiles to the LLVM 15 IR. It is based on top of the `llvm-hs` package, which is a submodule of this project.
This allows one to compile to any target or to apply smart optimizations.

## Building the project
The project is based on llvm-hs, which we will build from source.
To do so, clone the submodules : 
```sh 
git submodule init
```
Once done, make sure you have LLVM installed on your machine. A 15.x version is required. On MacOS, just run `brew install llvm@15` 

Just run :
```sh
make
```

## Running the parser
You have sample files in the `sample` directory. To parse the file `hello.fox`, just run :  
```sh
./fox parse samples/fibo.fox
```
## Running the interpreter
You have sample files in the `sample` directory. To run the file `hello.fox`, just run :  
```sh
./fox run samples/hello.fox
```
## Compiling the file to LLVM IR
You have sample files in the `sample` directory. To compile the file `hello.fox`, just run :  
```sh
./fox compile samples/hello.fox
```
This will generate a `fox.lc` file inside the `samples` folder.

### Compiling to host binary
To build the object files, run :
```
llc -filetype=obj samples/fox.lc -o samples/fox.o -opaque-pointers
```

To compile the final binary (using clang): 
```
clang samples/fox.o -o foxy
```

