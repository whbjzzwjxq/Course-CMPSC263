# Course-CMPSC263
Course project repo for CMPSC 263 in 22 Fall

## Members:
- Hongbo Wen

## Slides Link:
https://docs.google.com/presentation/d/17q9rNSQ2cZBCKv4MzekYXfFZ8Py8iuWbtR6uj7_d4BE/edit#slide=id.g1a199eefdc7_5_0

## Topic
An Interpreter/Optimizer for LLVM IR based on Racket/Rosette
1. LLVM IR Extracter based on LLVM Pass
2. LLVM IR Interpreter based on Racket and Rosette, supporting symbolic execution.
3. LLVM IR Optimizer based on Rosette and Regraph

## Racket Setup
**NOTICE** Install Racket's Suite following: https://racket-lang.org/

Install Rosette: raco pkg install rosette
Install Regraph: raco pkg install regraph
Install Global: raco pkd install global
zhash is provided by git submodule

```bash
raco pkg install rosette
raco pkg install regraph
raco pkd install global
git clone --recurse-submodules this-repo
```

## Run Examples
```bash
racket ./impl/main.rkt --input ./outputs/example-basic-functioncall.json --debug
racket ./impl/main.rkt --input ./outputs/example-opt-rewrite1.json --debug --optimize
```

## Result Clarify
You could see:
```Racket
Interpreter returns
    (ite (bvsle x$0 (bv #x00000080 32))
        (bv #x00000000 32)
        (bvadd (bvmul x$1 x$1) (bvadd (bvmul x$0 x$0) (bvmul x$0 (bvmul (bv #x00000002 32) x$1)))))
Optimizer returns
    (ite (bvsle x$0 (bv #x00000080 32))
        (bv #x00000000 32)
        (bvadd x$0 x$1))
```
This means the else branch of the program is optimized from
```Racket
(bvadd (bvmul x$1 x$1) (bvadd (bvmul x$0 x$0) (bvmul x$0 (bvmul (bv #x00000002 32) x$1)))))
```
to
```Racket
(bvadd x$0 x$1))
```

`ite` is the intrinsic representation of `if` in the rosette.

## Optimization
1. Path merge.
2. Constant folding.
3. Rewritten by e-graph.

For more about e-graph: https://egraphs-good.github.io/

## Used 3rd Party Packages Referrence
Rosette https://github.com/emina/rosette
Zhash https://github.com/chyanju/zhash
Regraph https://github.com/herbie-fp/regraph
Global https://docs.racket-lang.org/global/index.html

### LLVM Installation
**NOTICE** You could skip this if you just want to execute the examples.
Use `Ninja` as the build tool default, so you need to install one before building LLVM.
If you don't have, please follow the offcial guide from LLVM at: https://github.com/llvm/llvm-project/tree/release/13.x .
**NOTICE** that the building command used by us is different from the standard one.
**NOTICE** the path/to/llvm-project should be an abstract path, such as /User/You/app/llvm-project.
**NOTICE** DLLVM_TARGETS_TO_BUILD depends on your architecture.
```bash
git clone --depth 1 --branch release/13.x git@github.com:llvm/llvm-project.git
cd ./llvm-project
cmake -S llvm -B build -G Ninja \
-DLLVM_TARGETS_TO_BUILD="{X86||ARM||RISCV}" \
-DCMAKE_BUILD_TYPE=Release \
-DLLVM_PARALLEL_LINK_JOBS=1 \
-DLLVM_OPTIMIZED_TABLEGEN=ON \
-DCMAKE_EXPORT_COMPILE_COMMANDS=ON
cmake --build build/
export LLVM_PATH=path/to/llvm-project/build
export PATH=$PATH:$LLVM_PATH/bin
```

### Build LLVM Pass
**NOTICE** Make sure the LLVM is installed.
```bash
cd $LLVM_PATH
ln -s path/to/IRExtractor ./llvm/lib/Transforms/IRExtractor
echo "add_subdirectory(IRExtractor)" >> ./llvm/lib/Transforms/CMakeLists.txt
cmake --build build/
```

### Generate JSON Files
Use pass directly:
```
opt -load -enable-new-pm=0 path-to-lib/IRExtractor.so --IRExtractor input.bc 1> /dev/null 2> output.json
```
Also could use script:
```
python3 ./gen_ir.py --debug -i ./examples -o ./outputs
```
