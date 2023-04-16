## acc-saturator

Equality saturation for OpenACC/OpenMP in C.

## Requirements
* Racket (>= 8.4)
* Omni Compiler (>= 20220511)
* Rust (>= 1.60)
* COIN-OR Cbc (= 2.10.0)

Recent versions of Cbc do not provide `libCbcSolver.so`, which is required by egg. Only the version 2.10.0 is tested.

Also,
```
% raco pkg install sxml
```

## Usage
Wrap as below:
```
% accsat nvc ..
```
