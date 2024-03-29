# Roslind Problem Solutions in Haskell

![CI](https://github.com/xepaul/HaskellRosalind/actions/workflows/haskellMatix.yml/badge.svg)

[Rosalind Problems Information](https://rosalind.info/problems/list-view/)

Rosalind Command Line [CLI.hs](app/CLI.hs)

Run with
```
❯ cabal run rosalind-cli -- --help
```
```
Up to date
Rosalind CLI

Usage: rosalind-cli COMMAND
  Runs Rosalind problems and commands

Available options:
  -h,--help                Show this help text

Available commands:
  server                   server commands
  problem                  run problem
  solver                   run solver
```
```
❯ cabal run rosalind-cli -- problem --help
```
```
Usage: rosalind-cli problem COMMAND
  run problem

Available options:
  -h,--help                Show this help text

Problem commands:
  hamm                     Execute problem hamm
  rna                      Execute problem rna
  revc                     Execute problem revc
  revc2                    Execute problem revc
  prot                     Execute problem prot
  tran                     Execute problem tran
  frmt                     Execute problem frmt
  orf                      Execute problem orf
  cons                     Execute problem cons
```

An example help for a  problem command

```
❯ cabal run rosalind-cli -- problem --help hamm
```
```
Usage: rosalind-cli problem hamm [(-e|--example) | (-i|--input FILE)] 
                                 [-o|--output-dir OUTPUT_DIR]
  Execute problem hamm

Available options:
  -e,--example             Override input FILE option to use the example input
                           for the problem
  -i,--input FILE          File input (default: "input.txt")
  -o,--output-dir OUTPUT_DIR
                           Write output to FILE with path (default: "./out.txt")
```

An example of running a problem.

```
❯ cabal run rosalind-cli -- problem hamm
```
```
Dataset option: SpecifiedInputFile "input.txt"
Evaluating hamm -> input.txt
Done -> ./out.txt
```


An example of running a solver.


```
❯ cabal run rosalind-cli -- solver --help
```
```
Usage: rosalind-cli solver COMMAND
  run solver

Available options:
  -h,--help                Show this help text

Available commands:
  dna2rna                  Convert DNA to RNA
  dnas2rna                 Convert multiple DNA to RNA
  revc                     Calculate the reverse complement of DNA
  hamm                     Calculate the hamm of 2 DNA sequences
```


```
❯ cabal run rosalind-cli -- solver --help dna2rna
```
```
Usage: rosalind-cli solver dna2rna (-d|--dnaInput DNA)
  Convert DNA to RNA

Available options:
  -d,--dnaInput DNA        DNA input
```

```
❯ cabal run rosalind-cli -- solver dna2rna -d ACGTAACCGGTT
```
```
DNA: ACGTAACCGGTT
RNA: ACGUAACCGGUU
```

An example running the server

```
❯ cabal run rosalind-cli -- run
```
```
Up to date
runserver
Listening on port 8081
Converted AAGCT to AGCTT
127.0.0.1 - - [19/Jan/2022:16:13:42 +0000] "GET /revc/AAGCT HTTP/1.1" 200 - 
```
```
❯ curl http://localhost:8081/revc/AAGCT
"AGCTT"% 
```

# Tests
Run with 
```
cabal test all --test-show-details=direct
```

```
Running 1 test suites...
Test suite Rosalind-test: RUNNING...
test/Driver.hs
  Unit tests Rosalind Rna Hedgehog
    test Motif tripping:                                              OK (0.38s)
        ✓ test Motif tripping passed 100 tests.
  Unit tests Rosalind Rna Hedgehog
    test Fast tripping:                                               OK (0.02s)
        ✓ test Fast tripping passed 100 tests.
  Tests Rosalind
    Unit tests Rosalind Revc
      check sample result:                                            OK
          ✓ check sample result passed 100 tests.
      check revc applied twice is the same as original :              OK
          ✓ check revc applied twice is the same as original  passed 100 tests.
      check revc by checking every conversion :                       OK
          ✓ check revc by checking every conversion  passed 100 tests.
  Unit tests Rosalind Rna Hedgehog
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
    tran file expected :                                              OK
  Tests Rosalind
    Unit tests Rosalind hamm
      hamm :                                                          OK
      check hamm with given mutations:                                OK
        +++ OK, passed 100 tests.
  Unit tests Rosalind Rna Hedgehog
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
    gc file :                                                         OK
  Unit tests Rosalind hamm (Hedgehog)
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
    check hamm with given mutations:                                  OK (0.04s)
        ✓ check hamm with given mutations passed 100 tests.
    check hamm with same =0 mutations:                                OK
        ✓ check hamm with same =0 mutations passed 100 tests.
    check hamm mutations in every base has mutation count of length : OK
        ✓ check hamm mutations in every base has mutation count of length  passed 100 tests.
    hamm file :                                                       OK
    hamm file expected :                                              OK
  Unit tests Rosalind Rna Hedgehog
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
    check if a T exists in the dna strand a U exists in the result:   OK
        ✓ check if a T exists in the dna strand a U exists in the result passed 100 tests.
    check each dnabase is correctly converted to rna:                 OK
        ✓ check each dnabase is correctly converted to rna passed 100 tests.
  Unit tests Rosalind hamm (Hedgehog)
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
    prot file expected :                                              OK
  Unit tests Rosalind Orf (Hedgehog)
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
  Unit tests Rosalind Subs (Hedgehog)
    check sample result:                                              OK
        ✓ check sample result passed 100 tests.
  Tests Rosalind
    Unit tests Rosalind Recv QC
      revc :                                                          OK
      check sample result:                                            OK
        +++ OK, passed 1 test.
      check revc applied twice is the same as original :              OK
        +++ OK, passed 100 tests.
      check revc' applied twice is the same as original :             OK
        +++ OK, passed 100 tests.

All 28 tests passed (0.49s)
Test suite Rosalind-test: PASS
Test suite logged to:
1 of 1 test suites (1 of 1 test cases) passed.
```