# Roslind Problem Solutions in Haskell

[Rosalind Problems Information](https://rosalind.info/problems/list-view/)

Command line runner [RosalindProblemRunner.hs](app/RosalindProblemRunner.hs)

Run with
```
❯ cabal run RosalindCommandExecuter -- --help
```
```
Up to date
Rosalind Problem runner

Usage: RosalindCommandExecuter COMMAND [-e|--example]
  Runs Rosalind problems on a given dataset file Dataset/<problem name>.txt

Available options:
  -e,--example             Choose dataset file <problem name>_example.txt
  -h,--help                Show this help text

Available commands:
  hamm                     Execute problem hamm
  rna                      Execute problem rna
  revc                     Execute problem revc
  revc2                    Execute problem revc
  prot                     Execute problem prot
  tran                     Execute problem tran
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