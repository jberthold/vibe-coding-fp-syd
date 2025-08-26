We are writing a Haskell tool which reads a log from pytest and displays the order in which tests were executed.

The logs written by pytest contain the following relevant lines:
(example)
```
  src/tests/integration/test_cli.py::test_cli_show_printers_snapshot
  src/tests/integration/test_integration.py::test_exec_smir[Ref-weirdRefs-haskell]
  src/tests/integration/test_integration.py::test_prove_rs[assert_eq]
  src/tests/integration/test_integration.py::test_schema_parse[rvalueaggregate]
  [gw3] [  0%] PASSED src/tests/integration/test_integration.py::test_schema_parse[rvalueaggregate]
  src/tests/integration/test_integration.py::test_schema_parse[statementassign1]
  [gw3] [  1%] FAILED src/tests/integration/test_integration.py::test_schema_parse[statementassign1]
  src/tests/integration/test_integration.py::test_schema_parse[statementassign2]
  [gw3] [  1%] PASSED src/tests/integration/test_integration.py::test_schema_parse[statementassign2]
  ...
```
The lines starting with `src/tests...` here indicate the start of a test (by printing its name). Lines starting by `[gw...` indicate that execution of a test has finished, and contain the result `PASSED`, `FAILED`, or maybe `SKIPPED`.
In the example above, 3 tests have finished (rvalueaggregate, statementassign1, statementassign2) while 3 other tests are still running.

Assume that the test names can be arbitrary strings without newline. The prefix `src/tests/integration..` is just an example.
The tool should parse logs written by pytest (discarding irrelevant lines), and establish the order in which tests were executed, including which tests were executed at the same time. The output of the tool should be a visualisation of the test runs which shows this ordering graphically, as in the following (again using the example above):
```
  *----------- test_cli_show_printers_snapshot
   *---------- test_exec_smir[Ref-weirdRefs-haskell]
    *--------- test_prove_rs[assert_eq]
     *-. test_schema_parse[rvalueaggregate]
        *-X test_schema_parse[statementassign1]
           *-. test_schema_parse[statementassign2]
```
This display shows that the first 3 tests are still running, and that the other 3 have terminated (ending in dot for passed and X for failed tests). It is visible that 4 tests are running in parallel at most.

Two example files are provided, the example above (`example1.log`) and a longer file `example2.log` (note that both logs are truncated).
