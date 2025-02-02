structure TextUITestRunner =
  struct
    (***************************************************************************)

    (**
     *  the type representing implementation specific parameters to runTest function.
     *
     * NOTE : In future version, this might be changed so that the verbose level
     *  of printing results can be specified. 
     *)
    datatype Parameter = Output TextIO.outstream

    (**
     *  the type representing a test result
     * @params testCount, failures, errors
     * @param testCount the number of test performed
     * @param failures list of pair of path and failure message of tests aborted by assertion failure
     * @param errors list of pair of path and exception message of tests aborted by any exception raised
     *)
    datatype TestResult = TestResult int ((string * string) list) ((string * exn) list)

    (***************************************************************************)

    val separator = "/"

    (***************************************************************************)

    fun printTo (parameter) string =
        case parameter of
          Output out => TextIO.output out string

    fun doTest parameter path test =
        let
          val print = printTo parameter
        in
          case test of
            (Test.TestCase test) =>
            ((
              test ();
              print ".";
              TestResult 1 [] []
            )
            handle Assert.Fail failure =>
                    let
                      val message = 
                          case failure of
                            Assert.GeneralFailure message => message
                          | Assert.NotEqualFailure expected actual =>
                            "expected:<" ^ expected ^">, actual:<" ^ actual ^ ">"
                    in
                      print "F";
                      TestResult 1 [(path, message)] []
                    end
                  | error => 
                    (
                      print "E";
                      TestResult 1 [] [(path, error)]
                    ))
          | (Test.Test label f) =>
            doTest parameter path (Test.TestLabel label (Test.TestCase f))
          | (Test.TestLabel label test) =>
            doTest parameter (path ^ separator ^ label) test
          | (Test.TestList tests) =>
            let 
              fun runOneTest test (index, (TestResult accumulatedTestCount accumulatedFailures accumulatedErrors)) =
                  let
                    val (TestResult currentTestCount currentFailures currentErrors) =
                        doTest
                            parameter
                            (path ^ separator ^ (Int.toString index)) test
                  in
                    (
                      index + 1,
                      TestResult (accumulatedTestCount + currentTestCount)
                                 (accumulatedFailures @ currentFailures)
                                 (accumulatedErrors @ currentErrors)
                    )
                  end
            in
              snd (List.foldr 
                          runOneTest
                          (1, TestResult 0 [] [])
                          tests)
            end
        end

    fun printTestResult parameter (TestResult testCount failures errors) =
      let
        val print = printTo parameter
        val message =
            ("tests = " ^ (Int.toString testCount) ^ ", ") ^
            ("failures = " ^ (Int.toString (List.length failures)) ^ ", ") ^
            ("errors = " ^ (Int.toString (List.length errors)))
      in
        print ("\n" ^ message ^ "\n");
        print "Failures:\n";
        List.app
        (fn (path, message) =>
            (print path; print  ": "; print message; print "\n"))
        failures;
        print  "Errors:\n";
        List.app
        (fn (path, exn) =>
            (print
             (path ^ ": uncaught exception " ^ Exception.exnName exn ^ "\n <<< "
              ^ Exception.exnMessage exn ^ " >>>\n")))
        errors
      end

    (***************************************************************************)

    fun runTest parameter test =
        printTestResult parameter (doTest parameter separator test)

    (***************************************************************************)

  end;