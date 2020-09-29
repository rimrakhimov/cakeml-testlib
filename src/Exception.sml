structure Exception =
  struct
    fun exnName exn = 
      case exn of
       (* basis primitive exceptions *)
        Bind => "Bind"
      | Chr => "Chr"
      | Div => "Div"
      | Subscript => "Subscript"

       (* TextIO type exceptions *)
      | TextIO.BadFileName => "TextIO.BadFileName"
      | TextIO.InvalidFD => "TextIO.InvalidFD"
      | TextIO.EndOfFile => "TextIO.EndOfFile"
      | TextIO.IllegalArgument => "TextIO.IllegalArgument"

       (* Assert custom type exceptions *)
      | (Assert.Fail _) => "Assert.Fail"

      | _ => "Unknown exception"

    fun exnMessage exn =
      case exn of
       (* basis primitive exceptions *)
        Bind => "Bind"
      | Chr => "Chr"
      | Div => "Div"
      | Subscript => "Subscript"

       (* TextIO type exceptions *)
      | TextIO.BadFileName => "TextIO.BadFileName"
      | TextIO.InvalidFD => "TextIO.InvalidFD"
      | TextIO.EndOfFile => "TextIO.EndOfFile"
      | TextIO.IllegalArgument => "TextIO.IllegalArgument"

       (* Assert custom type exceptions *)
      | (Assert.Fail (Assert.GeneralFailure msg)) => ("Assert.Fail" ^ " " ^ "GeneralFailure" ^ " \"" ^ msg ^ "\"")
      | (Assert.Fail (Assert.NotEqualFailure msg1 msg2)) => ("Assert.Fail" ^ " " ^ "NotEqualFailure" ^ " \"" ^ msg1 ^ "\"" ^ " \"" ^ msg2 ^ "\"")

      | _ => "Unknown exception"

  end;