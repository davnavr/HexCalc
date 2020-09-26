namespace HexCalc

type State =
    { Answer: Integer
      Variables: Map<string, Integer> }

    static member Default =
        { Answer = Integer.Zero
          Variables = Map.empty }

module State =
    let private updateVars f state =
        { state with Variables = f state.Variables }
    let setVar name (value: Integer) =
        if value.Value.IsZero
        then Map.remove name
        else Map.add name value
        |> updateVars
    let getVar name state =
        Map.tryFind
            name
            state.Variables
        |> Option.defaultValue Integer.Zero
