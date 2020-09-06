#if FAKE_DEPENDENCIES
#r "paket:
nuget Fake.Core.Target
nuget Fake.DotNet.Cli
nuget Fake.IO.FileSystem
//"
#endif

#load "./.fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO.FileSystemOperators

module DotNetCli = Fake.DotNet.DotNet

let rootDir = __SOURCE_DIRECTORY__
let outDir = rootDir </> "out"
let slnFile = rootDir </> "HexCalc.sln"

let handleErr msg: ProcessResult -> _ =
    function
    | { ExitCode = ecode } when ecode <> 0 ->
        failwithf "Process exited with code %i: %s" ecode msg
    | _ -> ()

let runProj args proj =
    sprintf
        "--project %s --no-restore --configuration Release -- %s"
        proj
        args
    |> DotNetCli.exec id "run"

Target.create "Clean" (fun _ ->
    slnFile
    |> DotNetCli.exec id "clean"
    |> ignore
)

Target.create "Build" (fun _ ->
    DotNetCli.build
        (fun opt ->
            { opt with
                Configuration = DotNetCli.Release
                NoRestore = true })
        slnFile
)

Target.create "Test" (fun _ ->
    rootDir </> "test" </> "HexCalc.Tests.fsproj"
    |> runProj ""
    |> handleErr "One or more tests may have failed"
)

Target.create "Pack" (fun _ ->
    rootDir </> "src" </> "HexCalc.fsproj"
    |> DotNetCli.pack
        (fun opt ->
            { opt with
                Configuration = DotNetCli.Release
                NoRestore = true
                OutputPath = Some outDir })
)

"Clean"
==> "Build"
==> "Test"
==> "Pack"

Target.runOrDefault "Pack"
