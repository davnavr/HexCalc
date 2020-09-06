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
open Fake.IO
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
    Shell.cleanDir outDir

    slnFile
    |> DotNetCli.exec id "clean"
    |> handleErr "Unexpected error while cleaning solution"
)

Target.create "Build" (fun _ ->
    DotNetCli.build
        (fun opt ->
            { opt with
                Configuration = DotNetCli.Release
                // TODO: Set package version.
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

Target.create "Publish" (fun _ ->
    let package =
        Directory.findFirstMatchingFile "HexCalc.*.nupkg" outDir
    sprintf
        "push %s --api-key %s --source %s"
        package
        (Environment.environVar "NUGET_API_KEY")
        "https://api.nuget.org/v3/index.json"
    |> DotNetCli.exec id "nuget"
    |> handleErr "Failed to publish package to NuGet"
)

Target.create "Default" ignore

"Clean"
==> "Build"
==> "Test"
==> "Default"
==> "Pack"
==> "Publish"

Target.runOrDefault "Default"
