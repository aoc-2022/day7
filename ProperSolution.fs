module day7.ProperSolution

open System
open System.IO

exception ParseError of string 

type Fil(name:string,size:int) =
    member this.Name = name
    member this.Size = size
    member this.ToPrettyString (indent:int) =
        let prefix = "  " |> String.replicate indent
        $"{prefix}{size} {name}"
    
type Dir(name:String,dirs: Map<string,Dir>,files:Map<string,Fil>) =
    member this.Dirs = dirs
    member this.Files = files 
    member this.AddDir (name:string) (dir:Dir) = Dir(name,dirs.Add (name,dir),files)
    member this.AddFile (file:Fil) = Dir(name,dirs,files.Add(file.Name,file))
    member this.Size () =
        let filesizes = files.Values |> Seq.map (fun f -> f.Size) |> Seq.sum
        let dirsizes = dirs.Values |> Seq.map (fun d -> d.Size ()) |> Seq.sum
        filesizes + dirsizes
    member this.FlattenDirs () : Dir seq  =
        dirs.Values |> Seq.collect (fun d -> d.FlattenDirs ()) |> Seq.append [this] 
    member this.ToPrettyString (indent:int) =
        let prefix = "  " |> String.replicate indent 
        let self = $"{prefix}{name}"
        let dirstrings = dirs.Values |> Seq.map (fun d -> d.ToPrettyString (indent+1)) |> Seq.toList
        let filestrings = files.Values |> Seq.map (fun f -> f.ToPrettyString (indent+1)) |> Seq.toList 
        [[self];dirstrings;filestrings] |> List.concat |> String.concat "\n"
    override this.ToString () = this.ToPrettyString 0 
        
    static member empty (name:string) = Dir(name,Map.empty,Map.empty)
        

let rec parse (pwd:Dir) (lines: string list) : Dir*(string list) = 
    match lines with
    | [] -> pwd,[]
    | line::rest -> 
        match line.Split [|' '|] with
        | [|"$";"cd";".."|] -> pwd,rest // returning this folder and the rest of the lines
        | [|"$";"cd";name|] ->
            let dir = (pwd.Dirs.TryFind name) |> Option.defaultValue (Dir.empty name)
            let (dir,rest) = parse dir rest
            let pwd = pwd.AddDir name dir
            parse pwd rest
        | [|"$";"ls"|] -> parse pwd rest // ignoring ls, we'll just process the entries
        | [|"dir";_|] -> parse pwd rest // ignoring dir entries, we'll create dirs when we cd into them
        | [|size;name|] ->
            let fil = Fil(name,int size)
            let pwd = pwd.AddFile fil
            parse pwd rest
        | _ -> raise (ParseError $"wtf is this? {line}")
        
let run () =
    let input = File.ReadAllLines "/tmp/aoc/input" |> Array.toList
    let (dir,rest) = parse (Dir.empty "/") input
    let dirs = dir.FlattenDirs ()
    let totalSize = dir.Size ()
    let toFree = totalSize - 40000000
    let sizes = dirs |> Seq.map (fun d -> d.Size ())
    let sumOfSmall = sizes |> Seq.filter (fun size -> size <= 100000) |> Seq.sum 
    let bestForRemoval = sizes |> Seq.filter (fun size -> size > toFree) |> Seq.min
    printfn $"{dir} [rest={rest}"
    printfn $"Task 1: {sumOfSmall}"
    printfn $"Task 2: {bestForRemoval}"
    
    