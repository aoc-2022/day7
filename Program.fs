open System.IO
let lines = File.ReadAllText "/tmp/aoc/input"
let commands = lines.Split [|'$'|] |> Array.map (fun x -> x.Split [|'\n'|] |> Array.toList) |> Array.toList 

type Session (fs:Map<string list,string list>,pwd:string list, commands: string list list) =
    member this.Pwd = pwd
    member this.Fs = fs
    member this.Execute =
        printfn $"commands: {commands}"
        match commands with
        | [] -> this
        | []::rest -> Session(fs,["banana"],[])
        | (c::files)::rest ->
            printfn $"{c}"
            let cmd = c.Split [|' '|] |> Array.tail 
            let cmd = cmd |> Array.toList
            match cmd with
            | ["ls"] ->
                printfn $"LIST: -> {files}"
                let fs = fs.Add(pwd,files)
                Session(fs,pwd,rest).Execute
            | ["cd";"/"] ->
                printfn $"CD /"
                Session(fs,[],rest).Execute
            | ["cd";".."] ->
                printfn $"CD .."
                Session(fs,pwd.Tail,rest).Execute
            | ["cd";dir] ->
                printfn $"CD {dir}"
                Session(fs,dir::pwd,rest).Execute
    override this.ToString() = $"Session({fs},{pwd},{commands}"
            
let session = Session(Map.empty,[],commands |> List.tail)

let s = session.Execute
let fs = s.Fs

let sizes (files: string list) =
    let sizelist = files |> List.map (fun file -> file.Split [|' '|] |> Array.head)
    let sizelist = sizelist |> List.filter (fun s -> (s = "" or s = "dir") |> not)
    sizelist |> List.map int |> List.sum 

let fs2 = fs |> Map.map (fun _ -> fun v -> sizes v) |> Map.toList 

let rec cascade (dir:string list,size:int) =
    match dir with
    | [] -> [([],size)]
    | _::b -> (dir,size) :: (cascade (b,size))

let fs3 = fs2 |> List.map cascade |> List.concat

let fs4 = fs3 |> List.groupBy (fun (d,s) -> d)

let fs5 = fs4 |> List.map (fun (dir,values) -> values)
let fs6 = fs5 |> List.map (List.map (fun (a,b) -> b))
let fs7 = fs6 |> List.map List.sum

let task1 = fs7 |> List.filter (fun s -> s<=100000) |> List.sum 

printfn $"Task 1 {task1}"

let (_,used) = fs4 |> List.filter (fun (k,v) -> k = []) |> List.head
let roots = used |> List.map (fun (a,size) -> size) |> List.sum

let todelete =  roots - 40000000  

printfn $"fs = {todelete}"

let cands = fs7 |> List.filter (fun s -> s >= todelete) |> List.sort |> List.head 

printfn $"fs7={cands}"
