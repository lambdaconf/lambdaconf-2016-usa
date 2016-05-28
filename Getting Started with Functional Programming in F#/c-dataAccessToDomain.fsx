#r "packages/FSharp.Data.SqlClient/lib/net40/FSharp.Data.SqlClient.dll"
open System
open FSharp.Data

[<Literal>]
let ConnectionString = "Data Source=REVANS-PC\SQLEXPRESS;Database=Sandbox;Trusted_Connection=True;"

type Location = {
    City : String
    State : String
}

let getLocations () =
    use query = new SqlCommandProvider<"SELECT * FROM Locations", ConnectionString>()
    query.Execute()
    |> Seq.map (fun x -> { City = x.City; State = x.State })