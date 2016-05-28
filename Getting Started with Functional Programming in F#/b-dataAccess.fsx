//access some data

#r "packages/FSharp.Data.SqlClient/lib/net40/FSharp.Data.SqlClient.dll"
open System
open FSharp.Data

[<Literal>]
let ConnectionString = "Data Source=REVANS-PC\SQLEXPRESS;Database=Sandbox;Trusted_Connection=True;"

let getLocations () =
    use query = new SqlCommandProvider<"SELECT * FROM Locations", ConnectionString>()
    query.Execute()
