#r "packages/FSharp.Data.SqlClient/lib/net40/FSharp.Data.SqlClient.dll"
open System
open FSharp.Data

[<Literal>]
let ConnectionString = "Data Source=REVANS-PC\SQLEXPRESS;Database=Sandbox;Trusted_Connection=True;"

type Location = {
    City : String
    State : String
}

let insert location =
    use command =
        new SqlCommandProvider<
            "Insert into locations(city, state)
            values(@city, @state)"
            , ConnectionString>()
    command.Execute(location.City, location.State)

let stateIsColorado location = location.State = "CO"

let insertLocatioinWithChecks location =
    if stateIsColorado location
    then insert location
    else raise (System.ArgumentException("Not a valid State"))

    
//insertLocatioinWithChecks { City = "San Francisco"; State = "CA" }
