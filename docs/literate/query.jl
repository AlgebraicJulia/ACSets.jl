q = From(:Student) |> Select(:name);
@assert q(jd) == [:Fiona, :Gregorio, :Heather]

q = From(:Student) |> 
    Where(:Student, From(:Junct) |> Select(:student)) |> 
    Select(:name);
@assert q(jd) == [:Fiona, :Gregorio, :Heather]

q = From(:Student) |> 
    Where(:Student, From(:Junct) |> Select(:student)) |> 
    Select(:Student);
@assert q(jd) == [1,2,3]

# not specifying a select statement defaults to the From
q = From(:Student) |> 
    Where(:Student, From(:Junct) |> Select(:student)); 
@assert q(jd) == [1,2,3]

q = From(:Student) |>
Where(:Student, From(:Junct) |> Select(:student)) &
Where(:name, :Gregorio) | Where(:name, :Fiona) |> Select(:name);
q(jd)

q = From(:Student) |> Where(:name, [:Gregorio, :Fiona]) |> Select(:name);
@assert q(jd) == [:Fiona, :Gregorio]

q = From(:Student) |> Where(:name, ∉, :Gregorio) |> Select(:name);
@assert q(jd) == [:Fiona, :Heather]

isGregorio(x::Symbol) = x == :Gregorio
@assert !isGregorio(:Heather)

q = From(:Student) |> Where(:name, !isGregorio) |> Select(:name);
@assert q(jd) == [:Fiona, :Heather]
