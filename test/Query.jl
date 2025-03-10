using Test

using ACSets
using ACSets.Query: AndWhere, OrWhere, WhereCondition

using DataFrames: nrow

@testset "Conditionals" begin

  w1 = WhereCondition(:src, |>, ∈([1,2]))
  w2 = WhereCondition(:tgt, |>, ∈([2,3]))
  w3 = WhereCondition(:src, |>, ∈([1,3]))
  
  # tuple to list
  @test AndWhere(w1, w2) == AndWhere([w1, w2])
  # associativity
  @test AndWhere([w1, w2], w3) == AndWhere([w1, w2, w3])
  @test AndWhere(w1, [w2, w3]) == AndWhere([w1, w2, w3])
  # & operator
  @test AndWhere(w1, w2) & w3 == AndWhere([w1, w2, w3])
  @test w1 & AndWhere(w2, w3) == AndWhere([w1, w2, w3])
  
  # tuple to list
  @test OrWhere(w1, w2) == OrWhere([w1, w2])
  # associativity
  @test OrWhere([w1, w2], w3) == OrWhere([w1, w2, w3])
  @test OrWhere(w1, [w2, w3]) == OrWhere([w1, w2, w3])
  # | operator
  @test OrWhere(w1, w2) | w3 == OrWhere([w1, w2, w3])
  @test w1 | OrWhere(w2, w3) == OrWhere([w1, w2, w3])
  
  # mixing conjunction
  @test AndWhere(w1, w2) | w3 == OrWhere(AndWhere(w1, w2), w3)
  @test w1 & OrWhere(w2, w3) == AndWhere(w1, OrWhere(w2, w3))

end

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])
@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS
  
DDS(i::Int) = DDS(rand(1:i, i))
DDS(v::Vector{Int}) = @acset DDS begin X=length(v); Φ=v end

d = DDS(5)

@testset "Querying" begin

  q = From(:X)
  @test q(d) == (:X => parts(d, :X))
  
  q = From(:X) |> Select(:X)
  @test q(d) == [:X => parts(d, :X)]
  
  q = From(:X) |> Select(:Φ)
  @test q(d) == [:Φ => subpart(d, :Φ)]
  
  SchDecGraph = BasicSchema([:E,:V], [(:src,:E,:V),(:tgt,:E,:V)],
                            [:X], [(:dec,:E,:X)])
  @acset_type DecGraph(SchDecGraph, index=[:src,:tgt])
  
  g = @acset DecGraph{String} begin
    V = 4
    E = 4
    src = [1,2,3,4]
    tgt = [2,3,4,1]
    dec = ["az","bz","cz","dz"]
  end
  
  q = From(:V) |> Where(:dec, !=("az")) |> Select(:dec)
  @test q(g) == [:dec => ["bz", "cz", "dz"]]
  
  q = From(:V) |> Where(:src, 1) & Where(:tgt, 2) |> Select(:dec)
  @test q(g) == [:dec => ["az"]]
  
  q = From(:E) |> Where([:src, :tgt], (x,y) -> x + 1 == y)
  @test q(g) == (:E => [1,2,3])
  
  # data frames
  q = From(:E) |> Select(Val(:a), :src)
  df = q(g; formatter=DFQueryFormatter())
  
  @test nrow(df) == nparts(g, :E)
  @test df[!, :Val_a] == fill(:a, nparts(g, :E))
  
  # Catlab.jl allows us to build conjunctive queries on ACSets with the `@relation` macro. In this example, we will show how we can specify conjunctive queries with a FunSQL-like syntax. Let's load up our student-class schema again.
  SchJunct = BasicSchema([:Student, :Class, :Junct], [(:student, :Junct, :Student), (:class, :Junct, :Class)],
                         [:Name], [(:name, :Student, :Name), (:subject, :Class, :Name)])
  @acset_type JunctionData(SchJunct, index=[:name])
  jd = JunctionData{Symbol}()
  
  df = Dict(:Fiona => [:Math, :Philosophy, :Music],
            :Gregorio => [:Cooking, :Math, :CompSci],
            :Heather => [:Gym, :Art, :Music, :Math])
  
  foreach(keys(df)) do student
    classes = df[student]
    student_id = incident(jd, student, :name)
    if isempty(student_id); student_id = add_part!(jd, :Student, name=student) end
    foreach(classes) do class
      class_id = incident(jd, class, :subject)
      if isempty(class_id); class_id = add_part!(jd, :Class, subject=class) end
      add_part!(jd, :Junct, student=student_id, class=only(class_id))
    end
  end
  
  q = From(:Student) |> Select(:name)
  @test q(jd) == [:name => [:Fiona, :Gregorio, :Heather]]
  
  q = From(:Student) |> Where(:Student, From(:Junct => :student)) |> 
      Select(:name)
  @test q(jd) == [:name => [:Fiona, :Gregorio, :Heather]]
  
  q = From(:Student) |> 
     Where(:Student, From(:Junct) |> Select(:student)) |> 
      Select(:Student)
  @test q(jd) == [:Student => [1,2,3]]
  
  # not specifying a select statement defaults to the From
  q = From(:Student) |> 
      Where(:Student, From(:Junct) |> Select(:student)) 
  @test q(jd) == (:Student => [1,2,3])
  
  q = From(:Student) |>
  Where(:Student, From(:Junct => :student)) &
  Where(:name, :Gregorio) | Where(:name, :Fiona) |> Select(:name)
  @test q(jd) == [:name => [:Fiona, :Gregorio]]
  
  q = From(:Student) |> Where(:name, [:Gregorio, :Fiona]) |> Select(:name);
  @test q(jd) == [:name => [:Fiona, :Gregorio]]
  
  q = From(:Student) |> Where(:name, !=(:Gregorio)) |> Select(:name);
  @test q(jd) == [:name => [:Fiona, :Heather]]
  
  isGregorio(x::Symbol) = x == :Gregorio
  @test !isGregorio(:Heather)
  
  q = From(:Student) |> Where(:name, !isGregorio) |> Select(:name);
  @test q(jd) == [:name => [:Fiona, :Heather]]
  
  # apply function to Select
  q = From(:Student) |> Select(:name => isGregorio)
  @test q(jd) == [:isGregorioname => [0, 1, 0]]

end
