using Test

using ACSets
using DataFrames: nrow

SchDDS = BasicSchema([:X], [(:Φ,:X,:X)])
@abstract_acset_type AbstractDDS
@acset_type DDS(SchDDS, index=[:Φ]) <: AbstractDDS

DDS(i::Int) = DDS(rand(1:i, i))
function DDS(v::Vector{Int})
  x = DDS()
  add_parts!(x, :X, length(v))
  set_subpart!(x, :Φ, v)
  x
end

d = DDS(5)

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
  dec = ["a","b","c","d"]
end

# When we process the `Where` condition with a function on the RHS, we splat
# the values entered into the function. This means that strings get splatted
# into Char. To make sure that strings are properly compared, the user must reconstruct it with a String([x]) function.
q = From(:V) |> Where(:dec, x -> String(x) != "a") |> Select(:dec)
@test q(g) == [:dec => ["b", "c", "d"]]

q = From(:V) |> Where(:src, 1) & Where(:tgt, 2) |> Select(:dec)
@test q(g) == [:dec => ["a"]]

q = From(:E) |> Where([:src, :tgt], (x,y) -> x + 1 == y)
@test q(g) == (:E => [1,2,3])

# data frames
q = From(:E) |> Select(Val(:a), :src)
df = q(g; formatter=:df)

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
    # let's make this idempotent by adding student only if they aren't in the system
    student_id = incident(jd, student, :name)
    if isempty(student_id); student_id = add_part!(jd, :Student, name=student) end
    # for each of the classes the student has...
    foreach(classes) do class
        # idempotently add their class
        class_id = incident(jd, class, :subject)
        if isempty(class_id); class_id = add_part!(jd, :Class, subject=class) end
        # enforce pair constraint
        id_pair = incident(jd, only(student_id), :student) ∩ incident(jd, only(class_id), :class)
        isempty(id_pair) && add_part!(jd, :Junct, student=student_id, class=only(class_id))
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
