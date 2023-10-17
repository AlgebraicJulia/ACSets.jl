using ACSets

MySch = BasicSchema(
    [:X,:Y,:Rel],
    [(:proj_x,:Rel,:X),(:proj_y,:Rel,:Y)],
    [:NameType,:NumberType],
    [(:xname,:X,:NameType),(:yname,:Y,:NameType),(:cost,:Rel,:NumberType)]
)

@acset_type MyDataType(MySch, index=[:proj_x,:proj_y], unique_index=[:xname,:yname])

mydata = @acset MyDataType{Symbol,Float64} begin
    X = 4
    Y = 3
    xname = [:widget1,:widget2,:widget3,:widget4]
    yname = [:place1,:place2,:place3]
end

costs = rand(4,3)

add_parts!(
    mydata, :Rel, prod(size(costs)),
    proj_x = repeat(1:4,3),
    proj_y = vcat(map(i->fill(i,4),1:3)...),
    cost = vcat(costs...)
)

# a pattern that comes up very, very often
mydata[
    intersect(
        incident(mydata, :widget3, [:proj_x,:xname]),
        incident(mydata, :place2, [:proj_y,:yname])
    ),
    :cost
]

# Kris' example
using Catlab
SchTest = BasicSchema(
    [:A,:B,:C,:R],
    [(:f,:R,:A),(:g,:R,:B),(:h,:R,:C)],
    [],
    []
)

# @acset_type Test(SchTest, index=[:h, (:f,:g)])
@acset_type Test(SchTest)

X = @acset Test begin
    A=1
    B=1
    C=1
    R=1
    f=[1]
    g=[1]
    h=[1]
end

# incident(X, (1,1), (:f,:g)) # == [1]
incident(X, 1, :f)
incident(X, 1, :g)

acs = X
parts = (1,1)
f = (:f,:g)

function ACSetInterface.incident(
        acs::SimpleACSet, parts::T1, f::T2) where {T1<:Tuple,T2<:Tuple{Vararg{Symbol}}}
    s = acset_schema(acs)
    length(unique([dom(s,f[i]) for i in eachindex(f)])) == 1 || error("some homs in $(f) do not have the same domain")
    length(parts) == length(f) || error("parts should be the same length as f")
    return intersect([incident(acs, parts[i], f[i]) for i in eachindex(f)]...)
end


incident(X, (1,1), (:f,:g))

function ACSetInterface.incident(
        acs::SimpleACSet, parts::T1, f::T2) where {T1<:Tuple,T2<:Tuple{Vararg{AbstractVector{Symbol}}}}
    # foldr(names, init=part) do name, part
    #     reduce(vcat, collect.(incident(acs, part, name)), init=Int[])
    # end
    println("im being called!")
end

incident(mydata, (:widget3,:place2), ([:proj_x,:xname],[:proj_y,:yname]))