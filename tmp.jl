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