# # Creating Junction Tables

#=
In relational database design, junction tables are ways of storing many-many relationships between multiple tables. Simply, each row of the table is a list of foreign key relationships to other tables.

This page gives a simple example for junction tables. We will create a ACSet schema and the code necessary to create it. Let's load our dependencies: in addition to ACSets, we'll need Catlab's `@present` macro:
=#

using ACSets
using Catlab

#=
In our example, we will create a schema expressing student membership in classes. We specify Students, Classes, and Junct, which is our name for expressing the relationship between Students and Classes. We specify that Junct has two maps: `student` and `class`, which associates the Junction table 
=#

@present SchJunct(FreeSchema) begin
    Name::AttrType
    #
    Student::Ob
    name::Attr(Student, Name)
    #
    Class::Ob
    subject::Attr(Class, Name)
    #
    Junct::Ob
    student::Hom(Junct,Student)
    class::Hom(Junct,Class)
end

#=
Categorically, the Junction table acts like a span between Student and Class!

Let's create a type based on this schema and bind an instance of it to the variable `jd`.
=#

@acset_type JunctionData(SchJunct, index=[:name])
jd = JunctionData{Symbol}()

#=
Now we need data! We may talk about a student-class relationship by how each student is in multiple classes. We'll represent our data this way as a dictionary:
=#

df = Dict(:Fiona => [:Math, :Philosophy, :Music],
          :Gregorio => [:Cooking, :Math, :CompSci],
          :Heather => [:Gym, :Art, :Music, :Math])

#=
Now we need to add this data to the junction table. The process for adding table should do the following:

1. For each student `(keys(df`) let's get their classes (`df[student]`) and add the student into the ACSet.
2. For each class, let's see whether it's already present in the ACSet by getting its ID.
3. If the ID value is empty, then the class is not there. We add the class to the table and save the ID. Otherwise if the class is there, we don't need to do anything.
4. Now we add the association between the student and class by adding their respective IDs to the Junction table.

This algorithm is realized in nine lines of code:
=#

foreach(keys(df)) do student
    classes = df[student]
    student_id = add_part!(jd, :Student, name=student)
    foreach(classes) do class
        class_id = incident(jd, class, :subject)
        if isempty(id); class_id = add_part!(jd, :Class, subject=class) end
        add_part!(jd, :Junct, student=studentid, class=only(class_id))
    end
end

#=
Let's check that it worked:
=#

jd

#=
Huzzah! A standard practice in database architecture is quickly accomplished in ACSets. How we mentioned in passing that a Junction Table functions like a Span in databases. Let's make this clear and then discuss what database object functions in the dual case, a Cospan.
=#
