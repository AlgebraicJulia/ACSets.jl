########
@present SchJunct(FreeSchema) begin
    Name::AttrType
    Color::AttrType
    #
    Student::Ob
    name::Attr(Student, Name)
    favoriteSubject::Attr(Student, Name)
    favoriteColor::Attr(Student, Color)
    #
    Class::Ob
    subject::Attr(Class, Name)
    #
    Junct::Ob
    student::Hom(Junct,Student)
    class::Hom(Junct,Class)
end
@acset_type JunctionData(SchJunct, index=[:name])
jd = JunctionData{Symbol, Symbol}()

create!(conn, jd)

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

#=
Let's check that it worked:
=#
jd
