module SQLiteACSetsExt

using ACSets

using SQLite
using DataFrames

function selectfrom(db::SQLite.DB, table)
    query = DBInterface.execute(db, "SELECT * FROM $table")
    DataFrames.DataFrame(query)
end

db = SQLite.DB()

DBInterface.execute(db, """
    CREATE TABLE IF NOT EXISTS artist(
        artistid    INTEGER PRIMARY KEY,
        artistname  TEXT
    );
    CREATE TABLE IF NOT EXISTS track(
        trackid     INTEGER,
        trackname   TEXT,
        trackartist INTEGER,
        FOREIGN KEY(trackartist) REFERENCES artist(artistid)
    );
    """)

macro splitsql_str(str)
    split(str, ";\n")
end

stmts = splitsql"""
    CREATE TABLE IF NOT EXISTS artist(
        artistid    INTEGER PRIMARY KEY,
        artistname  TEXT
    );
    CREATE TABLE IF NOT EXISTS track(
        trackid     INTEGER,
        trackname   TEXT,
        trackartist INTEGER,
        FOREIGN KEY(trackartist) REFERENCES artist(artistid)
    );"""

# could have a string macro that parses these
function DBInterface.execute(db::SQLite.DB, xs::Vector{<:AbstractString})
    DBInterface.execute.(Ref(db), xs)
end

DBInterface.execute(db, stmts)

selectfrom(db, "artist")
selectfrom(db, "track")

end
