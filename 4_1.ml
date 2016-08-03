type person = King
            | Peer of string * string * int
            | Knight of string
            | Peasant of string

let persons = [King; Peasant "Jack Cade"; Knight "Gawain"]
