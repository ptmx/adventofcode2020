readfile =: 1!:1

pattern =: '^([[:digit:]]+)-([[:digit:]]+) ([[:alpha:]]): ([[:alpha:]]+)$'
match =: pattern & rxmatch
extract =: }. @: (match rxfrom ])

getmin =: ". @: > @: (0 & {) @: extract
getmax =: ". @: > @: (1 & {) @: extract
getchar =: (0 & {) @: > @: (2 & {) @: extract
getstring =: > @: (3 & {) @: extract

countequal =: +/ @: (getstring = getchar)
gtemin =: (getmin <: countequal)
gtemax =: (countequal <: getmax)
isvalid =: gtemin *. gtemax

data =. readfile < 'input.txt'
lines =. cutopen data
valid =. > (isvalid each lines)
countvalid =. +/ valid

echo countvalid
exit''
