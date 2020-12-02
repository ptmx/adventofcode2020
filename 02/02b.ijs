readfile =: 1!:1

pattern =: '^([[:digit:]]+)-([[:digit:]]+) ([[:alpha:]]): ([[:alpha:]]+)$'
match =: pattern & rxmatch
extract =: }. @: (match rxfrom ])

getfirstindex =: (- & 1) @: ". @: > @: (0 & {) @: extract
getsecondindex =: (- & 1) @: ". @: > @: (1 & {) @: extract
getchar =: (0 & {) @: > @: (2 & {) @: extract
getstring =: > @: (3 & {) @: extract

getfirst =: getfirstindex { getstring
getsecond =: getsecondindex { getstring

firstmatches =: getfirst = getchar
secondmatches =: getsecond = getchar
isvalid =: firstmatches ~: secondmatches

data =. readfile < 'input.txt'
lines =. cutopen data
valid =. > (isvalid each lines)
countvalid =. +/ valid

echo countvalid
exit''
