#!/usr/bin/bash

valid_count=0

function unsetVars {
  byr=""
  iyr=""
  eyr=""
  hgt=""
  hcl=""
  ecl=""
  pid=""
  cid=""
}

function validateCurrent {
  if [[ ! -z "$byr" ]] && \
     [[ ! -z "$iyr" ]] && \
     [[ ! -z "$eyr" ]] && \
     [[ ! -z "$hgt" ]] && \
     [[ ! -z "$hcl" ]] && \
     [[ ! -z "$ecl" ]] && \
     [[ ! -z "$pid" ]]
  then
    valid_count=$((valid_count + 1))
  fi
}

function setVar {
  IFS=":" read -ra tuple <<< $1
  key=${tuple[0]}
  value=${tuple[1]}
  eval "$key=$value"
}

function setVars {
  IFS=" " read -ra pairs <<< $1
  for pair in "${pairs[@]}"
  do
    setVar "$pair"
  done
}

while read line
do
  trimmed=$(echo $line | tr -d "\r\n")
  if [[ -z $trimmed ]]
  then
    validateCurrent
    unsetVars
  else
    setVars "$trimmed"
  fi
done < $1

validateCurrent

echo $valid_count
