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
  valid_birth_year=false
  valid_issue_year=false
  valid_expiry_year=false
  valid_height=false
  valid_hair_color=false
  valid_eye_color=false
  valid_pid=false

  if [[ "$byr" -ge "1920" ]] && \
     [[ "$byr" -le "2002" ]]
  then
    valid_birth_year=true
  fi

  if [[ "$iyr" -ge "2010" ]] && \
     [[ "$iyr" -le "2020" ]]
  then
    valid_issue_year=true
  fi

  if [[ "$eyr" -ge "2020" ]] && \
     [[ "$eyr" -le "2030" ]]
  then
    valid_expiry_year=true
  fi

  if [[ "$hgt" =~ ^([0-9]+)(cm|in)$ ]]
  then
    height=${BASH_REMATCH[1]}
    units=${BASH_REMATCH[2]}
    if [[ "$units" == "cm" ]] && \
       [[ "$height" -ge "150" ]] && \
       [[ "$height" -le "193" ]]
    then
      valid_height=true
    fi

    if [[ "$units" == "in" ]] && \
         [[ "$height" -ge "59" ]] && \
         [[ "$height" -le "76" ]]
    then
      valid_height=true
    fi
  fi

  if [[ "$hcl" =~ ^\#[0-9a-f]{6}$ ]]
  then
    valid_hair_color=true
  fi

  if [[ "$ecl" =~ ^(amb|blu|brn|gry|grn|hzl|oth)$ ]]
  then
    valid_eye_color=true
  fi

  if [[ "$pid" =~ ^[0-9]{9}$ ]]
  then
    valid_pid=true
  fi

  if [[ "$valid_birth_year" == "true" ]] && \
     [[ "$valid_issue_year" == "true" ]] && \
     [[ "$valid_expiry_year" == "true" ]] && \
     [[ "$valid_height" == "true" ]] && \
     [[ "$valid_hair_color" == "true" ]] && \
     [[ "$valid_eye_color" == "true" ]] && \
     [[ "$valid_pid" == "true" ]]
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
