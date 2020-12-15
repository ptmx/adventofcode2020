indexA = 2020
indexB = 30000000
starting_numbers = {15, 5, 1, 4, 7, 0}

spoken_indexes = {}
index = 0
prev_number = nil
while index < indexB do
  if table.getn(starting_numbers) > 0 then
    prev_number = table.remove(starting_numbers, 1)
  else
    prev_number_indexes = spoken_indexes[prev_number]
    if prev_number_indexes == nil then
      prev_number_indexes = {}
    end

    if table.getn(prev_number_indexes) <= 1 then
      prev_number = 0
    else
      prev_number = prev_number_indexes[#prev_number_indexes] - prev_number_indexes[#prev_number_indexes - 1]
    end
  end

  prev_number_indexes = spoken_indexes[prev_number]
  if prev_number_indexes == nil then
    prev_number_indexes = {}
  end
  prev_number_indexes[#prev_number_indexes + 1] = index

  spoken_indexes[prev_number] = prev_number_indexes
  index = index + 1

  if index == indexA then
    print(prev_number)
  end
end

print(prev_number)
