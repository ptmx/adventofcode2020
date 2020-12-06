File.stream!("input.txt")
|> Stream.chunk_by(&(&1 == "\n"))
|> Stream.filter(&(&1 != ["\n"]))
|> Stream.map(&Enum.map(&1, fn (s) -> String.trim(s) end))
|> Stream.map(&Enum.join/1)
|> Stream.map(&String.to_charlist/1)
|> Stream.map(&MapSet.new/1)
|> Stream.map(&MapSet.size/1)
|> Enum.sum
|> IO.puts
