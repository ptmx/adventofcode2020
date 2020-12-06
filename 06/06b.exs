File.stream!("input.txt")
|> Stream.chunk_by(&(&1 == "\n"))
|> Stream.filter(&(&1 != ["\n"]))
|> Stream.map(&Enum.map(&1, fn (s) -> String.trim(s) end))
|> Stream.map(&Enum.map(&1, fn (s) -> String.to_charlist(s) end))
|> Stream.map(&Enum.map(&1, fn (cl) -> MapSet.new(cl) end))
|> Stream.map(&Enum.reduce(&1, fn (x, acc) -> MapSet.intersection(x, acc) end))
|> Stream.map(&MapSet.size/1)
|> Enum.sum
|> IO.puts
