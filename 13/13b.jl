lines = readlines("input.txt")

bus_ids = split(lines[2], ",")
buses = [(parse(Int, bus_id), index - 1) for (index, bus_id) in enumerate(bus_ids) if bus_id != "x"]

lcm = reduce(*, [bus_id for (bus_id, _) in buses])

mn = [lcm ÷ bus_id for (bus_id, _) in buses]
yn = [(bus_id - bus_index) % bus_id for (bus_id, bus_index) in buses]
tn = [invmod(mi, bus_id) for ((bus_id, _), mi) in zip(buses, mn)]

t = reduce(+, [ti * mi * yi for (ti, mi, yi) in zip(tn,mn,yn)])

println(t % lcm)
