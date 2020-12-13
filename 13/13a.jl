lines = readlines("input.txt")

arrival_time = parse(Int, lines[1])
buses = [parse(Int, bus_id) for bus_id in split(lines[2], ",") if bus_id != "x"]

min_wait_time = nothing
min_wait_bus = nothing
for bus ∈ buses
    wait_time = bus - (arrival_time % bus)
    if (min_wait_time == nothing || wait_time < min_wait_time)
        global min_wait_time = wait_time
        global min_wait_bus = bus
    end
end

print(min_wait_time * min_wait_bus)
