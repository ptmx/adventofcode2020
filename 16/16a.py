def parse_range(valid_range):
    min_range, max_range = valid_range.split("-")
    return int(min_range), int(max_range)

def parse_rule(line):
    name, rule = line.split(": ")
    valid_ranges = [parse_range(valid_range) for valid_range in rule.split(" or ")]
    return name, valid_ranges

def parse_ticket(line):
    return [int(n) for n in line.split(',')]

def validate(value, rule):
    return any(min_range <= value <= max_range for min_range, max_range in rule[1])

with open("input.txt") as f:
    lines = [line.strip() for line in f.readlines()]
    your_ticket_index = lines.index("your ticket:")
    nearby_tickets_index = lines.index("nearby tickets:")

    rule_lines = lines[:your_ticket_index - 1]
    your_ticket_line = lines[your_ticket_index + 1]
    nearby_ticket_lines = lines[nearby_tickets_index + 1:]

    validation_rules = [parse_rule(rule_line) for rule_line in rule_lines]
    parsed_nearby_tickets = [parse_ticket(ticket_line) for ticket_line in nearby_ticket_lines]

    error_rate = sum(value
                        for ticket in parsed_nearby_tickets
                        for value in ticket
                        if not any(validate(value, rule) for rule in validation_rules))

    print(error_rate)
