from math import prod

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

def transpose(list_2d):
    return [list(column) for column in zip(*list_2d)]

def main():
    with open("input.txt") as f:
        lines = [line.strip() for line in f.readlines()]
        your_ticket_index = lines.index("your ticket:")
        nearby_tickets_index = lines.index("nearby tickets:")

        rule_lines = lines[:your_ticket_index - 1]
        your_ticket_line = lines[your_ticket_index + 1]
        nearby_ticket_lines = lines[nearby_tickets_index + 1:]

        validation_rules = [parse_rule(rule_line) for rule_line in rule_lines]
        parsed_your_ticket = parse_ticket(your_ticket_line)
        parsed_nearby_tickets = [parse_ticket(ticket_line) for ticket_line in nearby_ticket_lines]

        valid_rows = [ticket 
                        for ticket in parsed_nearby_tickets
                        if all(
                            any(validate(value, rule) for rule in validation_rules) 
                            for value in ticket)]
        
        column_values = transpose(valid_rows)

        possible_column_mappings = [(index,
                                    [rule[0] 
                                       for rule in validation_rules 
                                       if all(validate(value, rule) for value in column)
                                    ])
                                    for index, column in enumerate(column_values)]

        sorted_column_mappings = sorted(possible_column_mappings, key=lambda x: len(x[1]), reverse=True)

        mapped_columns = {}
        while len(sorted_column_mappings) > 0:
            index, columns = sorted_column_mappings.pop()
            remaining_possible_mappings = [column for column in columns if column not in mapped_columns]
            assert len(remaining_possible_mappings) == 1
            column = remaining_possible_mappings[0]
            mapped_columns[column] = index

        departure_column_indexes = [index for column, index in mapped_columns.items() if column.startswith("departure")]
        print(prod(parsed_your_ticket[index] for index in departure_column_indexes))

if __name__ == "__main__":
    main()
