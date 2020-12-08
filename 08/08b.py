class InstructionIndexOutOfBoundsError(Exception):
    pass

def floyd(f, x0):
    tortoise = f(x0)
    hare = f(f(x0))
    while tortoise[0] != hare[0]:
        tortoise = f(tortoise)
        hare = f(f(hare))
  
    mu = 0
    tortoise = x0
    while tortoise[0] != hare[0]:
        tortoise = f(tortoise)
        hare = f(hare)
        mu += 1
 
    lam = 1
    hare = f(tortoise)
    while tortoise[0] != hare[0]:
        hare = f(hare)
        lam += 1
 
    return lam, mu

def parse_instruction(line):
    line = line.strip()
    [instruction, value] = line.split(' ')
    return instruction, int(value)

def main():
    with open('input.txt') as f:
        instructions = [parse_instruction(line) for line in f.readlines()]

        def execute_instruction(args):
            index, acc = args

            if index >= len(instructions):
                raise InstructionIndexOutOfBoundsError()

            instruction, value = instructions[index]
            if instruction == 'nop':
                return index + 1, acc
            elif instruction == 'acc':
                return index + 1, acc + value
            elif instruction == 'jmp':
                return index + value, acc
            else:
                raise Exception('Unknown instruction code')

        for count, (instruction, value) in enumerate(instructions):
            if instruction == 'acc':
                continue
            
            new_instruction = 'jmp' if instruction == 'nop' else 'nop'
            instructions[count] = (new_instruction, value)

            try:
                lam, mu = floyd(execute_instruction, (0, 0))

                # restore initial instruction
                instructions[count] = (instruction, value)
            except InstructionIndexOutOfBoundsError:
                break

        index = 0
        acc = 0
        while True:
            try:
                index, acc = execute_instruction((index, acc))
            except InstructionIndexOutOfBoundsError:
                print(acc)
                break

if __name__ == '__main__':
    main()
