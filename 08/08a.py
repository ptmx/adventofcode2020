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
            instruction, value = instructions[index]
            if instruction == 'nop':
                return index + 1, acc
            elif instruction == 'acc':
                return index + 1, acc + value
            elif instruction == 'jmp':
                return index + value, acc
            else:
                raise Exception('Unknown instruction code')
        
        lam, mu = floyd(execute_instruction, (0, 0))

        index = 0
        acc = 0
        steps = 0
        while steps < mu + lam:
            index, acc = execute_instruction((index, acc))
            steps += 1
        
        print(acc)

if __name__ == '__main__':
    main()
