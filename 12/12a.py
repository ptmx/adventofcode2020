DIRECTIONS = ['E', 'N', 'W', 'S']

def main():
    with open('input.txt') as f:
        lines = [line.strip() for line in f.readlines()]

        x = 0
        y = 0
        orientation = 'E'
        for line in lines:
            direction = line[0]
            value = int(line[1:])

            if direction == 'F':
                direction = orientation

            if direction == 'E':
                x += value
            elif direction == 'W':
                x -= value
            elif direction == 'N':
                y += value
            elif direction == 'S':
                y -= value
            else:
                if value == 90:
                    rotations = 1
                elif value == 180:
                    rotations = 2
                elif value == 270:
                    rotations = 3

                shift = -rotations if direction == 'R' else rotations
                orientation = DIRECTIONS[(DIRECTIONS.index(orientation) + shift) % len(DIRECTIONS)]
        
        print(abs(x) + abs(y))

if __name__ == '__main__':
    main()
