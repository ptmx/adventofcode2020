def main():
    with open('input.txt') as f:
        lines = [line.strip() for line in f.readlines()]

        x = 0
        y = 0
        waypoint_x = 10
        waypoint_y = 1
        for line in lines:
            direction = line[0]
            value = int(line[1:])

            if direction == 'E':
                waypoint_x += value
            elif direction == 'W':
                waypoint_x -= value
            elif direction == 'N':
                waypoint_y += value
            elif direction == 'S':
                waypoint_y -= value
            elif direction == 'F':
                x += waypoint_x * value
                y += waypoint_y * value
            else:
                if value == 90:
                    rotations = 1
                elif value == 180:
                    rotations = 2
                elif value == 270:
                    rotations = 3
                
                for _ in range(0, rotations):
                    if direction == 'R':
                        waypoint_x, waypoint_y = waypoint_y, -waypoint_x
                    elif direction == 'L':
                        waypoint_x, waypoint_y = -waypoint_y, waypoint_x
        
        print(abs(x) + abs(y))

if __name__ == '__main__':
    main()
