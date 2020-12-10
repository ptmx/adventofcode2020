def main():
    with open('input.txt') as f:
        lines = [int(line.strip()) for line in f.readlines()]
        lines.sort()
        deltas = {}
        for i in range(0, len(lines)):
            difference = lines[i] - (lines[i-1] if i > 0 else 0)
            try:
                deltas[difference] = deltas[difference] + 1
            except KeyError:
                deltas[difference] = 1
        print(deltas[1] * (deltas[3] + 1))

if __name__ == '__main__':
    main()
