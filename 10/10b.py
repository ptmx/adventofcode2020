def main():
    with open('input.txt') as f:
        lines = sorted([int(line.strip()) for line in f.readlines()])

        outlet_joltage = 0
        device_joltage = lines[-1] + 3
        joltages = [outlet_joltage] + lines + [device_joltage]

        dp = [0 for _ in range(device_joltage + 1)]

        for joltage in joltages:
            if joltage == 0:
                dp[joltage] = 1
            else:
                dp[joltage] = (
                    (dp[joltage - 1] if (joltage - 1) >= 0 else 0) +
                    (dp[joltage - 2] if (joltage - 2) >= 0 else 0) +
                    (dp[joltage - 3] if (joltage - 3) >= 0 else 0)
                )
        
        print(dp[device_joltage])

if __name__ == '__main__':
    main()
