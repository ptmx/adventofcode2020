const fs = require('fs');
const readline = require('readline');

PARENTHESIS_REGEX = /\(\d+ [+*] \d+( [+*] \d+)*\)/
ADDITION_REGEX = /\d+ \+ \d+/
MULTIPLICATION_REGEX = /\d+ \* \d+/
EVALUATED_REGEX = /^\d+$/

function evaluateParenthetical(match) {
  return evaluateLine(match.slice(1, match.length - 1));
}

function evaluateLine(line) {
  while (PARENTHESIS_REGEX.test(line)) {
    line = line.replace(PARENTHESIS_REGEX, evaluateParenthetical);
  }

  while (!EVALUATED_REGEX.test(line)) {
    while (ADDITION_REGEX.test(line)) {
      line = line.replace(ADDITION_REGEX, eval);
    }

    line = line.replace(MULTIPLICATION_REGEX, eval);
  }

  return line;
}

async function solve() {
  const fileStream = fs.createReadStream('input.txt');

  const rl = readline.createInterface({
    input: fileStream
  });

  let sum = 0;
  for await (const line of rl) {
    sum += parseInt(evaluateLine(line), 10);
  }

  console.log(sum);
}

solve();
