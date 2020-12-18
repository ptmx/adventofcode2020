const fs = require('fs');
const readline = require('readline');

PARENTHESIS_REGEX = /\(\d+ [+*] \d+( [+*] \d+)*\)/
FIRST_OPERATION_REGEX = /^\d+ [+*] \d+/

function evaluateParenthetical(match) {
  return evaluateLine(match.slice(1, match.length - 1));
}

function evaluateLine(line) {
  while (PARENTHESIS_REGEX.test(line)) {
    line = line.replace(PARENTHESIS_REGEX, evaluateParenthetical);
  }

  while (FIRST_OPERATION_REGEX.test(line)) {
    line = line.replace(FIRST_OPERATION_REGEX, eval, line);
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
