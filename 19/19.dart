import 'dart:io';
import 'dart:collection';

void solve(bool partB) {
  List<String> lines = new File("input.txt").readAsLinesSync();
  List<String> ruleLines = lines.sublist(0, lines.indexOf(""));
  List<String> messages = lines.sublist(lines.indexOf("") + 1);

  var ruleMap = ruleLines.fold(new HashMap(), (ruleMap, line) {
    var parts = line.split(': ');
    var ruleId = parts[0];
    var rule = parts[1];
    // Group OR conditions together with parentheses
    rule = rule.replaceFirstMapped(
      new RegExp(r"(\d+.*) \| (\d+.*)$"),
      (match) => "((${match.group(1)}) | (${match.group(2)}))"
    );
    ruleMap[ruleId] = rule;
    return ruleMap;
  });

  if (partB) {
    ruleMap["8"] = "((42)+)"; // 42 | 42 8
    ruleMap["11"] = "((42 31)|(42 42 31 31)|(42 42 42 31 31 31)|(42 42 42 42 31 31 31 31)|(42 42 42 42 42 31 31 31 31 31))"; // 42 31 | 42 11 31
  }

  String rule = ruleMap["0"];
  while (new RegExp(r"\d+").hasMatch(rule)) {
    rule = rule.replaceAllMapped(new RegExp(r"\d+"), (match) => ruleMap[match.group(0)]);
  }

  rule = rule.replaceAll(new RegExp(r'[" ]'), '');

  var ruleRegex = new RegExp("^${rule}\$");

  var total = messages.fold(0, (total, message) => total + (ruleRegex.hasMatch(message) ? 1 : 0));
  print(total);
}

main() {
  solve(false);
  solve(true);
}
