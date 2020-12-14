#include <bitset>
#include <fstream>
#include <iostream>
#include <sstream>
#include <unordered_map>

std::string ONE_MASK = std::bitset<64>(ULLONG_MAX).to_string();
std::string ZERO_MASK = std::bitset<64>(0).to_string();

void updateMasks(
  unsigned long long int &andMask,
  unsigned long long int &orMask,
  std::string value
) {
  std::string newAndMask = ONE_MASK;
  std::string newOrMask = ZERO_MASK;
  for (size_t i = 0; i < value.size(); i++) {
    size_t index = 64 - value.length() + i;
    if (value[i] == 'X') {
      newAndMask[index] = '1';
      newOrMask[index] = '0';
    } else {
      newAndMask[index] = value[i];
      newOrMask[index] = value[i];
    }
  }
  andMask = std::stoull(newAndMask, NULL, 2);
  orMask = std::stoull(newOrMask, NULL, 2);
}

void setMemoryValue(
  std::unordered_map<std::string, unsigned long long int> &m,
  unsigned long long int andMask,
  unsigned long long int orMask,
  std::string command,
  std::string value
) {
  std::string address = command.substr(4, command.length() - 5);
  unsigned long long int intValue = std::stoll(value);
  unsigned long long int maskedValue = (intValue & andMask) | orMask;
  m[command] = maskedValue;
}

int main() {
  unsigned long long int andMask = 0;
  unsigned long long int orMask = 0;
  std::unordered_map<std::string, unsigned long long int> m;

  std::ifstream infile("input.txt");
  std::string line;
  while (std::getline(infile, line)) {
    size_t delimiterPosition = line.find(" = ");
    std::string command = line.substr(0, delimiterPosition);
    std::string value = line.substr(delimiterPosition + 3);

    if (command == "mask") {
      updateMasks(andMask, orMask, value);
    } else {
      setMemoryValue(m, andMask, orMask, command, value);
    }
  }

  unsigned long long int total = 0;
  for (auto& it: m) {
    total += it.second;
  }
  std::cout << total << std::endl;

  return 0;
}
