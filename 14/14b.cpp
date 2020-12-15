#include <bitset>
#include <cmath>
#include <fstream>
#include <iostream>
#include <sstream>
#include <unordered_map>
#include <vector>

std::string ONE_MASK = std::bitset<64>(ULLONG_MAX).to_string();
std::string ZERO_MASK = std::bitset<64>(0).to_string();

void flipBit(unsigned long long int &value, int index, unsigned long long int bit) {
  if (bit == 1) {
    value = value | (1ULL << index);
  } else {
    value = value & ~(1ULL << index);
  }
}

void updateMasks(
  unsigned long long int &andMask,
  unsigned long long int &orMask,
  std::vector<int> &xIndexes,
  std::string value
) {
  xIndexes.clear();
  std::string newAndMask = ONE_MASK;
  std::string newOrMask = ZERO_MASK;
  for (size_t i = 0; i < value.size(); i++) {
    size_t index = 64 - value.length() + i;
    if (value[i] == '1') {
      newOrMask[index] = '1';
    } else if (value[i] == 'X') {
      xIndexes.push_back(index);
      newAndMask[index] = '0';
    }
  }
  andMask = std::stoull(newAndMask, NULL, 2);
  orMask = std::stoull(newOrMask, NULL, 2);
}

void setMemoryValue(
  std::unordered_map<unsigned long long int, unsigned long long int> &m,
  unsigned long long int andMask,
  unsigned long long int orMask,
  std::vector<int> xIndexes,
  std::string command,
  std::string value
) {
  std::string address = command.substr(4, command.length() - 5);
  unsigned long long int baseAddress = (std::stoll(address) & andMask) | orMask;
  unsigned long long int intValue = std::stoll(value);

  for (size_t i = 0; i < pow(2, xIndexes.size()); i++) {
    unsigned long long int floatingAddress = baseAddress;
    for (size_t j = 0; j < xIndexes.size(); j++) {
      flipBit(floatingAddress, 63 - xIndexes[j], (i & (1 << j)) >> j);
    }
    m[floatingAddress] = intValue;
  }
}

int main() {
  unsigned long long int andMask = 0;
  unsigned long long int orMask = 0;
  std::vector<int> xIndexes;
  std::unordered_map<unsigned long long int, unsigned long long int> m;

  std::ifstream infile("input.txt");
  std::string line;
  while (std::getline(infile, line)) {
    size_t delimiterPosition = line.find(" = ");
    std::string command = line.substr(0, delimiterPosition);
    std::string value = line.substr(delimiterPosition + 3);

    if (command == "mask") {
      updateMasks(andMask, orMask, xIndexes, value);
    } else {
      setMemoryValue(m, andMask, orMask, xIndexes, command, value);
    }
  }

  unsigned long long int total = 0;
  for (auto& it: m) {
    total += it.second;
  }
  std::cout << total << std::endl;

  return 0;
}
