#include <iostream>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <fstream>
#include <vector>
#include <unordered_set>

using std::cout;
using std::endl;
using std::string;
using std::vector;

vector<string> split(string str, char del)
{
  vector<string> tokens = {};
  string acc = "";
  for (uint64_t i = 0; i < str.length(); i++)
  {
    char c = str[i];
    if (c == del)
    {
      tokens.emplace_back(acc);
      acc.clear();
    }
    acc += c;
  }
  // adding left over

  if (acc != "")
    tokens.emplace_back(acc);

  return tokens;
}

vector<int> parse_cards(vector<string> &cards_str)
{
  vector<int> nums = {};
  for (uint64_t i = 1; i < cards_str.size(); i++)
  {
    try
    {
      int n = stoi(cards_str[i]);
      nums.emplace_back(n);
    }
    // coz of bad split
    catch (const std::invalid_argument &err)
    {
      continue;
    }
  }
  return nums;
}

int main(int argc, char const *argv[])
{

  std::fstream input("./input.txt");

  std::string line = "";

  int p2Sum = 0;
  int p1Sum = 0;
  int lineNo = 0;

  std::unordered_map<int, int> copies = {};

  while (std::getline(input, line))
  {
    // Adding once for original card
    if (copies.count(lineNo) == 0)
    {
      copies[lineNo] = 1;
    }
    else
    {
      copies[lineNo] += 1;
    }

    // processing the line
    line = line.substr(line.find(": "));
    std::vector<std::string> parts = split(line, '|');
    auto winning_cards_str = split(parts[0], ' ');
    auto our_cards_str = split(parts[1], ' ');

    auto winning_cards_vec = parse_cards(winning_cards_str);
    std::unordered_set<int> winning_cards(winning_cards_vec.begin(), winning_cards_vec.end());
    auto our_cards = parse_cards(our_cards_str);

    int matches = 0;
    int gameSum = 0;
    for (auto c : our_cards)
    {
      if (winning_cards.count(c) != 0)
      {
        // found a match
        matches += 1;
        gameSum *= 2;
        if (gameSum == 0)
          gameSum = 1;
      }
    }
    p1Sum += gameSum;

    // Increasing count as per cards won

    int copiesWon = copies[lineNo];
    for (int i = lineNo + 1; i <= lineNo + matches; i++)
    {
      if (copies.count(i) == 0)
      {
        copies[i] = copiesWon;
      }
      else
      {
        copies[i] += copiesWon;
      }
    }

    lineNo += 1;
  }

  // summing all the copies count
  for (auto lp : copies)
  {
    p2Sum += lp.second;
  }

  cout << p1Sum << endl;
  cout << p2Sum << endl;

  return 0;
}