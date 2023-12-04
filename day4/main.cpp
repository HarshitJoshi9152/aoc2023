#include <iostream>
#include <ranges>
#include <iomanip>
#include <cstdint>
#include <string>
#include <unordered_map>
#include <sstream>
#include <algorithm>
#include <fstream>
#include <array>
#include <vector>
#include <utility>
#include <unordered_set>

using std::cout;
using std::endl;
using std::string;
using std::vector;

// vector<string> split(string str, char del)
// {
//   vector<string> tokens = {};
//   int cursor = 0;
//   int start = 0;
//   while ((cursor = str.find(del, start)) != std::string::npos)
//   {
//     string token = str.substr(start, cursor);
//     tokens.emplace_back(token);
//     start = cursor + 1;
//   }
//   // add rest of the strng
//   tokens.emplace_back(str.substr(start));
//   return tokens;
// }

vector<string> split(string str, char del)
{

  vector<string> tokens = {};
  string acc = "";
  for (int i = 0; i < str.length(); i++)
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

// std::vector<std::string> split(const std::string &s, char delim)
// {
//   std::vector<std::string> result;
//   std::stringstream ss(s);
//   std::string item;

//   while (getline(ss, item, delim))
//   {
//     result.push_back(item);
//   }

//   return result;
// }

vector<int> parse_cards(vector<string> &cards_str)
{
  vector<int> nums = {};
  for (int i = 1; i < cards_str.size(); i++)
  {
    // cout << cards_str[i] << endl;
    try
    {
      int n = stoi(cards_str[i]);
      nums.emplace_back(n);
    }
    // coz of bad split
    catch (std::invalid_argument err)
    {
      continue;
    }
  }

  // cout << ":END:" << endl;
  return nums;
}

int main(int argc, char const *argv[])
{

  std::fstream input("./input.txt");

  std::string line = "";

  int sum = 0;

  // lineNo: Copies
  // std::unordered_map<int, int> copies = {};
  while (std::getline(input, line))
  {
    // Remove till ": "

    line = line.substr(line.find(": "));

    cout << line << endl;
    std::vector<std::string> parts = split(line, '|');

    auto winning_cards_str = split(parts[0], ' ');
    auto our_cards_str = split(parts[1], ' ');

    auto winning_cards_vec = parse_cards(winning_cards_str);
    std::unordered_set<int> winning_cards(winning_cards_vec.begin(), winning_cards_vec.end());
    auto our_cards = parse_cards(our_cards_str);

    int gameSum = 0;
    bool foundOne = false;
    for (auto c : our_cards)
    {
      if (winning_cards.count(c) != 0)
      {
        // found a match
        gameSum *= 2;
        if (gameSum == 0)
          gameSum = 1;
      }
      // cout << c << endl;
    }

    sum += gameSum;
  }

  cout << sum << endl;

  return 0;
}