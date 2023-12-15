#include <iostream>
#include <iomanip>
#include <cstdint>
#include <string>
#include <fstream>
#include <array>
#include <vector>
#include <unordered_map>
#include <forward_list>
#include <ranges>
#include <string_view>

using std::cout;
using std::endl;
using std::string;
using std::unordered_map;
using std::vector;

typedef unordered_map<int, int> ResourceMap;
// Should i use a unordered_map<forward_list> ? or a

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

void make_mappings(std::string, ResourceMap &map)
{
  // Process the string and map to map
}

int main(int argc, char const *argv[])
{
  // WE need to map seed no to soil nos.

  std::fstream file("./input.txt");

  std::string input((std::istreambuf_iterator<char>(file)),
                    std::istreambuf_iterator<char>());

  ResourceMap map_seed_to_soil;
  ResourceMap map_soil_to_fertilizer;
  ResourceMap map_fertilizer_to_water;
  ResourceMap map_water_to_light;
  ResourceMap map_light_to_temperature;
  ResourceMap map_temperature_to_humidity;
  ResourceMap map_humidity_to_location;

  vector<int> seeds = {};

  auto lines = (input | std::views::split(std::string("\n\n")) | std::ranges::to<std::vector<string>>());

  // vector<string> parts = split(input, '\n');

  for (auto i : lines)
  {
    cout << i << endl;
  }

  return 0;
}