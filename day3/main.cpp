#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdint>
#include <array>
#include <cmath>
#include <utility>
#include <vector>
#include <string>

using std::cout;
using std::endl;
using std::pair;
using std::string;
using std::vector;

#define pair(x, y) std::make_pair(x, y)

typedef pair<int, int> Coordinate;

inline bool checkSymbol(char b)
{
  return !(b == '.' || (b >= '0' && b <= '9'));
}

int dist(int p1, int p2, int q1, int q2)
{
  return std::sqrt(std::pow(q1 - p1, 2) + std::pow(q2 - p2, 2) * 1.0);
}

bool isGear(int r, int c, int lineLength, string buffer)
{
  int rowLimit = buffer.length() / lineLength;

  std::array<Coordinate, 9> coors = {
      {
          {r - 1, c - 1},
          {r - 1, c},
          {r - 1, c + 1},

          {r, c - 1},
          {r, c},
          {r, c + 1},

          {r + 1, c - 1},
          {r + 1, c},
          {r + 1, c + 1},

      }};

  std::vector<Coordinate> numbers = {};
  for (int i = 0; i < coors.size(); i++)
  {
    Coordinate coor = coors[i];
    int r = coor.first;
    int c = coor.second;

    if (r < 0 || r > rowLimit - 1)
      continue;
    if (c > lineLength - 1 || c < 0)
      continue;

    char symbol = buffer[r * lineLength + c];

    if (std::isdigit(symbol))
    {
      numbers.emplace_back(pair(r, c));
    }
  }

  for (auto n : numbers)
  {
    int r, c;
    std::tie(r, c) = n;
    for (auto nn : numbers)
    {
      int rr, cc;
      std::tie(rr, cc) = nn;
      if (dist(r, c, rr, cc) >= 2)
      {
        return true;
      }
    }
  }
  return false;
}

bool checkPure(int64_t cs, int64_t ce, int64_t rc, string buffer, int lineLength)
{

  int rowLimit = buffer.length() / lineLength;
  //       ............
  // rc -> .cs .... ce.
  //       ............

  vector<Coordinate> coorsBuffer;

  if (!cs - 1 < 0)
  {
    coorsBuffer.emplace_back(std::make_pair(rc, cs - 1));
  }

  if (!ce + 1 > lineLength - 1)
  {
    coorsBuffer.emplace_back(std::make_pair(rc, cs + 1));
  }

  // - 1 and + 1 to check diagonals too
  for (int i = cs - 1; i <= ce + 1; i++)
  {

    // checking bounds
    if (i < 0 || i >= lineLength)
      continue;
    if (rc - 1 >= 0)
      coorsBuffer.emplace_back(std::make_pair(rc - 1, i));
    if (rc + 1 < rowLimit)
      coorsBuffer.emplace_back(std::make_pair(rc + 1, i));
  }

  for (auto p : coorsBuffer)
  {
    // Check if indexes are valid
    int row = p.first;
    int col = p.second;

    char c = buffer[row * lineLength + col];
    // if (checkSymbol(c))
    if (c == '*' && isGear(row, col, lineLength, buffer))
    {
      return true;
    }
  }
  return false;
}

int main(int argc, char const *argv[])
{
  std::fstream input("input.txt");

  // std::string test_input((std::istreambuf_iterator<char>(input)),
  //                        std::istreambuf_iterator<char>());

  string test_input =
      "*12.......*..*\n"
      "*+.........34*\n"
      "*.......-12..*\n"
      "*..78........*\n"
      "*..*....60...*\n"
      "*78.........9*\n"
      "*.5.....23..$*\n"
      "*8...90*12...*\n"
      "*............*\n"
      "*2.2......12.*\n"
      "*.*.........**\n"
      "*1.1..503+.56*\n";

  uint32_t lineLength = test_input.find_first_of('\n') + 1;
  uint64_t sum = 0;
  vector<int> toSum = {};
  std::string acc = "";

  uint64_t row_count = 0;
  for (int i = 0; i < test_input.length(); i++)
  {
    char c = test_input[i];
    if (c == '\n')
    {
      row_count += 1;
    }
    else if (c >= '0' && c <= '9')
    {
      acc += c;
    }
    else if (c == '.')
    {
      if (acc == "")
        continue;
      // convert acc to int and add...
      int value = stoi(acc);

      // We need to have the starting and ending of the
      int col_start = i % lineLength - acc.length();
      int col_end = i % lineLength - 1;

      if (checkPure(col_start, col_end, row_count, test_input, lineLength))
      {
        // sum += value;
        toSum.emplace_back(value);
        cout << "ADDING " << value << endl;
      }
      acc.clear();
    }
    else
    {
      // This is a symbol
      if (acc == "")
        continue;
      int value = stoi(acc);
      // sum += value;
      toSum.emplace_back(value);
      cout << "ADDING " << value << endl;

      acc.clear();
    }
  }
  cout << "Size -> " << toSum.size() << endl;
  for (int i = 0; i < toSum.size(); i += 2)
  {
    int a = toSum[i];
    int b = toSum[i + 1];

    sum += a * b;
  }

  cout << sum;

  return 0;
}