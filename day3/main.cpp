#include <iostream>
#include <fstream>
#include <iomanip>
#include <cstdint>
#include <utility>
#include <vector>
#include <string>

using std::cout;
using std::endl;
using std::pair;
using std::string;
using std::vector;

typedef pair<int, int> Coordinate;

inline bool checkSymbol(char b)
{
  return !(b == '.' || (b >= '0' && b <= '9'));
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
    if (checkSymbol(c))
    {
      return true;
    }
  }
  return false;
}

int main(int argc, char const *argv[])
{
  std::fstream input("input.txt");

  std::string test_input((std::istreambuf_iterator<char>(input)),
                         std::istreambuf_iterator<char>());

  uint32_t lineLength = test_input.find_first_of('\n') + 1;
  uint64_t sum = 0;
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

      // if (col_start < 0)
      // {
      //   col_start = lineLength - col_start;
      // }

      // if (col_end < 0)
      // {
      //   col_end = lineLength - col_end;
      // }

      if (checkPure(col_start, col_end, row_count, test_input, lineLength))
      {
        sum += value;
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
      sum += value;
      cout << "ADDING " << value << endl;

      acc.clear();
    }
  }

  cout << sum;

  return 0;
}