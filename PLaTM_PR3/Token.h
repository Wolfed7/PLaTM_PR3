#pragma once

using namespace std;

enum TokenType
{
   TokenOperator,
   TokenSeparator,
   TokenWord,
   TokenConstant,
   TokenBracket,
   TokenError,
};

class Token 
{
public:
   size_t tableID;
   size_t rowID;

   string ToStr()
   {
      return "(" + to_string(tableID) + "," + to_string(rowID) + ")";
   }
};