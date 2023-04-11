#pragma once
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <string>
#include <stack>

#include "token.h"
#include "tables.h"

using namespace std;

struct ParseTableRow 
{
   vector<string> terminals_;
   int jump_;
   bool accept_;
   bool stack_;
   bool return_;
   bool error_;
};

struct ParserError
{
   string msg;
   int line;
};

class ParseTable
{
private:
   vector<ParseTableRow> data;

public:
   ParseTable(string filename);
   void push(ParseTableRow row);
   ParseTableRow &at(size_t idx);
   void print();
};

class Parser
{
   ParseTable *table;
   Tables tables;
   vector<ParserError> errors;
   vector<size_t> tokenLineIndeces;
   vector<Token> polish;

private:
   bool ContainsTerminal(vector<string> terminals, string terminal);
   void PushError(ParserError error);

public:
   // Комментарии можно поискать в parser.cpp
   Parser(ParseTable *table, Tables tables, vector<size_t> tokenLineIndexes);
   void Parse(vector<Token> tokens);
   string GetTokenStr(Token);
   string GetRealTokenStr(Token);
   vector<string> markTable;
   vector<ParserError> GetErrors();
   vector<Token> GetPolish();
};