#include "parser.h"
#include "tables.h"
#include "token.h"
#include <cstdlib>
#include <vector>

using namespace std;

enum TypesID
{
   TYPE_NULL,
   TYPE_FLOAT,
   TYPE_INT,
   TYPE_CHAR
};

// -----------------------------------------------------------
// PARSE TABLE
// -----------------------------------------------------------

ParseTable::ParseTable(string filename) 
{
   ifstream file(filename);
   string line;

   getline(file, line);

   // Заполняем табличку из файла, строка за строкой
   while (getline(file, line))
   {
      stringstream lineStream(line);

      vector<string> words;
      string word;

      while (getline(lineStream, word, '\t'))
         words.push_back(word);

      if (words[0] == "94")
      {
         int a = 0;
      }

      ParseTableRow row;

      stringstream terminalStream(words[1]);
      string terminal;

      while (getline(terminalStream, terminal, ' '))
         row.terminals_.push_back(terminal);

      row.jump_ = stoi(words[2]);
      row.accept_ = stoi(words[3]);
      row.stack_ = stoi(words[4]);
      row.return_ = stoi(words[5]);
      row.error_ = stoi(words[6]);

      data.push_back(row);
   }
}

ParseTableRow &ParseTable::at(size_t idx)
{
   // TODO: Index out of array exception
   return data[idx - 1];
}

void ParseTable::push(ParseTableRow row) 
{
   data.push_back(row);
}

void ParseTable::print() 
{
   for (auto &row : data) 
   {

      for (auto &word : row.terminals_)
         cout << word << " ";

      cout << row.jump_ << " ";
      cout << row.accept_ << " ";
      cout << row.stack_ << " ";
      cout << row.return_ << " ";
      cout << row.error_ << endl;
   }
}

// -----------------------------------------------------------
// ADDITIONAL FUNCTIONS
// -----------------------------------------------------------

int GetPriority(size_t op) 
{
   switch (op) 
   {
   case 0:
   case 1:
      return 5;

   case 2:
      return 4;

   case 3:
      return 12;

   case 4:
   case 5:
      return 8;

   case 6:
   case 7:
      return 7;
   }

   return 199;
}

// -----------------------------------------------------------
// PARSER
// -----------------------------------------------------------

Parser::Parser(ParseTable *table, Tables tables, vector<size_t> tokenLineIndeces)
{
   this->table = table;
   this->tables = tables;
   this->tokenLineIndeces = tokenLineIndeces;
}

string Parser::GetTokenStr(Token token)
{
   string tokenStr;

   switch (token.tableID)
   {
   case StaticBrackets:
      tokenStr = tables.GetStaticLex(pair<int, int>({ StaticBrackets, token.rowID }));
      break;

   case StaticSpecials:
      tokenStr = tables.GetStaticLex(pair<int, int>({ StaticSpecials, token.rowID }));
      break;

   case StaticOperators:
      tokenStr = tables.GetStaticLex(pair<int, int>({ StaticOperators, token.rowID }));
      break;

   case StaticKeywords:
      tokenStr = tables.GetStaticLex(pair<int, int>({ StaticKeywords, token.rowID }));
      break;

   case StaticSeparators:
      tokenStr = tables.GetStaticLex(pair<int, int>({ StaticSeparators, token.rowID }));
      break;

   case DynamicVariables:
      tokenStr = "var";
      break;

   case DynamicConstants:
      tokenStr = "const";
      break;
   }

   return tokenStr;
}

string Parser::GetRealTokenStr(Token token)
{
   string tokenStr;

   switch (token.tableID)
   {
   case StaticBrackets:
   case StaticSpecials:
   case StaticOperators:
   case StaticKeywords:
   case StaticSeparators:
      tokenStr = GetTokenStr(token);
      break;

   case DynamicVariables:
      tokenStr = tables.SearchOnDynamic(pair<int, int>({ DynamicVariables, token.rowID }))->Name;
      break;

   case DynamicConstants:
      tokenStr = tables.SearchOnDynamic(pair<int, int>({ DynamicConstants, token.rowID }))->Name;
      break;
   }

   return tokenStr;
}

bool Parser::containsTerminal(vector<string> terminals, string terminal) 
{
   for (auto &t : terminals)
      if (t.compare(terminal) == 0)
         return true;

   return false;
}

vector<Token> Parser::GetPolish() 
{
   return polish;
}

void Parser::Parse(vector<Token> tokens)
{
   stack<size_t> st;
   stack<Token> outputBuf;
   size_t currentRow = 1;
   size_t currentTokenNumber = 0;
   size_t currentLine = 1;
   TypesID tempTypeID = TYPE_NULL;

   // For RGZ
   enum NumberType
   {
      NUMBER_INTEGER,
      NUMBER_FLOAT
   };

   NumberType rExpr = NUMBER_INTEGER;
   NumberType lExpr = NUMBER_INTEGER;

   outputBuf.push({ 100, 100 });

   for (auto &token : tokens)
   {
      string tokenStr = GetTokenStr(token);

      while (tokenLineIndeces[currentLine - 1] == currentTokenNumber)
         currentLine++;

      while (true)
      {
         ParseTableRow row = table->at(currentRow);
         size_t tempCurrentRow = currentRow;

         //cout << row.jump_ << " |\t" << currentRow << " &\t" << tokenStr;
         //getchar();

         if (currentRow == 27)       tempTypeID = TYPE_INT;
         else if (currentRow == 28)  tempTypeID = TYPE_CHAR;
         else if (currentRow == 29)  tempTypeID = TYPE_FLOAT;
         else if (currentRow == 23)  tempTypeID = TYPE_NULL;

         if (row.stack_)
         {
            //cout << "- PUSH TO STACK " << currentRow << endl;
            st.push(currentRow);
         }

         if (row.return_)
         {
            //cout << " . RETURN TO " << st.top() << endl;
            currentRow = st.top() + 1;
            st.pop();
         }
         else if (row.jump_ != 0 && containsTerminal(row.terminals_, tokenStr))
         {
            //cout << " . JUMP TO " << row.jump_ << endl;
            currentRow = row.jump_;
         }
         else if (!row.error_)
         {
            currentRow += 1;
         }
         else
         {
            string msg = "Неизвестная ошибка: ";
            msg += to_string(currentRow);
            ParserError err = { msg, (int)currentLine };

            switch (currentRow)
            {
            case 22:
               err.msg = "Отсутствует имя переменной";
               break;

            case 20:
            case 71:
               err.msg = "Неверный баланс скобок";
               break;

            case 31:
               err.msg = "Пропущена точка с запятой";
               break;

            case 48:
               err.msg = "Неправильная конструкция. Возможно, вы забыли '='?";
               break;

            case 64:
               err.msg = "Опущен операнд";
               break;

            case 70:
               err.msg = "В скобках должно быть хоть что-то";
               break;

            default:
               break;
            }

            PushError(err);
            return;
         }

         Variable *constant;
         Variable *var;

         if (row.accept_) 
         {
            // Проверим, существует ли переменная или константа в таблицах.
            switch (token.tableID) 
            {
            case DynamicConstants:
               constant = tables.SearchOnDynamic(pair<int, int>({ DynamicConstants, token.rowID }));

               if (constant->Type == TYPE_FLOAT)
                  rExpr = NUMBER_FLOAT;

               break;

            case DynamicVariables:
               var = tables.SearchOnDynamic(pair<int, int>({ DynamicVariables, token.rowID }));

               if (var->Type == TYPE_NULL)
                  if (tempCurrentRow == 30)
                  {
                     var->Type = tempTypeID;
                  }
                  else
                  {
                     ParserError err = { "Неопределённая переменная", (int)currentLine };
                     PushError(err);
                     return;
                  }
               else
                  if (tempCurrentRow == 30)
                  {
                     ParserError err = { "Переопределённая переменная", (int)currentLine };
                     PushError(err);
                     return;
                  }

               if (var->Type == TYPE_FLOAT)
                  if (tempCurrentRow == 30 || tempCurrentRow == 47)
                     lExpr = NUMBER_FLOAT;
                  else
                     rExpr = NUMBER_FLOAT;

               break;

            case StaticSeparators:
               if (lExpr == NUMBER_INTEGER && rExpr == NUMBER_FLOAT)
               {
                  ParserError err = { "Невозможно приравнять вещественное число к целому", (int)currentLine };
                  PushError(err);
                  return;
               }

               lExpr = NUMBER_INTEGER;
               rExpr = NUMBER_INTEGER;

               break;

            case StaticOperators:
               if (tempCurrentRow == 58)
               {
                  //tables.constants->Update("0", Char(0));
                  //Token zero = { DynamicConstants, tables.constants->getHash("0") };
                  cout << "checkhere" << endl;
                  //polish.push_back(zero);
               }
               break;
            }

            // --------------------------------
            //  Работаем со стеком.
            // --------------------------------
            if (  token.tableID == DynamicConstants
               || token.tableID == DynamicVariables
               || token.tableID == StaticSeparators
               || token.tableID == StaticOperators
               || (token.tableID == StaticBrackets && currentRow > 9 && (token.rowID == 0 || token.rowID == 1))) 
            {

               switch (token.tableID)
               {
               case DynamicConstants:
               case DynamicVariables:
                  polish.push_back(token);
                  break;

               case StaticSeparators:
                  while (outputBuf.top().tableID != 100)
                  {
                     polish.push_back(outputBuf.top());
                     outputBuf.pop();
                  }

                  polish.push_back(token);
                  break;

               case StaticBrackets:
                  if (token.rowID == 0)
                  {
                     outputBuf.push(token);
                  }
                  else if (token.rowID == 1)
                  {
                     while (!(outputBuf.top().tableID == StaticBrackets && outputBuf.top().rowID == 0))
                     {
                        polish.push_back(outputBuf.top());
                        outputBuf.pop();
                     }

                     outputBuf.pop();
                  }
                  break;

               case StaticOperators:
                  size_t op = token.rowID;
                  Token last = outputBuf.top();

                  if (last.tableID == 100) 
                     outputBuf.push(token);
                  else if (last.tableID == StaticOperators)
                     if (GetPriority(op) <= GetPriority(outputBuf.top().rowID))
                     {
                        outputBuf.push(token);
                     }
                     else
                     {
                        polish.push_back(last);
                        outputBuf.pop();
                        outputBuf.push(token);
                     }
                  else
                     outputBuf.push(token);

                  break;
               }
            }
            break;
         }
      }
      currentTokenNumber++;
      //cout << ">|\t" << tokenStr << endl;
   }

   if (!st.empty())
   {
      ParserError err = { "Стек парсера не пустой. Возможно, вы забыли '}'?", (int)currentLine };
      PushError(err);
      return;
   }

   //cout << endl;
}

void Parser::PushError(ParserError error)
{
   errors.push_back(error);
}

vector<ParserError> Parser::GetErrors()
{
   return errors;
}