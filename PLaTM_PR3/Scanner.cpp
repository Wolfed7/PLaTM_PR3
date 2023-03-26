#include "Scanner.h"
#include "Token.h"
#include "Tables.h"

using namespace std;

Scanner::Scanner(Tables tables) 
{
   tables_ = tables;
   state_ = NULL;
}

void Scanner::StartScan(string code) 
{
   tokenLineIndexes.push_back(0);
   ChangeStateTo(new ScannerStateStart());

   for (char lexeme : code) 
   {
      state_->update(*this, lexeme);

      if (lexeme == '\n') 
      {
         tokenLineIndexes.push_back(tokens.size());
         currentLine++;
      }
   }

   if (multilineComment) 
   {
      PushError("Многострочный комментарий не закрыт.");
   }

   state_->update(*this, ' ');
}

void Scanner::ChangeStateTo(ScannerState *newState) 
{
   if (state_ != NULL)
      delete state_;
   state_ = newState;
}

void Scanner::PushToBuffer(char lexeme) 
{
   buf.push_back(lexeme);
}

void Scanner::PushError(string errorMsg) 
{
   ScannerError err = { errorMsg, GetLine() };
   errors.push_back(err);
}

int Scanner::GetLine() 
{
   return currentLine;
}

vector<size_t> Scanner::GetTokenLineIndexes() 
{
   return tokenLineIndexes;
}

void Scanner::ClearBuffer() 
{
   buf.clear();
}

void Scanner::CreateToken(TokenType type) 
{
   auto tokenStr = GetBufferAsString();
   pair<int, int> staticAddress;
   staticAddress = tables_.SearchOnStatic(tokenStr);

   switch (type) 
   {
   case TokenOperator:
   {
      if (staticAddress != FAILURE_PAIR)
      {
         Token token = { StaticOperators, staticAddress.second };
         tokens.push_back(token);
      }
      else
      {
         PushError("Неопознанная операция.");
      }
      break;
   }

   case TokenSeparator:
   {
      if (staticAddress != FAILURE_PAIR)
      {
         Token token = { StaticSeparators, staticAddress.second };
         tokens.push_back(token);
      }
      else
      {
         PushError("Неопознанный разделитель.");
      }
      break;
   }

   case TokenWord:
   {
      if (staticAddress.first == StaticKeywords)
      {
         Token token = { StaticKeywords,  staticAddress.second };
         tokens.push_back(token);
      }
      else if (staticAddress.first == StaticSpecials)
      {
         Token token = { StaticSpecials, staticAddress.second };
         tokens.push_back(token);
      }
      else
      {
         pair<int, int> newDynamicAddr = tables_.IncludeInDynamic(tokenStr, None, false);
         Token token = { DynamicVariables, newDynamicAddr.second };
         tokens.push_back(token);
      }
      break;
   }

   case TokenConstant:
   {
      for (size_t i = 0; i < tokenStr.size(); i++)
         if (!isdigit(buf[i]))
         {
            PushError("Задана неправильная числовая константа.");
            break;
         }

      pair<int, int> newDynamicAddr = tables_.IncludeInDynamic(tokenStr, Int);
      Token token = { DynamicConstants, newDynamicAddr.second };
      tokens.push_back(token);

      break;
   }

   case TokenBracket:
   {
      if (staticAddress != FAILURE_PAIR)
      {
         Token token = { StaticBrackets, staticAddress.second };
         tokens.push_back(token);
      }
      break;
   }

   case TokenError:
      break;
   }

   ClearBuffer();
}

string Scanner::GetBufferAsString() 
{
   string str(buf.begin(), buf.end());
   return str;
}

vector<ScannerError> Scanner::GetErrors() 
{
   return errors;
}

vector<Token> Scanner::GetTokens() 
{
   return tokens;
}

// Состояния конечного автомата.
// Начинаем со стартового.
void ScannerStateStart::update(Scanner &scanner, char lexeme) 
{
   if (isalpha(lexeme) || lexeme == '_') 
   {
      scanner.ChangeStateTo(new ScannerStateWord());
      scanner.PushToBuffer(lexeme);
   }
   else if (isdigit(lexeme)) 
   {
      scanner.ChangeStateTo(new ScannerStateNumber());
      scanner.PushToBuffer(lexeme);
   }
   else if (lexeme == '-' || lexeme == '+' || lexeme == '*' || lexeme == '=' || lexeme == '!' || lexeme == '<' || lexeme == '>')
   {
      scanner.ChangeStateTo(new ScannerStateOperation());
      scanner.PushToBuffer(lexeme);
   }
   else if (lexeme == '/') 
   {
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateCommentOrOperation());
   }
   else if (lexeme == ';' || lexeme == ',') 
   {
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenSeparator);
   }
   else if (lexeme == '{' || lexeme == '}' || lexeme == '(' || lexeme == ')') 
   {
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenBracket);
   }
   else if (isspace(lexeme)) 
   {
      scanner.ClearBuffer();
   }
   else
   {
      scanner.PushError("Встречен недопустимый символ");
   }

}

void ScannerStateWord::update(Scanner &scanner, char lexeme) 
{
   if (isalpha(lexeme) || isdigit(lexeme) || lexeme == '_') 
   {
      scanner.PushToBuffer(lexeme);
   }
   else if (isspace(lexeme)) 
   {
      scanner.CreateToken(TokenWord);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '-' || lexeme == '+' || lexeme == '*' || lexeme == '=' || lexeme == '!' || lexeme == '<' || lexeme == '>') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateOperation());
   }
   else if (lexeme == '/') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateCommentOrOperation());
   }
   else if (lexeme == ';' || lexeme == ',') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenSeparator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '{' || lexeme == '}' || lexeme == '(' || lexeme == ')') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenBracket);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
}

void ScannerStateNumber::update(Scanner &scanner, char lexeme) 
{
   if (isdigit(lexeme)) 
   {
      scanner.PushToBuffer(lexeme);
   }
   else if (isspace(lexeme)) 
   {
      scanner.CreateToken(TokenConstant);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '-' || lexeme == '+' || lexeme == '*' || lexeme == '=' || lexeme == '!' || lexeme == '<' || lexeme == '>')
   {
      scanner.CreateToken(TokenConstant);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateOperation());
   }
   else if (lexeme == '/') 
   {
      scanner.CreateToken(TokenConstant);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateCommentOrOperation());
   }
   else if (lexeme == ';' || lexeme == ',') 
   {
      scanner.CreateToken(TokenConstant);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenSeparator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '{' || lexeme == '}' || lexeme == '(' || lexeme == ')') 
   {
      scanner.CreateToken(TokenConstant);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenBracket);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (isalpha(lexeme)) 
   {
      scanner.PushError("Название переменной не может начинаться с цифры.");
      scanner.CreateToken(TokenConstant);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
}

void ScannerStateOperation::update(Scanner &scanner, char lexeme) 
{
   if (lexeme == '/' || lexeme == '-' || lexeme == '+' || lexeme == '*' || lexeme == '=' || lexeme == '!' || lexeme == '<' || lexeme == '>')
   {
      scanner.PushToBuffer(lexeme);
   }
   else if (isspace(lexeme)) 
   {
      scanner.CreateToken(TokenOperator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (isdigit(lexeme)) 
   {
      scanner.CreateToken(TokenOperator);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateNumber());
   }
   else if (isalpha(lexeme))
   {
      scanner.CreateToken(TokenOperator);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateWord());
   }
   else if (lexeme == ';' || lexeme == ',')
   {
      scanner.CreateToken(TokenOperator);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenSeparator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '{' || lexeme == '}' || lexeme == '(' || lexeme == ')') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenBracket);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
}

void ScannerStateCommentOrOperation::update(Scanner &scanner, char lexeme) 
{
   if (lexeme == '=')
   {
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateOperation());
   }
   else if (isspace(lexeme)) 
   {
      scanner.CreateToken(TokenOperator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (isdigit(lexeme)) 
   {
      scanner.CreateToken(TokenOperator);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateNumber());
   }
   else if (isalpha(lexeme))
   {
      scanner.CreateToken(TokenOperator);
      scanner.PushToBuffer(lexeme);
      scanner.ChangeStateTo(new ScannerStateWord());
   }
   else if (lexeme == '/')
   {
      scanner.ClearBuffer();
      scanner.ChangeStateTo(new ScannerStateSinglelineComment());
   }
   else if (lexeme == '*')
   {
      scanner.multilineComment = true;
      scanner.ClearBuffer();
      scanner.ChangeStateTo(new ScannerStateMultilineComment());
   }
   else if (lexeme == ';' || lexeme == ',') 
   {
      scanner.CreateToken(TokenConstant);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenSeparator);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else if (lexeme == '{' || lexeme == '}' || lexeme == '(' || lexeme == ')') 
   {
      scanner.CreateToken(TokenWord);
      scanner.PushToBuffer(lexeme);
      scanner.CreateToken(TokenBracket);
      scanner.ChangeStateTo(new ScannerStateStart());
   }
}

void ScannerStateSinglelineComment::update(Scanner &scanner, char lexeme) 
{
   if (lexeme == '\n') 
   {
      scanner.ChangeStateTo(new ScannerStateStart());
   }
}

void ScannerStateMultilineComment::update(Scanner &scanner, char lexeme) 
{
   if (lexeme == '*') 
   {
      scanner.ChangeStateTo(new ScannerStateFinishMultilineComment());
   }
}

void ScannerStateFinishMultilineComment::update(Scanner &scanner, char lexeme) 
{
   if (lexeme == '/') 
   {
      scanner.multilineComment = false;
      scanner.ChangeStateTo(new ScannerStateStart());
   }
   else
   {
      scanner.ChangeStateTo(new ScannerStateMultilineComment());
   }
}