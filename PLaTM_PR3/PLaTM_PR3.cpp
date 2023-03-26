#include <iostream>
#include <fstream>
#include <sstream>

#include "Tables.h"
#include "Scanner.h"
#include "parser.h"

using namespace std;

const string Keywords = "Keywords.txt";
const string Operators = "Operators.txt";
const string Separators = "Separators.txt";
const string Brackets = "Brackets.txt";
const string Specials = "Specials.txt";

const string testFile = "test.cpp";

int main()
{
   setlocale(LC_ALL, "");

   // ******************************** //
   // Это всё таблицы и работа с ними. //
   // ******************************** //
   Tables tables
   (
      Keywords,
      Operators,
      Separators,
      Brackets,
      Specials
   );

   //// Проверка поиска в постоянных таблицах.
   //tables.SearchOnStatic("int");
   //// Добавление в переменные таблицы.
   //pair<int, int> a = tables.IncludeInDynamic("counter", Int, false);
   //pair<int, int> b = tables.IncludeInDynamic("counter", Int, false);
   //tables.IncludeInDynamic("132", Int);
   //tables.IncludeInDynamic("siztable", Int, false);
   //tables.IncludeInDynamic("IsProgram", Int, false);
   //tables.IncludeInDynamic("table213", Int, false);
   //tables.IncludeInDynamic("421", Int);
   //// Возвращение адреса идентификатора.
   //pair<int, int> SearchResult = tables.SearchOnDynamic("siztable");
   //// Возвращение структуры, содержащей атрибуты.
   //Variable *identifier = tables.SearchOnDynamic(SearchResult);
   //// Изменение атрибута Type.
   //tables.SetType(SearchResult, Void);
   //// Атрибут изменился.
   //identifier = tables.SearchOnDynamic(SearchResult);

   // ******************************** //
   //  Это всё сканер и работа с ним.  //
   // ******************************** //

   string content;
   ifstream file(testFile);
   if (file) 
   {
      ostringstream ss;
      ss << file.rdbuf();
      content = ss.str();
   }
   file.close();

   auto scanner = Scanner(tables);
   scanner.StartScan(content);

   auto s_errors = scanner.GetErrors();
   auto tokens = scanner.GetTokens();
   auto tokenLineIndexes = scanner.GetTokenLineIndexes();

   // Вывод токенов.
   ofstream fout("tokens.txt");
   for (size_t i = 0; i < tokenLineIndexes.size() - 1; i++)
   {
      for (size_t j = tokenLineIndexes[i]; j < tokenLineIndexes[i + 1]; j++)
         fout << tokens[j].ToStr() << " ";
      fout << "\n";
   }

   for (auto &err : s_errors) 
      cout << err.line << "\t" << err.msg << "\n";

   if (s_errors.size() > 0)
      return 1;


   // ******************************** //
   //     Синтаксичекий анализатор.    //
   // ******************************** //

   auto table = ParseTable("ParseTable.txt");
   auto parser = Parser(&table, tables, tokenLineIndexes);
   parser.Parse(tokens);

   auto p_errors = parser.GetErrors();
   auto polish = parser.GetPolish();

   for (auto &err : p_errors)
      cout << err.line << "|\t" << err.msg << '\n';

   if (p_errors.size() > 0)
      return 1;

   //for (auto token : polish)
   //   cout << parser.getNiceTokenStr(token) << " ";

   return 0;
}