#pragma once
#include "StaticTable.h"
#include "DynamicTable.h"
#include "Variable.h"

const pair<int, int> FAILURE_PAIR = { -1,-1 };
const int FAILURE_SIGNAL = -1;

enum TableType
{
   StaticKeywords,
   StaticOperators,
   StaticSeparators,
   StaticBrackets,
   StaticSpecials,
   DynamicVariables,
   DynamicConstants,

   DynamicLogic = 8,
   DynamicMark = 10,
   DynamicMarkGo = 11
};

class Tables
{
   StaticTable *Keywords;
   StaticTable *Operators;
   StaticTable *Separators;
   StaticTable *Brackets;
   StaticTable *Specials;
   
   DynamicTable *Variables;
   DynamicTable *Constants;

public:

   Tables();
   Tables(string, string, string, string, string);

   // ¬озвращает пару: номер таблицы и
   // номер найденного в таблице элемента,
   // в случае неудачи возвращает пару -1. 
   pair<int, int> SearchOnStatic(string Name);
   string GetStaticLex(pair<int, int> Address);
   pair<int, int> SearchOnDynamic(string Name);

   // ѕоиск лексемы по адресу, возвращает указатель
   // на структуру, содержащую атрибуты.
   Variable *SearchOnDynamic(pair<int, int> Address);

   // ƒобавление в таблицы идентификаторов и констант.
   pair<int, int> IncludeInDynamic(string Name, VariableType Type, bool IsInitialised);
   pair<int, int> IncludeInDynamic(string Value, VariableType Type);

   // »зменение/добавление атрибутов лексемы по адресу.
   pair<int, int> SetName(pair<int, int> Address, string Word);
   pair<int, int> SetType(pair<int, int> Address, VariableType Type);
};