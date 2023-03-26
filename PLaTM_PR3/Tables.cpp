#include "Tables.h"

Tables::Tables()
{
   Keywords   = new StaticTable("Keywords.txt"  );
   Operators  = new StaticTable("Operators.txt" );
   Separators = new StaticTable("Separators.txt");
   Brackets   = new StaticTable("Brackets.txt"  );
   Specials   = new StaticTable("Specials.txt"  );

   Constants = new DynamicTable();
   Variables = new DynamicTable();
}

Tables::Tables
(
   string keywords,
   string operators,
   string separators,
   string brackets,
   string specials
)
{
   try
   {
      Keywords   = new StaticTable(keywords  );
      Operators  = new StaticTable(operators );
      Separators = new StaticTable(separators);
      Brackets   = new StaticTable(brackets  );
      Specials   = new StaticTable(specials  );

      Constants = new DynamicTable();
      Variables = new DynamicTable();
   }
   catch (exception ex)
   {
      throw ex.what();
      exit(EXIT_FAILURE);
   }
}

pair<int, int> Tables::SearchOnStatic(string Word)
{
   int inTableNum = 0;

   inTableNum = Keywords->BinarySearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { StaticKeywords,  inTableNum };

   inTableNum = Operators->BinarySearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { StaticOperators,  inTableNum };

   inTableNum = Separators->BinarySearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { StaticSeparators,  inTableNum };

   inTableNum = Brackets->BinarySearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { StaticBrackets,  inTableNum };

   inTableNum = Specials->BinarySearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { StaticSpecials,  inTableNum };

   return FAILURE_PAIR;
}

//  ŒÀ’Œ« ¿¿¿¿¿¿¿¿¿¿¿¿¿ Î‡·‡ 3
string Tables::GetStaticLex(pair<int, int> Address)
{
   int inTableNum = Address.second;

   switch (Address.first)
   {

   case StaticKeywords:
      return Keywords->GetName(inTableNum);
      break;

   case StaticOperators:
      return Operators->GetName(inTableNum);

   case StaticSeparators:
      return Separators->GetName(inTableNum);

   case StaticBrackets:
      return Brackets->GetName(inTableNum);

   case StaticSpecials:
      return Specials->GetName(inTableNum);

   default:
      break;
   }

   return "";
}

pair<int, int> Tables::SearchOnDynamic(string Word)
{
   int inTableNum = 0;
   inTableNum = Variables->LinearSearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { DynamicVariables,  inTableNum };

   inTableNum = Constants->LinearSearch(Word);
   if (inTableNum != FAILURE_SIGNAL)
      return { DynamicConstants,  inTableNum };

   return FAILURE_PAIR;
}

Variable *Tables::SearchOnDynamic(pair<int, int> Address)
{
   return Address.first == DynamicVariables ? 
      &Variables->Table[Address.second] :
      &Constants->Table[Address.second];
}

pair<int, int> Tables::IncludeInDynamic(string Name, VariableType Type, bool IsInitialised)
{
   pair<int, int> Address = SearchOnDynamic(Name);
   if (Address != FAILURE_PAIR)
      return Address;

   Variable newVar = { Name, Type, IsInitialised };
   if (Variables->LinearSearch(Name) == FAILURE_SIGNAL)
   {
      Variables->Table.push_back(newVar);
      return { DynamicVariables, Variables->Table.size() - 1 };
   }
}

pair<int, int> Tables::IncludeInDynamic(string Value, VariableType Type)
{
   pair<int, int> IsExists = SearchOnDynamic(Value);
   if (IsExists != FAILURE_PAIR)
      return IsExists;

   Variable newVar = { Value, Type };
   if (Constants->LinearSearch(Value) == FAILURE_SIGNAL)
   {
      Constants->Table.push_back(newVar);
      return { DynamicConstants, Constants->Table.size() - 1 };
   }
}

pair<int, int> Tables::SetName(pair<int, int> Address, string Word)
{
   Address.first == DynamicVariables ? Variables->Table[Address.second].Name = Word : Constants->Table[Address.second].Name = Word;
   return Address;
}

pair<int, int> Tables::SetType(pair<int, int> Address, VariableType Type)
{
   Address.first == DynamicVariables ? Variables->Table[Address.second].Type = Type : Constants->Table[Address.second].Type = Type;
   return Address;
}
