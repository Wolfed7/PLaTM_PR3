#include "DynamicTable.h"
#include "Tables.h"

int DynamicTable::LinearSearch(string Name)
{
   for (size_t i = Table.size(); i > 0 ; i--)
      if (Table[i - 1].Name == Name)
         return i - 1;

   return FAILURE_SIGNAL;
}
