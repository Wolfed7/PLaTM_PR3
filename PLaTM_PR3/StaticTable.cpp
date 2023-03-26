#include "StaticTable.h"
#include "Tables.h"

#include <vector>
#include <iostream>
#include <fstream>
#include <algorithm>

StaticTable::StaticTable(string FilePath)
{
   ifstream fin(FilePath);

   while (!fin.eof())
   {
      string Word;
      fin >> Word;
      Table.push_back(Word);
   }

   sort(Table.begin(), Table.end());
}

int StaticTable::BinarySearch(string Word)
{
   int begin = 0;
   int end = Table.size() - 1;

   while (begin <= end) 
   {
      int mid = (begin + end) / 2;
      string midVal = Table[mid];

      if (midVal < Word)
         begin = mid + 1;
      else if (midVal > Word)
         end = mid - 1;
      else
         return mid;
   }

   return FAILURE_SIGNAL;
}

string StaticTable::GetName(int addr)
{
   return Table[addr];
}

size_t StaticTable::Lenth()
{
   return Table.size();
}