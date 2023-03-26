#pragma once
#include <vector>
#include <iostream>

#include "Variable.h"

using namespace std;

class DynamicTable
{
public:
   vector<Variable> Table;
   DynamicTable() { Table = {}; };

   // Возвращает номер найденного в таблице элемента,
   // в случае неудачи возвращает -1. 
   int LinearSearch(string Name);
};

