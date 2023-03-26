#pragma once
#include <iostream>

using namespace std;

enum VariableType
{
   None,
   Void,
   Int,
};

struct Variable
{
   string Name;
   int Type;
   bool IsInitialised;
};