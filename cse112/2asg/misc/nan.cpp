// $Id: nan.cpp,v 1.6 2020-11-04 15:26:06-08 - - $

#include <iomanip>
#include <iostream>
using namespace std;

#define SHOW(X) cout << #X << " = " << (X) << endl;

int main() {
   const double nan = 0.0 / 0.0;
   cout << boolalpha;
   SHOW (nan);
   SHOW (nan == nan);
   SHOW (nan < nan);
   SHOW (nan > nan);
   SHOW (nan == 0.0);
   SHOW (nan < 0.0);
   SHOW (nan > 0.0);
}
