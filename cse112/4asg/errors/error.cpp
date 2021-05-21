// $Id: error.cpp,v 1.1 2021-05-06 16:39:52-07 - - $
#include <string>
int main (int argc, char**argv) {
   return argc == 1 ? 0 : std::stoi (argv[1]);
}
