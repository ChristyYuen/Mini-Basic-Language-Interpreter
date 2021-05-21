// $Id: segfault.cpp,v 1.1 2021-05-06 16:39:52-07 - - $
#include <cstdint>
int main() {
   uintptr_t u = 4;
   int* p = reinterpret_cast<int*> (u);
   *p = 3;
}
