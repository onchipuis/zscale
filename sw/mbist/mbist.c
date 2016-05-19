// See LICENSE for license details.

#include "zscale.h"
#include "encoding.h"

int main()
{

  int i;
  for (i = 0;i < 1024;i++) {
    write_csr(0x780, (i << 1) | 1);
  }
  return 0;
}
