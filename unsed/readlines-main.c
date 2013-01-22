#include "readlines.h"
#include <stdio.h>
#include <unistd.h>
#include <stddef.h>
#include <stdlib.h>

const size_t BUF_SIZE = 8000;

int main(int argc, char* argv[])
{
   if (argc != 2)
   {
      printf("Need more arguments");
      return 0;
   }

   int max_size;
   sscanf(argv[1], "%i", &max_size);
   struct RL* rl = rl_open(0, max_size);
   char buf[BUF_SIZE];
   int curr = rl_readline(rl, buf, BUF_SIZE); 
   while (curr != 0)
   {
      if (curr == -1)
      {
         printf("Oups, something wrong");
         return 0;
      }

      if (curr > 0)
      {
      	 size_t i = 0;
 
         while (i < curr) 
         { 
            i += write(1, &buf[i], curr - i);
         }
      }	 
      curr = rl_readline(rl, buf, BUF_SIZE); 
   }

   rl_close(rl);
   return 0;
}
