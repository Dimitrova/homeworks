#include "readlines.h"
#include <stdio.h>
#include <unistd.h>

const size_t BUF_SIZE = 8000;

int main(int num_of_args, char** argv)
{
   if (num_of_args != 2)
   {
      int pid = 0;
      int status;
      int fds[2];
      pipe(fds);
      if (pid = fork()) 
      {
         // parent
         dup2(fds[1], 1);
         close(fds[0]);
         close(fds[1]);
	 execl ("/bin/write", "need argument for max_size\n" , NULL);
      }
 
   }
   int max_size;
   sscanf(argv[1], "%i", &max_size);
   struct RL* rl = rl_open(0, max_size);
   char* buf;
   int curr = rl_readline(rl, buf, BUF_SIZE); 
   while (curr != 0)
   {
      if (curr >= 0)
      {
         int pid = 0;
      	 int status;
      	 int fds[2];
      	 pipe(fds);
      	 if (pid = fork()) 
	 {
            // parent
            dup2(fds[1], 1);
            close(fds[0]);
            close(fds[1]);
            execl ("/bin/write", buf , NULL);
      	 }
      }
      if (rl_close(rl) != 0)
      {
         break;
      }
      rl = rl_open(0, max_size);
      curr = rl_readline(rl, buf, BUF_SIZE);	 
   }
   return 0;
}
