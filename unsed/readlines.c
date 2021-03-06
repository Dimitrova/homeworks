#include "readlines.h"
#include <stddef.h>
#include <stdlib.h>
#include <errno.h>

const size_t BUFF_SIZE = 8000;

struct RL* rl_open(int fd, size_t max_size)
{
   int size = sizeof(char);
   size = size * BUFF_SIZE;
   char* buff = (char*) malloc(size);
   if (buff == NULL)
   {
      return NULL;
   }

   struct RL* result = (struct RL*) malloc(sizeof(struct RL));
   if (result == NULL)
   {
      return NULL;
   }

   (*result).fd = fd;
   (*result).max_size = max_size;
   (*result).buffer = buff;
   (*result).need_to_skip = 0;
   return result;
}

size_t rl_max_size(struct RL *rl)
{
   return (*rl).max_size;
}

int rl_close(struct RL *rl)
{
   int fd = (*rl).fd;
   if (close(fd) != 0)
   {
      return errno;
   }
   free((*rl).buffer);
   free(rl);
   return 0;
}

int rl_readline(struct RL* rl, char* buf, size_t buf_size)
{
   int j;
   int i = 0;
   int k = read((*rl).fd, (*rl).buffer + i, (*rl).max_size + 1);
   if (k > buf_size)
   {
      return -2;
   }
   while ((*rl).buffer[i] != '\n' && i < k) 
   {
      i++;
   }
   if ((*rl).buffer[i] == '\n')
   {
      if ((*rl).need_to_skip == 1)
      {
         (*rl).need_to_skip = 0;
         return -3;
      }
      for (j = 0; j < k; j++)
      {
        buf[j] = (*rl).buffer[j];
      }
      return k;
   }
   if (i == (*rl).max_size + 1)
   {
      (*rl).need_to_skip = 1;
      return -3;
   }
   if (k == 0) 
   {
      return 0;
   }
   return -1;
}
