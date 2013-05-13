#if HAVE_CONFIG_H
#include "config.h"
#endif

#include "error.h"

void *
xmalloc (size_t num)
{
  void *new = malloc (num);
  if (!new)
    sic_fatal ("Memory exhausted");
  return new;
}

void *
xrealloc (void *p, size_t num)
{
  void *new;

  if (!p)
    return xmalloc (num);

  new = realloc (p, num);
  if (!new)
    sic_fatal ("Memory exhausted");

  return new;
}

void *
xcalloc (size_t num, size_t size)
{
  void *new = xmalloc (num * size);
  bzero (new, num * size);
  return new;
}

