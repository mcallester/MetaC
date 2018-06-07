
#include "mc.h"
#include <stdint.h>

uint64_t next_uf_node_id; // 8 bytes for undo_set

typedef struct uf_struct{
  void * identity;
  int node_id;
  uint64_t count; /* 8 bytes for undo set */
  struct uf_struct *next;
} *uf_ptr;


uf_ptr uf_make(void *identity);

uf_ptr uf_find(uf_ptr e);

uf_ptr uf_equate(uf_ptr e1, uf_ptr e2);



void uf_init();
