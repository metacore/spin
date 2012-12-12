#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "bench.h"

root_struct *_root;
os_database *db;
os_typespec *root_type;

void 
usage ()
{
  fprintf(stderr, "rvmbench [-i]\n-i : initialize the database.\n");
}


void 
initialize ()
{
  db = os_database::create("rvmbench_data");

  printf("creating the database.\n");
  OS_BEGIN_TXN(tx1, 0, os_transaction::update)
    _root = new(db, root_type) root_struct;
    db->create_root("tpca_root")->set_value(_root, root_type);
  OS_END_TXN(tx1)
}

int 
main (int argc, char *argv[])
{
  int c;
  objectstore::initialize();
  OS_ESTABLISH_FAULT_HANDLER

  root_type = new os_typespec("root_struct");

  if (argc > 1 && strcmp(argv[0], "-i")) {
    initialize();
    exit(0);
  }
  printf("starting the benchmark.\n");

  db = os_database::open("rvmbench_data");

  RunTrial();

  OS_END_FAULT_HANDLER

  exit(0);
}
