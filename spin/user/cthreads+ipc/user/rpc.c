char  RPC_io_code[] = {
	0
};
char  RPC_mo_code[] = {
	0
};
InitCode() {
	  void *d1, *d2, *maind;
	  extern void *download();
    d1 = download( RPC_io_code, sizeof( RPC_io_code));
    d2 = download( RPC_mo_code, sizeof( RPC_io_code));
    drop(d1, d2);
}
