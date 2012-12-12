#include <stdio.h>
#include <sys/time.h>
#include <mach.h>
#include <rvm.h>
#include <assert.h>
#include <unistd.h>
#include "bench.h"


char *_Rvm_Data_Device = "data";
char *_Rvm_Log_Device = "log";
rvm_offset_t _Rvm_DataLength;
int _Rvm_Truncate;
rvm_length_t _Rvm_Optimization_Level;
int _status;

void initialize_server() {
    rvm_return_t err;
    rvm_options_t *options;

    options = rvm_malloc_options();
    options->log_dev = _Rvm_Log_Device;
    options->flags |= _Rvm_Optimization_Level;
    if (_Rvm_Truncate > 0 && _Rvm_Truncate < 100) {
	printf("Setting Rvm Truncate threshhold to %d.\n", _Rvm_Truncate);
	options->truncate = _Rvm_Truncate;
    }

    if ((err = RVM_INIT(options)) != RVM_SUCCESS)	/* Start rvm */	    
	printf("rvm_init failed %s", rvm_return(err));
    assert(err == RVM_SUCCESS);			  		    
    assert(_Rvm_Data_Device != NULL);	   /* Load in recoverable mem */ 
    _Rvm_DataLength = RVM_MK_OFFSET(0, 80*1024*1024);
    rds_load_heap(_Rvm_Data_Device,_Rvm_DataLength,
		  (char **)&_root, (int *)&err); 
    if (err != RVM_SUCCESS) {
	printf("rds_load_heap error %s",rvm_return(err));
	exit(-1);
    }
    rvm_free_options(options);

    /* set the thread data */
    rvm_data.tid = NULL;
}

static rvm_return_t _status;
static rvm_perthread_t *_rvm_data;
static rvm_tid_t tid;

void begin_transaction(int restore_mode)
{
    /* Initialize the rvm_perthread_t object. */
    _rvm_data = RVM_THREAD_DATA;
    if (_rvm_data == 0) fatal("BeginTransaction: _rvm_data = 0");
    if (_rvm_data->tid != 0) { 
	fatal("_rvm_data->tid is non zero during begin transaction");
    }
    rvm_init_tid(&tid);
    _rvm_data->tid = &tid;
    /* Begin the transaction. */
    _status = rvm_begin_transaction(_rvm_data->tid, (restore_mode));
    if (_status == RVM_SUCCESS)
      _status = (rvm_return_t)_setjmp(_rvm_data->abort);
    else 
      fatal("begin_transaction");
}

void 
end_transaction(int flush_mode, int *statusp)
{
    /* End the transaction. */
    if (_status == RVM_SUCCESS) {
	_status = rvm_end_transaction(_rvm_data->tid, flush_mode);
    }
    if (statusp)
      *(statusp) = _status;
    
    /* De-initialize the rvm_perthread_t object. */
    _rvm_data->tid = 0;
}

extern int NTRANS;
extern int NACCOUNTS;

main(int argc, char **argv)
{
    int c;
    while ((c = getopt(argc, argv, "n:l:d:a:")) != EOF) {
	switch (c) {
	  case 'n':
	    NTRANS = atoi(optarg);
	    break;
	  case 'a':
	    NACCOUNTS = atoi(optarg);
	    break;
	  case 'l':
	    _Rvm_Log_Device = optarg;
	    break;
	  case 'd':
	    _Rvm_Data_Device = optarg;
	    break;
	  default:
	    printf("%s [-l LOGDEVICE] [-d DATADEVICE]\n", argv[0]);
	}
    }
    initialize_server();
    RunTrial();
}

