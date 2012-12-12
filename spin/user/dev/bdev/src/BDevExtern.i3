UNSAFE INTERFACE BDevExtern;

TYPE dev_t = INTEGER;

<*EXTERNAL*> PROCEDURE blkdev_open(id: dev_t) : INTEGER;
<*EXTERNAL*> PROCEDURE blkdev_close(id: dev_t) : INTEGER;
<*EXTERNAL*> PROCEDURE blkdev_read(id: dev_t; 
                                   VAR data: ARRAY OF CHAR;
                                   offset: CARDINAL; 
		                   VAR bytes: CARDINAL) : INTEGER;
<*EXTERNAL*> PROCEDURE blkdev_write(id: dev_t;
                                    READONLY data: ARRAY OF CHAR;
                                    offset: CARDINAL;
                                    VAR bytes: CARDINAL) : INTEGER;
<*EXTERNAL*> PROCEDURE blkdev_register(register_dev: PROCANY);


END BDevExtern.
