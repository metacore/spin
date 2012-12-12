/*
 * OS specific service interface for M3 support
 */


typedef struct {
  LONGEST ref;   /* the Thread.T value */
  int     id;    /* the thread's internal ID */
  char   *bits;  /* the pointer to the Thread.T's data fields */
} M3_THREAD;


struct m3_os_ops {
    char	*os_name;
    int		os_new_typecode_format;
    void	(*os_init_thread_constants) PARAMS((void));
    void	(*os_threads) PARAMS((char*, int));
    void	(*os_get_m3_thread) PARAMS((CORE_ADDR, M3_THREAD*));
    void	(*os_first_m3_thread) PARAMS((M3_THREAD*));
    void	(*os_next_m3_thread) PARAMS((M3_THREAD*));
    int		(*os_reg_offset) PARAMS((int));
};




struct m3_os_ops *init_target_os_m3_ops_spin();
struct m3_os_ops *init_target_os_m3_ops_posix();


#define eval(x) evaluate_expression (parse_expression (x))
#define print(x) value_print (x, stdout, 0, Val_pretty_default)
#define printx(y) value_print (y, stdout, 'x', Val_pretty_default)



    
    
    
