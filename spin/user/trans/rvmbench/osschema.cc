/* _Os_ApP_iD_StR 849139414 */
#include <ostore/ostore.hh>
#include <ostore/nreloc/schftyps.hh>
#include <ostore/os_types/schminit.hh>
#include <ostore/hdrstart.hh>
/*
 */
/*
 */
static os_unsigned_int8
rep_desc_type_387[] = {
  2,                            
  0, 0, 0, 8,
  3,
  3,
  29, 120, 1,
  1,
  34,
  62,
  98, 114, 97, 110, 99, 104, 0
};

static os_unsigned_int8
rep_desc_type_388[] = {
  2,                            
  0, 0, 0, 9,
  3,
  3,
  3,
  29, 116, 1,
  1,
  34,
  62,
  116, 101, 108, 108, 101, 114, 0
};

static os_unsigned_int8
rep_desc_type_389[] = {
  2,                            
  0, 0, 0, 12,
  3,
  3,
  3,
  3,
  3,
  3,
  29, 40, 1,
  1,
  34,
  62,
  104, 105, 115, 116, 111, 114, 121, 0
};

static os_unsigned_int8
rep_desc_type_390[] = {
  2,                            
  0, 0, 0, 25,
  3,
  29, 1, 128,
  43, 1, 131,
  34,
  29, 10, 128,
  43, 1, 132,
  34,
  30, 128, 0, 0, 64,
  43, 1, 133,
  34,
  62,
  114, 111, 111, 116, 95, 115, 116, 114, 117, 99, 116, 0
};

static os_unsigned_int8*
rep_desc_type_table[]={
  rep_desc_type_387,
  rep_desc_type_388,
  rep_desc_type_389,
  rep_desc_type_390
};

static os_unsigned_int32
rep_desc_vtbl_entry_offset[]={
  0, 0
};

/* Vtbl addresses */
 _Reloc_vtbl_pointer
OS_RDFF_VTBL_ADDRESSES[]={
  0,
  0,
  0
};

 char* OS_RDFF_VTBL_NAMES[]={
  0, 0
};

static os_unsigned_int32
rep_desc_discriminant_entry_offset[]={
  0, 0
};

/* Discriminant addresses */
 _Reloc_discriminant_pointer
OS_RDFF_DISCRIMINANT_ADDRESSES[]={
  0,  0, 0};

 char* OS_RDFF_DISCRIMINANT_NAMES[]={
  0, 0
};

static os_unsigned_int16
rep_desc_tag[]={
  0,                            
  0                             
};

struct _Pers_var_type_spec {
   char *typespec;
   void *last_db;
   os_unsigned_int32 last_tag;
};
class _Pvts;


static struct { char marker[16]; char asdb[256]; } aci = { "   OS Patch ASP", "/local/yasushi/rvmbench/rvmbench.adb"} ;
static _Application_schema_info __Application_schema_info0 =
{ 849139414, 16, aci.asdb} ;
static _OS_schema_init_info application_schema_info = {
  /* link_resolved	                      */ 0,
  /* format_version                      */ 4,
  /* number_of_types                     */ 0,
  /* first_rd_number                     */ 387,
  /* last_rd_number                      */ 390,
  /* first_tag_number                    */ 0,
  /* last_tag_number                     */ 0,
  /* rep_desc_tags                       */ rep_desc_tag,
  /* types                               */ rep_desc_type_table,
  /* vtbl_entry_offsets                  */ rep_desc_vtbl_entry_offset,
  /* vtbl_names                          */ (const char*const*)OS_RDFF_VTBL_NAMES,
  /* vtbl_addresses                      */ (void**)OS_RDFF_VTBL_ADDRESSES,
  /* vtbl_offsets                        */ 0,
  /* discriminant_entry_offsets          */ rep_desc_discriminant_entry_offset,
  /* discriminant_names                  */ (const char*const*)OS_RDFF_DISCRIMINANT_NAMES,
  /* discriminant_addresses              */ (void**)OS_RDFF_DISCRIMINANT_ADDRESSES,
  /* vbtbl_entry_offsets                 */ 0,
  /* vbtbl_names                         */ 0,
  /* vbtbl_addresses                     */ (void**)0,
  /* neutralized_compilers               */ 16,
  /* app_schema_info                     */ &__Application_schema_info0,
  /* next_init_info                     */ 0,
  /* n_vtbl_addrs                       */ sizeof(OS_RDFF_VTBL_ADDRESSES)/sizeof(_Reloc_vtbl_pointer),
  /* n_discr_addrs                      */ sizeof(OS_RDFF_DISCRIMINANT_ADDRESSES)/sizeof(_Reloc_discriminant_pointer),
  /* n_vbtbl_addrs                      */ 0
};
static _OS_dynamic_schema_init_info dinfo(application_schema_info);

extern os_boolean
OS_RDFF_LINK_RESOLVED_MAPS = 0;

