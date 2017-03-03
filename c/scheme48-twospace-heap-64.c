#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "prescheme.h"
#include "scheme48vm.h"
#include "scheme48heap.h"

struct image_location {
  long new_descriptor;
  long next;
};
struct table {
  long *keys;
  struct image_location **values;
  long count;
  long size;
};
static struct image_location *table_ref(struct table*, long);
static long copy_weak_pointer(long, char *, char **);
static void table_setB(struct table*, long, struct image_location*);
static long real_copy_object(long, char *, char **);
static char resumer_recordP(long);
static long trace_image_value(long);
void s48_write_barrier(long, char *, long);
void s48_forbid_gcB(void);
void s48_allow_gcB(void);
char s48_gc_can_allocate_unmovableP(void);
char * s48_allocate_traced_unmovableAgc(long);
char * s48_allocate_untraced_unmovableAgc(long);
char s48_unmovableP(long);
long s48_allocate_unmovable_stob(long, long);
long s48_heap_size(void);
long s48_gc_count(void);
long s48_gc_run_time(long*);
void s48_initialization_completeB(void);
long s48_max_heap_size(void);
long s48_startup_procedure(void);
long s48_initial_symbols(void);
long s48_initial_imported_bindings(void);
long s48_initial_exported_bindings(void);
long s48_resumer_records(void);
void s48_set_image_valuesB(long, long, long, long, long);
char * s48_allocate_small(long);
char s48_stob_in_heapP(long);
long s48_available(void);
char s48_extantP(long);
long s48_gather_objects(char(*)(long), char(*)(char(*)(long)));
static char pD1(long);
char s48_check_heap(long);
long s48_find_all(long);
long s48_trace_value(long);
long s48_find_all_records(long);
void s48_initializing_gc_root(void);
long s48_write_image(long, long, FILE *);
void s48_collect(char);
void s48_make_availableAgc(long);
char * s48_allocate_tracedAgc(long);
char * s48_allocate_weakAgc(long);
char * s48_allocate_untracedAgc(long);
long s48_allocate_stob(long, long);
long s48_allocate_weak_stob(long, long);
long s48_read_image(char*, long);
void s48_trace_locationsB(char *, char *);
void s48_trace_stob_contentsB(long);
void s48_trace_continuation_contentsB(char *, char *, long);
static char * Soldspace_hpS;
static char * Soldspace_limitS;
static char * Snewspace_beginS;
static char * Snewspace_endS;
static char * Soldspace_beginS;
static char * Soldspace_endS;
static char * Snew_heap_start_addrS;
static char * *Spure_areasS;
static char * *Simpure_areasS;
static long *Spure_sizesS;
static long *Simpure_sizesS;
static long Spure_area_countS;
static long Simpure_area_countS;
static char (*Scollect_predicateS)(long);
static long Sfinding_typeS;
static long Sheap_errors_leftS;
static long Sgc_countS;
static long Sgc_secondsS;
static long Sgc_msecondsS;
static char * Sfrom_beginS;
static char * Sfrom_endS;
static char * Sweak_pointer_hpS;
static char * Sweak_pointer_limitS;
static long SstatusS;
static char SeofPS;
static long Sstartup_procedureS;
static long SsymbolsS;
static long Simported_bindingsS;
static long Sexported_bindingsS;
static long Sresumer_recordsS;
static char * Simg_start_addrS;
static char * Simg_end_addrS;
static long Simg_heap_sizeS;
static char * Ssmall_img_start_addrS;
static char * Ssmall_img_hp_addrS;
static char * Ssmall_img_end_addrS;
static long Ssmall_img_heap_sizeS;
static char * Slarge_img_start_addrS;
static char * Slarge_img_hp_addrS;
static char * Slarge_img_end_addrS;
static long Slarge_img_heap_sizeS;
static char * Sweaks_img_start_addrS;
static char * Sweaks_img_hp_addrS;
static char * Sweaks_img_end_addrS;
static long Sweaks_img_heap_sizeS;
static char * Sheap_image_pointerS;
static char * Ssymbol_addressS;
static long Sheap_object_remaining_cellsS;
static char * Sheap_object_pointerS;
static struct table *Sstob_tableS;
static long SoffsetS;
static char * Sarea_startS;
static char SinitializingPS;
static long SstatusS;
static FILE * Simage_portS;
static char * Simage_bufferS;
static char * Simage_buffer_pointerS;
static long image_start_address;
static char * Simage_beginS;
static char * Simage_hpS;
static struct table *Sstob_tableS;
static long Sfirst_stobS;
static struct image_location *Slast_stobS;
static long Sresumer_countS;
static long Sresumer_recordsS;
static long Sundumpable_recordsS;
static long Sundumpable_countS;
static long Hthe_record_type270;
char * s48_ShpS;
char * s48_SlimitS;

static struct image_location *table_ref(struct table *table_1X, long key_2X)
{
  long arg0K0;
  long next_6X;
  long i_5X;
  long *keys_4X;
  long size_3X;
 {  if ((0 < (table_1X->size))) {
    size_3X = table_1X->size;
    keys_4X = table_1X->keys;
    arg0K0 = ((key_2X ^ ((PS_SHIFT_LEFT_INLINE(key_2X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_2X, 10)))) & (-1 + size_3X));
    goto L3037;}
  else {
    return (NULL);}}
 L3037: {
  i_5X = arg0K0;
  next_6X = *(keys_4X + i_5X);
  if ((key_2X == next_6X)) {
    return (*((table_1X->values) + i_5X));}
  else {
    if ((0 == next_6X)) {
      if ((i_5X == (table_1X->size))) {
        arg0K0 = 0;
        goto L3037;}
      else {
        return (NULL);}}
    else {
      arg0K0 = (1 + i_5X);
      goto L3037;}}}
}
static long copy_weak_pointer(long weak_7X, char * frontier_8X, char * *TT0)
{
  char * arg1K0;
  long new_13X;
  char * frontier_12X;
  char * new_frontier_11X;
  char * old_10X;
  char * x_9X;
 {  x_9X = Sweak_pointer_hpS;
  if ((x_9X == NULL)) {
    goto L6768;}
  else {
    if (((Sweak_pointer_hpS) < (Sweak_pointer_limitS))) {
      arg1K0 = frontier_8X;
      goto L6773;}
    else {
      goto L6768;}}}
 L6768: {
  old_10X = Sweak_pointer_hpS;
  new_frontier_11X = frontier_8X + 2048;
  Sweak_pointer_hpS = frontier_8X;
  Sweak_pointer_limitS = new_frontier_11X;
  *((long *) (Sweak_pointer_hpS)) = (long) (522310);
  *((long *) ((Sweak_pointer_hpS) + 16)) = (long) ((((long) old_10X)));
  arg1K0 = new_frontier_11X;
  goto L6773;}
 L6773: {
  frontier_12X = arg1K0;
  new_13X = 3 + (((long) ((Sweak_pointer_hpS) + 8)));
  *((long *) ((Sweak_pointer_hpS) + 8)) = (long) ((*((long *) (((char *) (-3 + weak_7X))))));
  Sweak_pointer_hpS = ((Sweak_pointer_hpS) + 16);
  *((long *) (((char *) (-11 + weak_7X)))) = (long) (new_13X);
  *TT0 = frontier_12X;
  return new_13X;}
}
static void table_setB(struct table *table_14X, long key_15X, struct image_location *value_16X)
{
  long arg0K0;
  long i_32X;
  struct image_location *value_31X;
  long key_30X;
  long i_29X;
  long i_28X;
  struct image_location **new_values_27X;
  long *new_keys_26X;
  long new_size_25X;
  struct image_location **old_values_24X;
  long v_23X;
  long old_size_22X;
  long *old_keys_21X;
  long next_20X;
  long i_19X;
  long *keys_18X;
  long size_17X;
 {  if ((0 < (table_14X->size))) {
    size_17X = table_14X->size;
    keys_18X = table_14X->keys;
    arg0K0 = ((key_15X ^ ((PS_SHIFT_LEFT_INLINE(key_15X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_15X, 10)))) & (-1 + size_17X));
    goto L7276;}
  else {
    return;}}
 L7276: {
  i_19X = arg0K0;
  next_20X = *(keys_18X + i_19X);
  if ((key_15X == next_20X)) {
    *((table_14X->values) + i_19X) = value_16X;
    return;}
  else {
    if ((0 == next_20X)) {
      if ((i_19X == (table_14X->size))) {
        arg0K0 = 0;
        goto L7276;}
      else {
        *((table_14X->keys) + i_19X) = key_15X;
        *((table_14X->values) + i_19X) = value_16X;
        table_14X->count = (1 + (table_14X->count));
        if (((table_14X->count) == ((table_14X->size) / 3))) {
          old_keys_21X = table_14X->keys;
          old_size_22X = table_14X->size;
          v_23X = table_14X->size;
          old_values_24X = table_14X->values;
          new_size_25X = PS_SHIFT_LEFT_INLINE(v_23X, 1);
          new_keys_26X = (long*)malloc(sizeof(long) * (1 + new_size_25X));
          new_values_27X = (struct image_location**)malloc(sizeof(struct image_location*) * new_size_25X);
          if ((NULL == new_keys_26X)) {
            goto L4457;}
          else {
            if ((NULL == new_values_27X)) {
              goto L4457;}
            else {
              table_14X->keys = new_keys_26X;
              table_14X->values = new_values_27X;
              table_14X->size = new_size_25X;
              table_14X->count = 0;
              arg0K0 = 0;
              goto L4610;}}}
        else {
          return;}}}
    else {
      arg0K0 = (1 + i_19X);
      goto L7276;}}}
 L4457: {
  if ((NULL == new_keys_26X)) {
    goto L4465;}
  else {
    free(new_keys_26X);
    goto L4465;}}
 L4610: {
  i_28X = arg0K0;
  if ((i_28X < (1 + new_size_25X))) {
    *(new_keys_26X + i_28X) = 0;
    arg0K0 = (1 + i_28X);
    goto L4610;}
  else {
    arg0K0 = 0;
    goto L4488;}}
 L4465: {
  if ((NULL == new_values_27X)) {
    goto L4473;}
  else {
    free(new_values_27X);
    goto L4473;}}
 L4488: {
  i_29X = arg0K0;
  if ((i_29X == old_size_22X)) {
    free(old_keys_21X);
    free(old_values_24X);
    return;}
  else {
    key_30X = *(old_keys_21X + i_29X);
    if ((0 == key_30X)) {
      goto L4504;}
    else {
      value_31X = *(old_values_24X + i_29X);
      arg0K0 = ((key_30X ^ ((PS_SHIFT_LEFT_INLINE(key_30X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_30X, 10)))) & (-1 + new_size_25X));
      goto L4632;}}}
 L4473: {
  table_14X->size = 0;
  return;}
 L4504: {
  arg0K0 = (1 + i_29X);
  goto L4488;}
 L4632: {
  i_32X = arg0K0;
  if ((0 == (*(new_keys_26X + i_32X)))) {
    if ((i_32X == new_size_25X)) {
      arg0K0 = 0;
      goto L4632;}
    else {
      *(new_keys_26X + i_32X) = key_30X;
      *(new_values_27X + i_32X) = value_31X;
      goto L4504;}}
  else {
    arg0K0 = (1 + i_32X);
    goto L4632;}}
}
static long real_copy_object(long thing_33X, char * frontier_34X, char * *TT0)
{
  long new_39X;
  char * data_addr_38X;
  char * a_37X;
  long descriptor_36X;
  long h_35X;
 {  h_35X = *((long *) (((char *) (-11 + thing_33X))));
  if ((3 == (3 & h_35X))) {
    *TT0 = frontier_34X;
    return h_35X;}
  else {
    if ((2102 == h_35X)) {
      descriptor_36X = *((long *) (((char *) (-3 + thing_33X))));
      if ((3 == (3 & descriptor_36X))) {
        a_37X = ((char *) (-3 + descriptor_36X));
        if ((a_37X < (Sfrom_beginS))) {
          goto L10678;}
        else {
          if ((a_37X < (Sfrom_endS))) {
            return copy_weak_pointer(thing_33X, frontier_34X, TT0);}
          else {
            goto L10678;}}}
      else {
        goto L10678;}}
    else {
      goto L10678;}}}
 L10678: {
  *((long *) frontier_34X) = (long) (h_35X);
  data_addr_38X = frontier_34X + 8;
  new_39X = 3 + (((long) data_addr_38X));
  *((long *) (((char *) (-11 + thing_33X)))) = (long) (new_39X);
  memmove((void *)data_addr_38X, (void *)(((char *) (-3 + thing_33X))),(PS_SHIFT_RIGHT_LOGICAL_INLINE(h_35X, 8)));
  *TT0 = (data_addr_38X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(h_35X, 8)))));
  return new_39X;}
}
static char resumer_recordP(long stob_40X)
{
  long type_41X;
 {  if ((3 == (3 & stob_40X))) {
    if ((9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + stob_40X))))), 2))))) {
      type_41X = *((long *) (((char *) (-3 + stob_40X))));
      if ((3 == (3 & type_41X))) {
        if ((9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + type_41X))))), 2))))) {
          return (3 == (3 & (*((long *) ((((char *) (-3 + type_41X))) + 8)))));}
        else {
          return 0;}}
      else {
        return 0;}}
    else {
      return 0;}}
  else {
    return 0;}}
}
static long trace_image_value(long thing_42X)
{
  struct image_location *arg2K0;
  struct image_location *arg2K1;
  long arg0K0;
  long merged_arg0K0;

#ifdef USE_DIRECT_THREADING
  void *gc_recordP_return_address;
#else
  int gc_recordP_return_tag;
#endif
  char gc_recordP0_return_value;
  long x_43X;
  long header_63X;
  char v_62X;
  long x_61X;
  struct image_location *new_60X;
  long new_descriptor_59X;
  struct image_location *new_58X;
  struct image_location *image_location_57X;
  long new_descriptor_56X;
  char * data_addr_55X;
  long h_54X;
  long stob_53X;
  struct image_location *image_location_52X;
  long new_alias_51X;
  long i_50X;
  long len_49X;
  long vector_48X;
  char v_47X;
  long type_46X;
  char v_45X;
  struct image_location *have_44X;
 {  if ((3 == (3 & thing_42X))) {
    have_44X = table_ref((Sstob_tableS), thing_42X);
    if ((NULL == have_44X)) {
      merged_arg0K0 = thing_42X;
#ifdef USE_DIRECT_THREADING
      gc_recordP_return_address = &&gc_recordP_return_0;
#else
      gc_recordP_return_tag = 0;
#endif
      goto gc_recordP;
     gc_recordP_return_0:
      v_45X = gc_recordP0_return_value;
      if (v_45X) {
        type_46X = *((long *) (((char *) (-3 + thing_42X))));
        merged_arg0K0 = type_46X;
#ifdef USE_DIRECT_THREADING
        gc_recordP_return_address = &&gc_recordP_return_1;
#else
        gc_recordP_return_tag = 1;
#endif
        goto gc_recordP;
       gc_recordP_return_1:
        v_47X = gc_recordP0_return_value;
        if (v_47X) {
          if ((1 == (*((long *) ((((char *) (-3 + type_46X))) + 8))))) {
            if (((Sundumpable_countS) < (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-11 + (Sundumpable_recordsS)))))), 8))), 3)))) {
              vector_48X = Sundumpable_recordsS;
              len_49X = PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-11 + vector_48X))))), 8))), 3);
              arg0K0 = 0;
              goto L12669;}
            else {
              goto L14432;}}
          else {
            arg0K0 = thing_42X;
            goto L13162;}}
        else {
          arg0K0 = thing_42X;
          goto L13162;}}
      else {
        arg0K0 = thing_42X;
        goto L13162;}}
    else {
      return (have_44X->new_descriptor);}}
  else {
    return thing_42X;}}
 L12669: {
  i_50X = arg0K0;
  if ((i_50X == len_49X)) {
    *((long *) ((((char *) (-3 + (Sundumpable_recordsS)))) + (PS_SHIFT_LEFT_INLINE((Sundumpable_countS), 3)))) = (long) (thing_42X);
    Sundumpable_countS = (1 + (Sundumpable_countS));
    goto L14432;}
  else {
    if (((*((long *) ((((char *) (-3 + vector_48X))) + (PS_SHIFT_LEFT_INLINE(i_50X, 3))))) == thing_42X)) {
      goto L14432;}
    else {
      arg0K0 = (1 + i_50X);
      goto L12669;}}}
 L14432: {
  new_alias_51X = trace_image_value((*((long *) ((((char *) (-3 + thing_42X))) + 8))));
  image_location_52X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_52X)) {
    arg2K0 = image_location_52X;
    goto L14436;}
  else {
    image_location_52X->new_descriptor = new_alias_51X;
    image_location_52X->next = 0;
    arg2K0 = image_location_52X;
    goto L14436;}}
 L13162: {
  stob_53X = arg0K0;
  h_54X = *((long *) (((char *) (-11 + stob_53X))));
  data_addr_55X = (Simage_hpS) + 8;
  Simage_hpS = (data_addr_55X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(h_54X, 8)))));
  new_descriptor_56X = 3 + (((long) data_addr_55X));
  image_location_57X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_57X)) {
    arg0K0 = new_descriptor_56X;
    arg2K1 = image_location_57X;
    goto L13166;}
  else {
    image_location_57X->new_descriptor = new_descriptor_56X;
    image_location_57X->next = 0;
    arg0K0 = new_descriptor_56X;
    arg2K1 = image_location_57X;
    goto L13166;}}
 L14436: {
  new_58X = arg2K0;
  if ((NULL == new_58X)) {
    (Sstob_tableS)->size = 0;
    return new_alias_51X;}
  else {table_setB((Sstob_tableS), thing_42X, new_58X);
    return new_alias_51X;}}
 L13166: {
  new_descriptor_59X = arg0K0;
  new_60X = arg2K1;
  if ((NULL == new_60X)) {
    (Sstob_tableS)->size = 0;
    return new_descriptor_59X;}
  else {
    x_61X = Sfirst_stobS;
    if ((1 == x_61X)) {
      Sfirst_stobS = stob_53X;
      goto L13187;}
    else {
      (Slast_stobS)->next = stob_53X;
      goto L13187;}}}
 L13187: {
  Slast_stobS = new_60X;
  new_60X->next = 1;table_setB((Sstob_tableS), stob_53X, new_60X);
  v_62X = resumer_recordP(stob_53X);
  if (v_62X) {
    Sresumer_countS = (1 + (Sresumer_countS));
    return new_descriptor_59X;}
  else {
    return new_descriptor_59X;}}
 gc_recordP: {
  x_43X = merged_arg0K0;{
  if ((3 == (3 & x_43X))) {
    header_63X = *((long *) (((char *) (-11 + x_43X))));
    if ((3 == (3 & header_63X))) {
      if ((3 == (3 & header_63X))) {
        gc_recordP0_return_value = (9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + header_63X))))), 2))));
#ifdef USE_DIRECT_THREADING
        goto *gc_recordP_return_address;
#else
        goto gc_recordP_return;
#endif
}
      else {
        gc_recordP0_return_value = 0;
#ifdef USE_DIRECT_THREADING
        goto *gc_recordP_return_address;
#else
        goto gc_recordP_return;
#endif
}}
    else {
      if ((3 == (3 & x_43X))) {
        gc_recordP0_return_value = (9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + x_43X))))), 2))));
#ifdef USE_DIRECT_THREADING
        goto *gc_recordP_return_address;
#else
        goto gc_recordP_return;
#endif
}
      else {
        gc_recordP0_return_value = 0;
#ifdef USE_DIRECT_THREADING
        goto *gc_recordP_return_address;
#else
        goto gc_recordP_return;
#endif
}}}
  else {
    gc_recordP0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *gc_recordP_return_address;
#else
    goto gc_recordP_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 gc_recordP_return:
  switch (gc_recordP_return_tag) {
  case 0: goto gc_recordP_return_0;
  default: goto gc_recordP_return_1;
  }
#endif
}

}
void s48_write_barrier(long stob_64X, char * address_65X, long value_66X)
{

 {  return;}
}
void s48_forbid_gcB(void)
{

 {  return;}
}
void s48_allow_gcB(void)
{

 {  return;}
}
char s48_gc_can_allocate_unmovableP(void)
{

 {  return 0;}
}
char * s48_allocate_traced_unmovableAgc(long len_67X)
{

 {  ps_error("twospace gc does not support unmovable objects", 0);
  return NULL;}
}
char * s48_allocate_untraced_unmovableAgc(long len_68X)
{

 {  ps_error("twospace gc does not support unmovable objects", 0);
  return NULL;}
}
char s48_unmovableP(long stob_69X)
{

 {  return 0;}
}
long s48_allocate_unmovable_stob(long type_70X, long size_71X)
{

 {  ps_error("twospace gc does not support unmovable objects", 0);
  return 0;}
}
long s48_heap_size(void)
{

 {  return ((Snewspace_endS) - (Snewspace_beginS));}
}
long s48_gc_count(void)
{

 {  return (Sgc_countS);}
}
long s48_gc_run_time(long *TT0)
{

 {  *TT0 = (Sgc_msecondsS);
  return (Sgc_secondsS);}
}
void s48_initialization_completeB(void)
{

 {  SinitializingPS = 0;
  return;}
}
long s48_max_heap_size(void)
{

 {  return (PS_SHIFT_RIGHT_INLINE((7 + ((Snewspace_endS) - (Snewspace_beginS))), 3));}
}
long s48_startup_procedure(void)
{

 {  return (Sstartup_procedureS);}
}
long s48_initial_symbols(void)
{

 {  return (SsymbolsS);}
}
long s48_initial_imported_bindings(void)
{

 {  return (Simported_bindingsS);}
}
long s48_initial_exported_bindings(void)
{

 {  return (Sexported_bindingsS);}
}
long s48_resumer_records(void)
{

 {  return (Sresumer_recordsS);}
}
void s48_set_image_valuesB(long startup_proc_72X, long symbols_73X, long imports_74X, long exports_75X, long records_76X)
{

 {  Sstartup_procedureS = startup_proc_72X;
  SsymbolsS = symbols_73X;
  Simported_bindingsS = imports_74X;
  Sexported_bindingsS = exports_75X;
  Sresumer_recordsS = records_76X;
  return;}
}
char * s48_allocate_small(long len_77X)
{
  char * new_78X;
 {  new_78X = s48_ShpS;
  s48_ShpS = ((s48_ShpS) + (-8 & (7 + len_77X)));
  return new_78X;}
}
char s48_stob_in_heapP(long stob_79X)
{
  char temp_81X;
  char * addr_80X;
 {  addr_80X = ((char *) (-11 + stob_79X));
  temp_81X = addr_80X < (Snewspace_beginS);
  if (temp_81X) {
    goto L3918;}
  else {
    if ((addr_80X < (s48_ShpS))) {
      if ((2 == (3 & (*((long *) addr_80X))))) {
        return 1;}
      else {
        ps_write_string("Heap-check: stob has no header.", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        Sheap_errors_leftS = (-1 + (Sheap_errors_leftS));
        return ((Sheap_errors_leftS) < 1);}}
    else {
      goto L3918;}}}
 L3918: {
  ps_write_string("Heap-check: address out of bounds.", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  Sheap_errors_leftS = (-1 + (Sheap_errors_leftS));
  return ((Sheap_errors_leftS) < 1);}
}
long s48_available(void)
{

 {  return (PS_SHIFT_RIGHT_INLINE(((s48_SlimitS) - (s48_ShpS)), 3));}
}
char s48_extantP(long thing_82X)
{
  char * a_83X;
 {  if ((3 == (3 & thing_82X))) {
    if ((3 == (3 & thing_82X))) {
      a_83X = ((char *) (-3 + thing_82X));
      if ((a_83X < (Sfrom_beginS))) {
        return 1;}
      else {
        if ((a_83X < (Sfrom_endS))) {
          return (3 == (3 & (*((long *) (((char *) (-11 + thing_82X)))))));}
        else {
          return 1;}}}
    else {
      return 1;}}
  else {
    return 1;}}
}
long s48_gather_objects(char (*predicate_84X)(long), char (*for_each_object_85X)(char(*)(long)))
{
  char v_87X;
  char * start_hp_86X;
 {  Scollect_predicateS = predicate_84X;
  start_hp_86X = s48_ShpS;
  *((long *) (s48_ShpS)) = (long) (0);
  s48_ShpS = ((s48_ShpS) + 8);
  v_87X = (*for_each_object_85X)(pD1);
  if (v_87X) {
    *((long *) start_hp_86X) = (long) ((10 + (PS_SHIFT_LEFT_INLINE(((s48_ShpS) - (start_hp_86X + 8)), 8))));
    return (3 + (((long) (start_hp_86X + 8))));}
  else {
    s48_ShpS = start_hp_86X;
    return 1;}}
}
static char pD1(long obj_88X)
{
  char x_89X;
 {  x_89X = (*(Scollect_predicateS))(obj_88X);
  if (x_89X) {
    if ((((s48_ShpS) + 64) < (s48_SlimitS))) {
      *((long *) (s48_ShpS)) = (long) (obj_88X);
      s48_ShpS = ((s48_ShpS) + 8);
      return 1;}
    else {
      return 0;}}
  else {
    return 1;}}
}
char s48_check_heap(long error_count_90X)
{
  char * arg1K0;
  long arg0K0;
  char * merged_arg1K1;
  char * merged_arg1K0;

#ifdef USE_DIRECT_THREADING
  void *check_area_return_address;
#else
  int check_area_return_tag;
#endif
  char check_area0_return_value;
  char * start_91X;
  char * end_92X;
  char v_109X;
  long x_108X;
  char * addr_107X;
  char * next_106X;
  long d_105X;
  char * addr_104X;
  char v_103X;
  long i_102X;
  long count_101X;
  long *sizes_100X;
  char * *areas_99X;
  char v_98X;
  long i_97X;
  long count_96X;
  long *sizes_95X;
  char * *areas_94X;
  char v_93X;
 {  Sheap_errors_leftS = error_count_90X;
  merged_arg1K0 = (Snewspace_beginS);
  merged_arg1K1 = (s48_ShpS);
#ifdef USE_DIRECT_THREADING
  check_area_return_address = &&check_area_return_0;
#else
  check_area_return_tag = 0;
#endif
  goto check_area;
 check_area_return_0:
  v_93X = check_area0_return_value;
  if (v_93X) {
    if ((0 < (Simpure_area_countS))) {
      areas_94X = Simpure_areasS;
      sizes_95X = Simpure_sizesS;
      count_96X = Simpure_area_countS;
      arg0K0 = 0;
      goto L11599;}
    else {
      goto L11583;}}
  else {
    return 0;}}
 L11599: {
  i_97X = arg0K0;
  if ((i_97X < count_96X)) {
    merged_arg1K0 = (*(areas_94X + i_97X));
    merged_arg1K1 = ((*(areas_94X + i_97X)) + (*(sizes_95X + i_97X)));
#ifdef USE_DIRECT_THREADING
    check_area_return_address = &&check_area_return_1;
#else
    check_area_return_tag = 1;
#endif
    goto check_area;
   check_area_return_1:
    v_98X = check_area0_return_value;
    if (v_98X) {
      arg0K0 = (1 + i_97X);
      goto L11599;}
    else {
      return 0;}}
  else {
    goto L11583;}}
 L11583: {
  if ((0 < (Spure_area_countS))) {
    areas_99X = Spure_areasS;
    sizes_100X = Spure_sizesS;
    count_101X = Spure_area_countS;
    arg0K0 = 0;
    goto L11620;}
  else {
    return 1;}}
 L11620: {
  i_102X = arg0K0;
  if ((i_102X < count_101X)) {
    merged_arg1K0 = (*(areas_99X + i_102X));
    merged_arg1K1 = ((*(areas_99X + i_102X)) + (*(sizes_100X + i_102X)));
#ifdef USE_DIRECT_THREADING
    check_area_return_address = &&check_area_return_2;
#else
    check_area_return_tag = 2;
#endif
    goto check_area;
   check_area_return_2:
    v_103X = check_area0_return_value;
    if (v_103X) {
      arg0K0 = (1 + i_102X);
      goto L11620;}
    else {
      return 0;}}
  else {
    return 1;}}
 check_area: {
  start_91X = merged_arg1K0;
  end_92X = merged_arg1K1;{
  arg1K0 = start_91X;
  goto L10511;}
 L10511: {
  addr_104X = arg1K0;
  if ((addr_104X < end_92X)) {
    d_105X = *((long *) addr_104X);
    next_106X = addr_104X + (8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(d_105X, 8)))));
    if ((2 == (3 & d_105X))) {
      if ((end_92X < next_106X)) {
        ps_write_string("Heap-check: header too large.", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        Sheap_errors_leftS = (-1 + (Sheap_errors_leftS));
        check_area0_return_value = ((Sheap_errors_leftS) < 1);
#ifdef USE_DIRECT_THREADING
        goto *check_area_return_address;
#else
        goto check_area_return;
#endif
}
      else {
        if ((2 == (3 & d_105X))) {
          if (((31 & (PS_SHIFT_RIGHT_INLINE(d_105X, 2))) < 16)) {
            goto L10537;}
          else {
            arg1K0 = next_106X;
            goto L10511;}}
        else {
          goto L10537;}}}
    else {
      ps_write_string("Heap-check: unexpected non-header.", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      Sheap_errors_leftS = (-1 + (Sheap_errors_leftS));
      check_area0_return_value = ((Sheap_errors_leftS) < 1);
#ifdef USE_DIRECT_THREADING
      goto *check_area_return_address;
#else
      goto check_area_return;
#endif
}}
  else {
    check_area0_return_value = 1;
#ifdef USE_DIRECT_THREADING
    goto *check_area_return_address;
#else
    goto check_area_return;
#endif
}}
 L10537: {
  arg1K0 = (addr_104X + 8);
  goto L9271;}
 L9271: {
  addr_107X = arg1K0;
  if ((addr_107X == next_106X)) {
    arg1K0 = next_106X;
    goto L10511;}
  else {
    x_108X = *((long *) addr_107X);
    if ((2 == (3 & x_108X))) {
      ps_write_string("Heap-check: unexpected header.", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      Sheap_errors_leftS = (-1 + (Sheap_errors_leftS));
      if (((Sheap_errors_leftS) < 1)) {
        arg1K0 = next_106X;
        goto L10511;}
      else {
        check_area0_return_value = 0;
#ifdef USE_DIRECT_THREADING
        goto *check_area_return_address;
#else
        goto check_area_return;
#endif
}}
    else {
      if ((3 == (3 & x_108X))) {
        v_109X = s48_stob_in_heapP(x_108X);
        if (v_109X) {
          goto L9296;}
        else {
          check_area0_return_value = 0;
#ifdef USE_DIRECT_THREADING
          goto *check_area_return_address;
#else
          goto check_area_return;
#endif
}}
      else {
        goto L9296;}}}}
 L9296: {
  arg1K0 = (addr_107X + 8);
  goto L9271;}
#ifndef USE_DIRECT_THREADING
 check_area_return:
  switch (check_area_return_tag) {
  case 0: goto check_area_return_0;
  case 1: goto check_area_return_1;
  default: goto check_area_return_2;
  }
#endif
}

}
long s48_find_all(long type_110X)
{
  char * arg1K0;
  long arg0K0;
  char * merged_arg1K1;
  char * merged_arg1K0;

#ifdef USE_DIRECT_THREADING
  void *Hproc24111_return_address;
#else
  int Hproc24111_return_tag;
#endif
  char Hproc241110_return_value;
  char * start_112X;
  char * end_113X;
  char * next_130X;
  long d_129X;
  char * addr_128X;
  long type_127X;
  char v_126X;
  long i_125X;
  long count_124X;
  long *sizes_123X;
  char * *areas_122X;
  char v_121X;
  long i_120X;
  long count_119X;
  long *sizes_118X;
  char * *areas_117X;
  char v_116X;
  char * start_hp_115X;
  char * start_hp_114X;
 {  Sfinding_typeS = type_110X;
  start_hp_114X = s48_ShpS;
  start_hp_115X = s48_ShpS;
  *((long *) (s48_ShpS)) = (long) (0);
  s48_ShpS = ((s48_ShpS) + 8);
  merged_arg1K0 = (Snewspace_beginS);
  merged_arg1K1 = start_hp_114X;
#ifdef USE_DIRECT_THREADING
  Hproc24111_return_address = &&Hproc24111_return_0;
#else
  Hproc24111_return_tag = 0;
#endif
  goto Hproc24111;
 Hproc24111_return_0:
  v_116X = Hproc241110_return_value;
  if (v_116X) {
    if ((0 < (Simpure_area_countS))) {
      areas_117X = Simpure_areasS;
      sizes_118X = Simpure_sizesS;
      count_119X = Simpure_area_countS;
      arg0K0 = 0;
      goto L11734;}
    else {
      goto L11703;}}
  else {
    goto L11716;}}
 L11734: {
  i_120X = arg0K0;
  if ((i_120X < count_119X)) {
    merged_arg1K0 = (*(areas_117X + i_120X));
    merged_arg1K1 = ((*(areas_117X + i_120X)) + (*(sizes_118X + i_120X)));
#ifdef USE_DIRECT_THREADING
    Hproc24111_return_address = &&Hproc24111_return_1;
#else
    Hproc24111_return_tag = 1;
#endif
    goto Hproc24111;
   Hproc24111_return_1:
    v_121X = Hproc241110_return_value;
    if (v_121X) {
      arg0K0 = (1 + i_120X);
      goto L11734;}
    else {
      goto L11716;}}
  else {
    goto L11703;}}
 L11703: {
  if ((0 < (Spure_area_countS))) {
    areas_122X = Spure_areasS;
    sizes_123X = Spure_sizesS;
    count_124X = Spure_area_countS;
    arg0K0 = 0;
    goto L11755;}
  else {
    goto L11718;}}
 L11716: {
  s48_ShpS = start_hp_115X;
  return 1;}
 L11755: {
  i_125X = arg0K0;
  if ((i_125X < count_124X)) {
    merged_arg1K0 = (*(areas_122X + i_125X));
    merged_arg1K1 = ((*(areas_122X + i_125X)) + (*(sizes_123X + i_125X)));
#ifdef USE_DIRECT_THREADING
    Hproc24111_return_address = &&Hproc24111_return_2;
#else
    Hproc24111_return_tag = 2;
#endif
    goto Hproc24111;
   Hproc24111_return_2:
    v_126X = Hproc241110_return_value;
    if (v_126X) {
      arg0K0 = (1 + i_125X);
      goto L11755;}
    else {
      goto L11716;}}
  else {
    goto L11718;}}
 L11718: {
  *((long *) start_hp_115X) = (long) ((10 + (PS_SHIFT_LEFT_INLINE(((s48_ShpS) - (start_hp_115X + 8)), 8))));
  return (3 + (((long) (start_hp_115X + 8))));}
 Hproc24111: {
  start_112X = merged_arg1K0;
  end_113X = merged_arg1K1;{
  type_127X = Sfinding_typeS;
  arg1K0 = start_112X;
  goto L11029;}
 L11029: {
  addr_128X = arg1K0;
  if ((addr_128X < end_113X)) {
    d_129X = *((long *) addr_128X);
    next_130X = addr_128X + (8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(d_129X, 8)))));
    if ((2 == (3 & d_129X))) {
      if ((type_127X == (31 & (PS_SHIFT_RIGHT_INLINE(d_129X, 2))))) {
        if ((((s48_ShpS) + 64) < (s48_SlimitS))) {
          *((long *) (s48_ShpS)) = (long) ((3 + (((long) (addr_128X + 8)))));
          s48_ShpS = ((s48_ShpS) + 8);
          arg1K0 = next_130X;
          goto L11029;}
        else {
          Hproc241110_return_value = 0;
#ifdef USE_DIRECT_THREADING
          goto *Hproc24111_return_address;
#else
          goto Hproc24111_return;
#endif
}}
      else {
        arg1K0 = next_130X;
        goto L11029;}}
    else {
      ps_write_string("heap is in an inconsistent state.", (stderr));
      Hproc241110_return_value = 0;
#ifdef USE_DIRECT_THREADING
      goto *Hproc24111_return_address;
#else
      goto Hproc24111_return;
#endif
}}
  else {
    Hproc241110_return_value = 1;
#ifdef USE_DIRECT_THREADING
    goto *Hproc24111_return_address;
#else
    goto Hproc24111_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 Hproc24111_return:
  switch (Hproc24111_return_tag) {
  case 0: goto Hproc24111_return_0;
  case 1: goto Hproc24111_return_1;
  default: goto Hproc24111_return_2;
  }
#endif
}

}
long s48_trace_value(long stob_131X)
{
  char * new_hp_134X;
  long new_thing_133X;
  char * a_132X;
 {  if ((3 == (3 & stob_131X))) {
    a_132X = ((char *) (-3 + stob_131X));
    if ((a_132X < (Sfrom_beginS))) {
      return stob_131X;}
    else {
      if ((a_132X < (Sfrom_endS))) {
        new_thing_133X = real_copy_object(stob_131X, (s48_ShpS), &new_hp_134X);
        s48_ShpS = new_hp_134X;
        return new_thing_133X;}
      else {
        return stob_131X;}}}
  else {
    return stob_131X;}}
}
long s48_find_all_records(long record_type_135X)
{
  char * arg1K0;
  long arg0K0;
  char * merged_arg1K1;
  char * merged_arg1K0;

#ifdef USE_DIRECT_THREADING
  void *Hproc95136_return_address;
#else
  int Hproc95136_return_tag;
#endif
  char Hproc951360_return_value;
  char * start_137X;
  char * end_138X;
  long obj_156X;
  char * next_155X;
  long d_154X;
  char * addr_153X;
  long type_152X;
  char v_151X;
  long i_150X;
  long count_149X;
  long *sizes_148X;
  char * *areas_147X;
  char v_146X;
  long i_145X;
  long count_144X;
  long *sizes_143X;
  char * *areas_142X;
  char v_141X;
  char * start_hp_140X;
  char * start_hp_139X;
 {  Hthe_record_type270 = record_type_135X;
  Sfinding_typeS = 9;
  start_hp_139X = s48_ShpS;
  start_hp_140X = s48_ShpS;
  *((long *) (s48_ShpS)) = (long) (0);
  s48_ShpS = ((s48_ShpS) + 8);
  merged_arg1K0 = (Snewspace_beginS);
  merged_arg1K1 = start_hp_139X;
#ifdef USE_DIRECT_THREADING
  Hproc95136_return_address = &&Hproc95136_return_0;
#else
  Hproc95136_return_tag = 0;
#endif
  goto Hproc95136;
 Hproc95136_return_0:
  v_141X = Hproc951360_return_value;
  if (v_141X) {
    if ((0 < (Simpure_area_countS))) {
      areas_142X = Simpure_areasS;
      sizes_143X = Simpure_sizesS;
      count_144X = Simpure_area_countS;
      arg0K0 = 0;
      goto L11864;}
    else {
      goto L11833;}}
  else {
    goto L11846;}}
 L11864: {
  i_145X = arg0K0;
  if ((i_145X < count_144X)) {
    merged_arg1K0 = (*(areas_142X + i_145X));
    merged_arg1K1 = ((*(areas_142X + i_145X)) + (*(sizes_143X + i_145X)));
#ifdef USE_DIRECT_THREADING
    Hproc95136_return_address = &&Hproc95136_return_1;
#else
    Hproc95136_return_tag = 1;
#endif
    goto Hproc95136;
   Hproc95136_return_1:
    v_146X = Hproc951360_return_value;
    if (v_146X) {
      arg0K0 = (1 + i_145X);
      goto L11864;}
    else {
      goto L11846;}}
  else {
    goto L11833;}}
 L11833: {
  if ((0 < (Spure_area_countS))) {
    areas_147X = Spure_areasS;
    sizes_148X = Spure_sizesS;
    count_149X = Spure_area_countS;
    arg0K0 = 0;
    goto L11885;}
  else {
    goto L11848;}}
 L11846: {
  s48_ShpS = start_hp_140X;
  return 1;}
 L11885: {
  i_150X = arg0K0;
  if ((i_150X < count_149X)) {
    merged_arg1K0 = (*(areas_147X + i_150X));
    merged_arg1K1 = ((*(areas_147X + i_150X)) + (*(sizes_148X + i_150X)));
#ifdef USE_DIRECT_THREADING
    Hproc95136_return_address = &&Hproc95136_return_2;
#else
    Hproc95136_return_tag = 2;
#endif
    goto Hproc95136;
   Hproc95136_return_2:
    v_151X = Hproc951360_return_value;
    if (v_151X) {
      arg0K0 = (1 + i_150X);
      goto L11885;}
    else {
      goto L11846;}}
  else {
    goto L11848;}}
 L11848: {
  *((long *) start_hp_140X) = (long) ((10 + (PS_SHIFT_LEFT_INLINE(((s48_ShpS) - (start_hp_140X + 8)), 8))));
  return (3 + (((long) (start_hp_140X + 8))));}
 Hproc95136: {
  start_137X = merged_arg1K0;
  end_138X = merged_arg1K1;{
  type_152X = Sfinding_typeS;
  arg1K0 = start_137X;
  goto L11196;}
 L11196: {
  addr_153X = arg1K0;
  if ((addr_153X < end_138X)) {
    d_154X = *((long *) addr_153X);
    next_155X = addr_153X + (8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(d_154X, 8)))));
    if ((2 == (3 & d_154X))) {
      if ((type_152X == (31 & (PS_SHIFT_RIGHT_INLINE(d_154X, 2))))) {
        obj_156X = 3 + (((long) (addr_153X + 8)));
        if (((*((long *) (((char *) (-3 + obj_156X))))) == (Hthe_record_type270))) {
          if ((((s48_ShpS) + 64) < (s48_SlimitS))) {
            *((long *) (s48_ShpS)) = (long) (obj_156X);
            s48_ShpS = ((s48_ShpS) + 8);
            arg1K0 = next_155X;
            goto L11196;}
          else {
            Hproc951360_return_value = 0;
#ifdef USE_DIRECT_THREADING
            goto *Hproc95136_return_address;
#else
            goto Hproc95136_return;
#endif
}}
        else {
          arg1K0 = next_155X;
          goto L11196;}}
      else {
        arg1K0 = next_155X;
        goto L11196;}}
    else {
      ps_write_string("heap is in an inconsistent state.", (stderr));
      Hproc951360_return_value = 0;
#ifdef USE_DIRECT_THREADING
      goto *Hproc95136_return_address;
#else
      goto Hproc95136_return;
#endif
}}
  else {
    Hproc951360_return_value = 1;
#ifdef USE_DIRECT_THREADING
    goto *Hproc95136_return_address;
#else
    goto Hproc95136_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 Hproc95136_return:
  switch (Hproc95136_return_tag) {
  case 0: goto Hproc95136_return_0;
  case 1: goto Hproc95136_return_1;
  default: goto Hproc95136_return_2;
  }
#endif
}

}
void s48_initializing_gc_root(void)
{
  long arg0K0;
  long expr_181X;
  char * new_hp_180X;
  long new_thing_179X;
  char * a_178X;
  long stob_177X;
  long expr_176X;
  char * new_hp_175X;
  long new_thing_174X;
  char * a_173X;
  long stob_172X;
  long expr_171X;
  char * new_hp_170X;
  long new_thing_169X;
  char * a_168X;
  long stob_167X;
  long expr_166X;
  char * new_hp_165X;
  long new_thing_164X;
  char * a_163X;
  long stob_162X;
  long expr_161X;
  char * new_hp_160X;
  long new_thing_159X;
  char * a_158X;
  long stob_157X;
 {  if ((SinitializingPS)) {
    stob_157X = Sstartup_procedureS;
    if ((3 == (3 & stob_157X))) {
      a_158X = ((char *) (-3 + stob_157X));
      if ((a_158X < (Sfrom_beginS))) {
        arg0K0 = stob_157X;
        goto L13606;}
      else {
        if ((a_158X < (Sfrom_endS))) {
          new_thing_159X = real_copy_object(stob_157X, (s48_ShpS), &new_hp_160X);
          s48_ShpS = new_hp_160X;
          arg0K0 = new_thing_159X;
          goto L13606;}
        else {
          arg0K0 = stob_157X;
          goto L13606;}}}
    else {
      arg0K0 = stob_157X;
      goto L13606;}}
  else {
    return;}}
 L13606: {
  expr_161X = arg0K0;
  Sstartup_procedureS = expr_161X;
  stob_162X = SsymbolsS;
  if ((3 == (3 & stob_162X))) {
    a_163X = ((char *) (-3 + stob_162X));
    if ((a_163X < (Sfrom_beginS))) {
      arg0K0 = stob_162X;
      goto L13612;}
    else {
      if ((a_163X < (Sfrom_endS))) {
        new_thing_164X = real_copy_object(stob_162X, (s48_ShpS), &new_hp_165X);
        s48_ShpS = new_hp_165X;
        arg0K0 = new_thing_164X;
        goto L13612;}
      else {
        arg0K0 = stob_162X;
        goto L13612;}}}
  else {
    arg0K0 = stob_162X;
    goto L13612;}}
 L13612: {
  expr_166X = arg0K0;
  SsymbolsS = expr_166X;
  stob_167X = Simported_bindingsS;
  if ((3 == (3 & stob_167X))) {
    a_168X = ((char *) (-3 + stob_167X));
    if ((a_168X < (Sfrom_beginS))) {
      arg0K0 = stob_167X;
      goto L13618;}
    else {
      if ((a_168X < (Sfrom_endS))) {
        new_thing_169X = real_copy_object(stob_167X, (s48_ShpS), &new_hp_170X);
        s48_ShpS = new_hp_170X;
        arg0K0 = new_thing_169X;
        goto L13618;}
      else {
        arg0K0 = stob_167X;
        goto L13618;}}}
  else {
    arg0K0 = stob_167X;
    goto L13618;}}
 L13618: {
  expr_171X = arg0K0;
  Simported_bindingsS = expr_171X;
  stob_172X = Sexported_bindingsS;
  if ((3 == (3 & stob_172X))) {
    a_173X = ((char *) (-3 + stob_172X));
    if ((a_173X < (Sfrom_beginS))) {
      arg0K0 = stob_172X;
      goto L13624;}
    else {
      if ((a_173X < (Sfrom_endS))) {
        new_thing_174X = real_copy_object(stob_172X, (s48_ShpS), &new_hp_175X);
        s48_ShpS = new_hp_175X;
        arg0K0 = new_thing_174X;
        goto L13624;}
      else {
        arg0K0 = stob_172X;
        goto L13624;}}}
  else {
    arg0K0 = stob_172X;
    goto L13624;}}
 L13624: {
  expr_176X = arg0K0;
  Sexported_bindingsS = expr_176X;
  stob_177X = Sresumer_recordsS;
  if ((3 == (3 & stob_177X))) {
    a_178X = ((char *) (-3 + stob_177X));
    if ((a_178X < (Sfrom_beginS))) {
      arg0K0 = stob_177X;
      goto L13630;}
    else {
      if ((a_178X < (Sfrom_endS))) {
        new_thing_179X = real_copy_object(stob_177X, (s48_ShpS), &new_hp_180X);
        s48_ShpS = new_hp_180X;
        arg0K0 = new_thing_179X;
        goto L13630;}
      else {
        arg0K0 = stob_177X;
        goto L13630;}}}
  else {
    arg0K0 = stob_177X;
    goto L13630;}}
 L13630: {
  expr_181X = arg0K0;
  Sresumer_recordsS = expr_181X;
  return;}
}
long s48_write_image(long resume_proc_182X, long undumpables_183X, FILE * port_184X)
{
  struct table *arg3K0;
  long arg0K0;
  char * arg1K0;
  char * merged_arg1K0;
  long merged_arg0K1;
  long merged_arg0K0;

#ifdef USE_DIRECT_THREADING
  void *write_descriptor_return_address;
#else
  int write_descriptor_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *write_shared_table_return_address;
#else
  int write_shared_table_return_tag;
#endif
  char write_shared_table0_return_value;
#ifdef USE_DIRECT_THREADING
  void *copy_image_data_return_address;
#else
  int copy_image_data_return_tag;
#endif
  long descriptor_185X;
  long table_186X;
  char * start_187X;
  long size_188X;
  long have_330X;
  long v_329X;
  long value_328X;
  struct image_location *have_327X;
  long thing_326X;
  long link_325X;
  struct image_location *v_324X;
  long next_323X;
  long shared_322X;
  long link_321X;
  char temp_320X;
  long i_319X;
  long have_318X;
  long value_317X;
  long v_316X;
  struct image_location *have_315X;
  long v_314X;
  long v_313X;
  struct image_location *have_312X;
  long thing_311X;
  struct image_location *have_310X;
  long thing_309X;
  long x_308X;
  char * addr_307X;
  long v_306X;
  long value_305X;
  struct image_location *have_304X;
  long thing_303X;
  char * addr_302X;
  char * start_301X;
  long header_300X;
  long next_299X;
  struct image_location *have_298X;
  long thing_297X;
  struct image_location *have_296X;
  char * addr_295X;
  long v_294X;
  long link_293X;
  struct image_location *v_292X;
  long next_291X;
  char * start_290X;
  long header_289X;
  long next_288X;
  long value_287X;
  long have_286X;
  long available_285X;
  long size_284X;
  char * start_283X;
  long header_282X;
  long shared_281X;
  long link_280X;
  struct image_location *v_279X;
  long next_278X;
  struct image_location *have_277X;
  long thing_276X;
  long i_275X;
  long link_274X;
  long symbol_273X;
  long v_272X;
  long link_271X;
  struct image_location *v_270X;
  long next_269X;
  struct image_location **values_268X;
  long *keys_267X;
  struct table *table_266X;
  long link_265X;
  struct image_location *have_264X;
  long thing_263X;
  long id_262X;
  char * addr_261X;
  long symbol_260X;
  long have_259X;
  char v_258X;
  struct image_location *location_257X;
  long stob_256X;
  char * start_255X;
  long header_254X;
  struct image_location *have_253X;
  long thing_252X;
  long link_251X;
  long v_250X;
  long v_249X;
  long i_248X;
  struct image_location *v_247X;
  long thing_246X;
  long table_245X;
  struct image_location *location_244X;
  long stob_243X;
  struct table *stob_table_242X;
  long first_stob_241X;
  long v_240X;
  long v_239X;
  long v_238X;
  long v_237X;
  long n_236X;
  struct image_location *have_235X;
  long thing_234X;
  long v_233X;
  long n_232X;
  struct image_location *have_231X;
  long thing_230X;
  long v_229X;
  long n_228X;
  struct image_location *have_227X;
  long thing_226X;
  long v_225X;
  long cells_224X;
  long v_223X;
  long cells_222X;
  long v_221X;
  long v_220X;
  long v_219X;
  long v_218X;
  long i_217X;
  long v_216X;
  struct image_location **values_215X;
  long *keys_214X;
  struct table *table_213X;
  long v_212X;
  long resumer_records_211X;
  char * data_addr_210X;
  long cells_209X;
  long v_208X;
  long v_207X;
  long v_206X;
  struct image_location *last_205X;
  char * addr_204X;
  long next_203X;
  struct image_location *image_location_202X;
  char * start_201X;
  long link_200X;
  long entry_199X;
  long header_198X;
  long stob_197X;
  long link_196X;
  long i_195X;
  long table_194X;
  long resume_proc_193X;
  struct table *v_192X;
  struct table *table_191X;
  long i_190X;
  long *keys_189X;
 {  keys_189X = (long*)malloc(sizeof(long) * 4097);
  arg0K0 = 0;
  goto L7192;}
 L7192: {
  i_190X = arg0K0;
  if ((i_190X < 4097)) {
    *(keys_189X + i_190X) = 0;
    arg0K0 = (1 + i_190X);
    goto L7192;}
  else {
    table_191X = (struct table*)malloc(sizeof(struct table));
    if ((NULL == table_191X)) {
      arg3K0 = table_191X;
      goto L7165;}
    else {
      table_191X->keys = keys_189X;
      table_191X->values = ((struct image_location**)malloc(sizeof(struct image_location*) * 4096));
      table_191X->count = 0;
      table_191X->size = 4096;
      arg3K0 = table_191X;
      goto L7165;}}}
 L7165: {
  v_192X = arg3K0;
  Sstob_tableS = v_192X;
  Sfirst_stobS = 1;
  Slast_stobS = (NULL);
  Sundumpable_recordsS = undumpables_183X;
  Sundumpable_countS = 0;
  Sresumer_countS = 0;
  image_start_address = (((long) (Snew_heap_start_addrS)));
  Simage_beginS = (((char *) (image_start_address)));
  Simage_hpS = (((char *) (image_start_address)));
  Simage_portS = port_184X;
  Simage_bufferS = ((char *)malloc(4096));
  Simage_buffer_pointerS = (Simage_bufferS);
  SstatusS = NO_ERRORS;
  if (((Simage_bufferS) == NULL)) {
    return ENOMEM;}
  else {
    resume_proc_193X = trace_image_value(resume_proc_182X);
    table_194X = s48_exported_bindings();
    arg0K0 = 0;
    goto L12022;}}
 L12022: {
  i_195X = arg0K0;
  if ((1024 == i_195X)) {
    arg0K0 = (Sfirst_stobS);
    goto L14220;}
  else {
    link_196X = *((long *) ((((char *) (-3 + table_194X))) + (PS_SHIFT_LEFT_INLINE(i_195X, 3))));
    if ((0 == (3 & link_196X))) {
      arg0K0 = (3 + (-4 & link_196X));
      goto L11996;}
    else {
      arg0K0 = link_196X;
      goto L11996;}}}
 L14220: {
  stob_197X = arg0K0;
  header_198X = *((long *) (((char *) (-11 + stob_197X))));
  if ((2 == (3 & header_198X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(header_198X, 2))) < 16)) {
      goto L14084;}
    else {
      goto L14222;}}
  else {
    goto L14084;}}
 L11996: {
  entry_199X = arg0K0;
  if ((1 == entry_199X)) {
    arg0K0 = (1 + i_195X);
    goto L12022;}
  else {trace_image_value(entry_199X);
    link_200X = *((long *) ((((char *) (-3 + entry_199X))) + 24));
    if ((0 == (3 & link_200X))) {
      arg0K0 = (3 + (-4 & link_200X));
      goto L11996;}
    else {
      arg0K0 = link_200X;
      goto L11996;}}}
 L14084: {
  if ((2102 == header_198X)) {
    goto L14222;}
  else {
    start_201X = ((char *) (-3 + stob_197X));
    arg1K0 = start_201X;
    goto L14103;}}
 L14222: {
  if ((0 < ((Sstob_tableS)->size))) {
    image_location_202X = table_ref((Sstob_tableS), stob_197X);
    next_203X = image_location_202X->next;
    if ((3 == (3 & next_203X))) {
      arg0K0 = next_203X;
      goto L14220;}
    else {
      goto L14301;}}
  else {
    goto L14301;}}
 L14103: {
  addr_204X = arg1K0;
  if ((addr_204X == (start_201X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_198X, 8))))))) {
    goto L14222;}
  else {trace_image_value((*((long *) addr_204X)));
    arg1K0 = (addr_204X + 8);
    goto L14103;}}
 L14301: {
  last_205X = Slast_stobS;
  v_206X = s48_symbol_table();trace_image_value(v_206X);
  v_207X = s48_imported_bindings();trace_image_value(v_207X);
  v_208X = s48_exported_bindings();trace_image_value(v_208X);
  last_205X->next = 1;
  cells_209X = Sresumer_countS;
  data_addr_210X = (Simage_hpS) + 8;
  Simage_hpS = (data_addr_210X + (PS_SHIFT_LEFT_INLINE(cells_209X, 3)));
  Sresumer_recordsS = (3 + (((long) data_addr_210X)));
  if ((0 < ((Sstob_tableS)->size))) {
    resumer_records_211X = Sresumer_recordsS;
    if (((SstatusS) == NO_ERRORS)) {
      PS_WRITE_CHAR(10, port_184X, v_212X)
      SstatusS = v_212X;
      goto L6111;}
    else {
      goto L6111;}}
  else {
    table_213X = Sstob_tableS;
    keys_214X = table_213X->keys;
    values_215X = table_213X->values;
    arg0K0 = 0;
    goto L14617;}}
 L6111: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(12, (Simage_portS), v_216X)
    SstatusS = v_216X;
    goto L6113;}
  else {
    goto L6113;}}
 L14617: {
  i_217X = arg0K0;
  if ((i_217X == (table_213X->size))) {
    free(keys_214X);
    free(values_215X);
    free(table_213X);
    free((Simage_bufferS));
    return ENOMEM;}
  else {
    if ((0 == (*(keys_214X + i_217X)))) {
      goto L14619;}
    else {
      free((*(values_215X + i_217X)));
      goto L14619;}}}
 L6113: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_184X, v_218X)
    SstatusS = v_218X;
    goto L6122;}
  else {
    goto L6122;}}
 L14619: {
  arg0K0 = (1 + i_217X);
  goto L14617;}
 L6122: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_string("Vanilla 40", port_184X));
    goto L6129;}
  else {
    goto L6129;}}
 L6129: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_184X, v_219X)
    SstatusS = v_219X;
    goto L6138;}
  else {
    goto L6138;}}
 L6138: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(0, port_184X));
    goto L6145;}
  else {
    goto L6145;}}
 L6145: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_184X, v_220X)
    SstatusS = v_220X;
    goto L6154;}
  else {
    goto L6154;}}
 L6154: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(8, (Simage_portS)));
    goto L6199;}
  else {
    goto L6199;}}
 L6199: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_221X)
    SstatusS = v_221X;
    goto L6156;}
  else {
    goto L6156;}}
 L6156: {
  cells_222X = image_start_address;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_222X, 3)), (Simage_portS)));
    goto L6216;}
  else {
    goto L6216;}}
 L6216: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_223X)
    SstatusS = v_223X;
    goto L6160;}
  else {
    goto L6160;}}
 L6160: {
  cells_224X = (image_start_address) + ((Simage_hpS) - (Simage_beginS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_224X, 3)), (Simage_portS)));
    goto L6235;}
  else {
    goto L6235;}}
 L6235: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_225X)
    SstatusS = v_225X;
    goto L6166;}
  else {
    goto L6166;}}
 L6166: {
  thing_226X = s48_symbol_table();
  if ((3 == (3 & thing_226X))) {
    have_227X = table_ref((Sstob_tableS), thing_226X);
    if ((NULL == have_227X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L17892;}
    else {
      goto L17892;}}
  else {
    arg0K0 = thing_226X;
    goto L6170;}}
 L17892: {
  arg0K0 = (have_227X->new_descriptor);
  goto L6170;}
 L6170: {
  n_228X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_228X, (Simage_portS)));
    goto L6249;}
  else {
    goto L6249;}}
 L6249: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_229X)
    SstatusS = v_229X;
    goto L6172;}
  else {
    goto L6172;}}
 L6172: {
  thing_230X = s48_imported_bindings();
  if ((3 == (3 & thing_230X))) {
    have_231X = table_ref((Sstob_tableS), thing_230X);
    if ((NULL == have_231X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L17906;}
    else {
      goto L17906;}}
  else {
    arg0K0 = thing_230X;
    goto L6176;}}
 L17906: {
  arg0K0 = (have_231X->new_descriptor);
  goto L6176;}
 L6176: {
  n_232X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_232X, (Simage_portS)));
    goto L6263;}
  else {
    goto L6263;}}
 L6263: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_233X)
    SstatusS = v_233X;
    goto L6178;}
  else {
    goto L6178;}}
 L6178: {
  thing_234X = s48_exported_bindings();
  if ((3 == (3 & thing_234X))) {
    have_235X = table_ref((Sstob_tableS), thing_234X);
    if ((NULL == have_235X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L17920;}
    else {
      goto L17920;}}
  else {
    arg0K0 = thing_234X;
    goto L6182;}}
 L17920: {
  arg0K0 = (have_235X->new_descriptor);
  goto L6182;}
 L6182: {
  n_236X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_236X, (Simage_portS)));
    goto L6277;}
  else {
    goto L6277;}}
 L6277: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_237X)
    SstatusS = v_237X;
    goto L6184;}
  else {
    goto L6184;}}
 L6184: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(resumer_records_211X, (Simage_portS)));
    goto L6291;}
  else {
    goto L6291;}}
 L6291: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_238X)
    SstatusS = v_238X;
    goto L6186;}
  else {
    goto L6186;}}
 L6186: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(resume_proc_193X, (Simage_portS)));
    goto L6305;}
  else {
    goto L6305;}}
 L6305: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_239X)
    SstatusS = v_239X;
    goto L6188;}
  else {
    goto L6188;}}
 L6188: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(12, (Simage_portS), v_240X)
    SstatusS = v_240X;
    goto L14518;}
  else {
    goto L14518;}}
 L14518: {
  merged_arg0K0 = 1;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_0;
#else
  write_descriptor_return_tag = 0;
#endif
  goto write_descriptor;
 write_descriptor_return_0:
  first_stob_241X = Sfirst_stobS;
  stob_table_242X = Sstob_tableS;
  arg0K0 = first_stob_241X;
  goto L14024;}
 L14024: {
  stob_243X = arg0K0;
  if ((3 == (3 & stob_243X))) {
    location_244X = table_ref(stob_table_242X, stob_243X);
    if ((NULL == location_244X)) {
      ps_error("traced stob has no image-table entry", 0);
      goto L14029;}
    else {
      goto L14029;}}
  else {
    table_245X = s48_symbol_table();
    merged_arg0K0 = (*((long *) (((char *) (-11 + table_245X)))));
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_1;
#else
    write_descriptor_return_tag = 1;
#endif
    goto write_descriptor;
   write_descriptor_return_1:
    arg0K0 = 0;
    goto L12404;}}
 L14029: {
  if ((3 == (3 & stob_243X))) {
    if ((13 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + stob_243X))))), 2))))) {
      merged_arg0K0 = 2102;
#ifdef USE_DIRECT_THREADING
      write_descriptor_return_address = &&write_descriptor_return_2;
#else
      write_descriptor_return_tag = 2;
#endif
      goto write_descriptor;
     write_descriptor_return_2:
      thing_246X = *((long *) (((char *) (-3 + stob_243X))));
      if ((3 == (3 & thing_246X))) {
        v_247X = table_ref((Sstob_tableS), thing_246X);
        if ((NULL == v_247X)) {
          merged_arg0K0 = 1;
#ifdef USE_DIRECT_THREADING
          write_descriptor_return_address = &&write_descriptor_return_3;
#else
          write_descriptor_return_tag = 3;
#endif
          goto write_descriptor;
         write_descriptor_return_3:
          goto L14031;}
        else {
          goto L13750;}}
      else {
        goto L13750;}}
    else {
      goto L13756;}}
  else {
    goto L13756;}}
 L12404: {
  i_248X = arg0K0;
  if ((i_248X == (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-11 + table_245X))))), 8))), 3)))) {
    v_249X = s48_imported_bindings();
    merged_arg0K0 = v_249X;
#ifdef USE_DIRECT_THREADING
    write_shared_table_return_address = &&write_shared_table_return_0;
#else
    write_shared_table_return_tag = 0;
#endif
    goto write_shared_table;
   write_shared_table_return_0:
    v_250X = s48_exported_bindings();
    merged_arg0K0 = v_250X;
#ifdef USE_DIRECT_THREADING
    write_shared_table_return_address = &&write_shared_table_return_1;
#else
    write_shared_table_return_tag = 1;
#endif
    goto write_shared_table;
   write_shared_table_return_1:
    merged_arg0K0 = (10 + (PS_SHIFT_LEFT_INLINE((Sresumer_countS), 11)));
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_4;
#else
    write_descriptor_return_tag = 4;
#endif
    goto write_descriptor;
   write_descriptor_return_4:
    arg0K0 = (Sfirst_stobS);
    goto L14047;}
  else {
    link_251X = *((long *) ((((char *) (-3 + table_245X))) + (PS_SHIFT_LEFT_INLINE(i_248X, 3))));
    if ((0 == (3 & link_251X))) {
      arg0K0 = (3 + (-4 & link_251X));
      goto L12414;}
    else {
      arg0K0 = link_251X;
      goto L12414;}}}
 L14031: {
  arg0K0 = (location_244X->next);
  goto L14024;}
 L13750: {
  thing_252X = *((long *) (((char *) (-3 + stob_243X))));
  if ((3 == (3 & thing_252X))) {
    have_253X = table_ref((Sstob_tableS), thing_252X);
    if ((NULL == have_253X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L13814;}
    else {
      goto L13814;}}
  else {
    merged_arg0K0 = thing_252X;
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_5;
#else
    write_descriptor_return_tag = 5;
#endif
    goto write_descriptor;
   write_descriptor_return_5:
    goto L14031;}}
 L13756: {
  if ((3 == (3 & stob_243X))) {
    if ((6 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + stob_243X))))), 2))))) {
      header_254X = *((long *) (((char *) (-11 + stob_243X))));
      merged_arg0K0 = header_254X;
#ifdef USE_DIRECT_THREADING
      write_descriptor_return_address = &&write_descriptor_return_6;
#else
      write_descriptor_return_tag = 6;
#endif
      goto write_descriptor;
     write_descriptor_return_6:
      merged_arg0K0 = 0;
#ifdef USE_DIRECT_THREADING
      write_descriptor_return_address = &&write_descriptor_return_7;
#else
      write_descriptor_return_tag = 7;
#endif
      goto write_descriptor;
     write_descriptor_return_7:
      start_255X = (((char *) (-3 + stob_243X))) + 8;
      arg1K0 = start_255X;
      goto L10451;}
    else {
      goto L13760;}}
  else {
    goto L13760;}}
 L14047: {
  stob_256X = arg0K0;
  if ((3 == (3 & stob_256X))) {
    location_257X = table_ref((Sstob_tableS), stob_256X);
    v_258X = resumer_recordP(stob_256X);
    if (v_258X) {
      merged_arg0K0 = (location_257X->new_descriptor);
#ifdef USE_DIRECT_THREADING
      write_descriptor_return_address = &&write_descriptor_return_8;
#else
      write_descriptor_return_tag = 8;
#endif
      goto write_descriptor;
     write_descriptor_return_8:
      goto L14054;}
    else {
      goto L14054;}}
  else {
    have_259X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_259X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_259X));
        goto L14563;}
      else {
        goto L14563;}}
    else {
      goto L14524;}}}
 L12414: {
  symbol_260X = arg0K0;
  arg0K0 = symbol_260X;
  goto L12448;}
 L13814: {
  merged_arg0K0 = (have_253X->new_descriptor);
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_9;
#else
  write_descriptor_return_tag = 9;
#endif
  goto write_descriptor;
 write_descriptor_return_9:
  goto L14031;}
 L10451: {
  addr_261X = arg1K0;
  if ((addr_261X == (start_255X + (-8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_254X, 8)))))))) {
    ps_write_string("Channel closed in dumped image: ", (stderr));
    id_262X = *((long *) ((((char *) (-3 + stob_243X))) + 8));
    if ((0 == (3 & id_262X))) {
      ps_write_integer((PS_SHIFT_RIGHT_INLINE(id_262X, 2)), (stderr));
      goto L10428;}
    else {
      ps_write_string((((char *)(((char *) (-3 + id_262X))))), (stderr));
      goto L10428;}}
  else {
    thing_263X = *((long *) addr_261X);
    if ((3 == (3 & thing_263X))) {
      have_264X = table_ref((Sstob_tableS), thing_263X);
      if ((NULL == have_264X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L10465;}
      else {
        goto L10465;}}
    else {
      arg0K0 = thing_263X;
      goto L10456;}}}
 L13760: {
  if ((3 == (3 & stob_243X))) {
    if ((1 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + stob_243X))))), 2))))) {
      link_265X = *((long *) ((((char *) (-3 + stob_243X))) + 8));
      if ((0 == (3 & link_265X))) {
        arg0K0 = (3 + (-4 & link_265X));
        goto L13336;}
      else {
        arg0K0 = link_265X;
        goto L13336;}}
    else {
      goto L13764;}}
  else {
    goto L13764;}}
 L14054: {
  arg0K0 = (location_257X->next);
  goto L14047;}
 L14563: {
  Simage_buffer_pointerS = (Simage_bufferS);
  goto L14524;}
 L14524: {
  table_266X = Sstob_tableS;
  keys_267X = table_266X->keys;
  values_268X = table_266X->values;
  arg0K0 = 0;
  goto L14583;}
 L12448: {
  next_269X = arg0K0;
  if ((3 == (3 & next_269X))) {
    v_270X = table_ref((Sstob_tableS), next_269X);
    if ((NULL == v_270X)) {
      link_271X = *((long *) ((((char *) (-3 + next_269X))) + 8));
      if ((0 == (3 & link_271X))) {
        arg0K0 = (3 + (-4 & link_271X));
        goto L12448;}
      else {
        arg0K0 = link_271X;
        goto L12448;}}
    else {
      arg0K0 = next_269X;
      goto L12416;}}
  else {
    arg0K0 = next_269X;
    goto L12416;}}
 L10428: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  goto L14031;}
 L10465: {
  arg0K0 = (have_264X->new_descriptor);
  goto L10456;}
 L10456: {
  v_272X = arg0K0;
  merged_arg0K0 = v_272X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_10;
#else
  write_descriptor_return_tag = 10;
#endif
  goto write_descriptor;
 write_descriptor_return_10:
  arg1K0 = (addr_261X + 8);
  goto L10451;}
 L13336: {
  symbol_273X = arg0K0;
  arg0K0 = symbol_273X;
  goto L13369;}
 L13764: {
  if ((3 == (3 & stob_243X))) {
    if ((14 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + stob_243X))))), 2))))) {
      link_274X = *((long *) ((((char *) (-3 + stob_243X))) + 24));
      if ((0 == (3 & link_274X))) {
        arg0K0 = (3 + (-4 & link_274X));
        goto L13451;}
      else {
        arg0K0 = link_274X;
        goto L13451;}}
    else {
      goto L13768;}}
  else {
    goto L13768;}}
 L14583: {
  i_275X = arg0K0;
  if ((i_275X == (table_266X->size))) {
    free(keys_267X);
    free(values_268X);
    free(table_266X);
    free((Simage_bufferS));
    return (SstatusS);}
  else {
    if ((0 == (*(keys_267X + i_275X)))) {
      goto L14585;}
    else {
      free((*(values_268X + i_275X)));
      goto L14585;}}}
 L12416: {
  thing_276X = arg0K0;
  if ((3 == (3 & thing_276X))) {
    have_277X = table_ref((Sstob_tableS), thing_276X);
    if ((NULL == have_277X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L12467;}
    else {
      goto L12467;}}
  else {
    arg0K0 = thing_276X;
    goto L12418;}}
 L13369: {
  next_278X = arg0K0;
  if ((3 == (3 & next_278X))) {
    v_279X = table_ref((Sstob_tableS), next_278X);
    if ((NULL == v_279X)) {
      link_280X = *((long *) ((((char *) (-3 + next_278X))) + 8));
      if ((0 == (3 & link_280X))) {
        arg0K0 = (3 + (-4 & link_280X));
        goto L13369;}
      else {
        arg0K0 = link_280X;
        goto L13369;}}
    else {
      arg0K0 = next_278X;
      goto L13338;}}
  else {
    arg0K0 = next_278X;
    goto L13338;}}
 L13451: {
  shared_281X = arg0K0;
  arg0K0 = shared_281X;
  goto L13500;}
 L13768: {
  header_282X = *((long *) (((char *) (-11 + stob_243X))));
  start_283X = ((char *) (-3 + stob_243X));
  merged_arg0K0 = header_282X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_11;
#else
  write_descriptor_return_tag = 11;
#endif
  goto write_descriptor;
 write_descriptor_return_11:
  if ((2 == (3 & header_282X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(header_282X, 2))) < 16)) {
      goto L13784;}
    else {
      size_284X = -8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_282X, 8)));
      available_285X = 4096 - ((Simage_buffer_pointerS) - (Simage_bufferS));
      if ((available_285X < size_284X)) {
        if ((4096 < size_284X)) {
          have_286X = (Simage_buffer_pointerS) - (Simage_bufferS);
          if ((0 < have_286X)) {
            if (((SstatusS) == NO_ERRORS)) {
              SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_286X));
              goto L9477;}
            else {
              goto L9477;}}
          else {
            goto L9451;}}
        else {
          merged_arg1K0 = start_283X;
          merged_arg0K1 = available_285X;
#ifdef USE_DIRECT_THREADING
          copy_image_data_return_address = &&copy_image_data_return_0;
#else
          copy_image_data_return_tag = 0;
#endif
          goto copy_image_data;
         copy_image_data_return_0:
          merged_arg1K0 = (start_283X + available_285X);
          merged_arg0K1 = (size_284X - available_285X);
#ifdef USE_DIRECT_THREADING
          copy_image_data_return_address = &&copy_image_data_return_1;
#else
          copy_image_data_return_tag = 1;
#endif
          goto copy_image_data;
         copy_image_data_return_1:
          goto L14031;}}
      else {
        merged_arg1K0 = start_283X;
        merged_arg0K1 = size_284X;
#ifdef USE_DIRECT_THREADING
        copy_image_data_return_address = &&copy_image_data_return_2;
#else
        copy_image_data_return_tag = 2;
#endif
        goto copy_image_data;
       copy_image_data_return_2:
        goto L14031;}}}
  else {
    goto L13784;}}
 L14585: {
  arg0K0 = (1 + i_275X);
  goto L14583;}
 L12467: {
  arg0K0 = (have_277X->new_descriptor);
  goto L12418;}
 L12418: {
  value_287X = arg0K0;
  if ((3 == (3 & value_287X))) {
    arg0K0 = (-4 & value_287X);
    goto L12420;}
  else {
    arg0K0 = value_287X;
    goto L12420;}}
 L13338: {
  next_288X = arg0K0;
  header_289X = *((long *) (((char *) (-11 + stob_243X))));
  merged_arg0K0 = header_289X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_12;
#else
  write_descriptor_return_tag = 12;
#endif
  goto write_descriptor;
 write_descriptor_return_12:
  start_290X = ((char *) (-3 + stob_243X));
  arg1K0 = start_290X;
  goto L13398;}
 L13500: {
  next_291X = arg0K0;
  if ((3 == (3 & next_291X))) {
    v_292X = table_ref((Sstob_tableS), next_291X);
    if ((NULL == v_292X)) {
      link_293X = *((long *) ((((char *) (-3 + next_291X))) + 24));
      if ((0 == (3 & link_293X))) {
        arg0K0 = (3 + (-4 & link_293X));
        goto L13500;}
      else {
        arg0K0 = link_293X;
        goto L13500;}}
    else {
      arg0K0 = next_291X;
      goto L13453;}}
  else {
    arg0K0 = next_291X;
    goto L13453;}}
 L13784: {
  arg1K0 = start_283X;
  goto L13886;}
 L9477: {
  Simage_buffer_pointerS = (Simage_bufferS);
  goto L9451;}
 L9451: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_block((Simage_portS), ((char *) start_283X), size_284X));
    goto L14031;}
  else {
    goto L14031;}}
 L12420: {
  v_294X = arg0K0;
  merged_arg0K0 = v_294X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_13;
#else
  write_descriptor_return_tag = 13;
#endif
  goto write_descriptor;
 write_descriptor_return_13:
  arg0K0 = (1 + i_248X);
  goto L12404;}
 L13398: {
  addr_295X = arg1K0;
  if ((addr_295X == (start_290X + (-8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_289X, 8)))))))) {
    if ((3 == (3 & next_288X))) {
      have_296X = table_ref((Sstob_tableS), next_288X);
      if ((NULL == have_296X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13428;}
      else {
        goto L13428;}}
    else {
      arg0K0 = next_288X;
      goto L13351;}}
  else {
    thing_297X = *((long *) addr_295X);
    if ((3 == (3 & thing_297X))) {
      have_298X = table_ref((Sstob_tableS), thing_297X);
      if ((NULL == have_298X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13412;}
      else {
        goto L13412;}}
    else {
      arg0K0 = thing_297X;
      goto L13403;}}}
 L13453: {
  next_299X = arg0K0;
  header_300X = *((long *) (((char *) (-11 + stob_243X))));
  merged_arg0K0 = header_300X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_14;
#else
  write_descriptor_return_tag = 14;
#endif
  goto write_descriptor;
 write_descriptor_return_14:
  start_301X = ((char *) (-3 + stob_243X));
  arg1K0 = start_301X;
  goto L13529;}
 L13886: {
  addr_302X = arg1K0;
  if ((addr_302X == (start_283X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_282X, 8))))))) {
    goto L14031;}
  else {
    thing_303X = *((long *) addr_302X);
    if ((3 == (3 & thing_303X))) {
      have_304X = table_ref((Sstob_tableS), thing_303X);
      if ((NULL == have_304X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13900;}
      else {
        goto L13900;}}
    else {
      arg0K0 = thing_303X;
      goto L13891;}}}
 L13428: {
  arg0K0 = (have_296X->new_descriptor);
  goto L13351;}
 L13351: {
  value_305X = arg0K0;
  if ((3 == (3 & value_305X))) {
    merged_arg0K0 = (-4 & value_305X);
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_15;
#else
    write_descriptor_return_tag = 15;
#endif
    goto write_descriptor;
   write_descriptor_return_15:
    goto L14031;}
  else {
    merged_arg0K0 = value_305X;
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_16;
#else
    write_descriptor_return_tag = 16;
#endif
    goto write_descriptor;
   write_descriptor_return_16:
    goto L14031;}}
 L13412: {
  arg0K0 = (have_298X->new_descriptor);
  goto L13403;}
 L13403: {
  v_306X = arg0K0;
  merged_arg0K0 = v_306X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_17;
#else
  write_descriptor_return_tag = 17;
#endif
  goto write_descriptor;
 write_descriptor_return_17:
  arg1K0 = (addr_295X + 8);
  goto L13398;}
 L13529: {
  addr_307X = arg1K0;
  if ((addr_307X == (start_301X + (-16 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_300X, 8)))))))) {
    x_308X = *((long *) ((((char *) (-3 + stob_243X))) + 8));
    if ((5 == x_308X)) {
      arg0K0 = 529;
      goto L13478;}
    else {
      thing_309X = *((long *) ((((char *) (-3 + stob_243X))) + 16));
      if ((3 == (3 & thing_309X))) {
        have_310X = table_ref((Sstob_tableS), thing_309X);
        if ((NULL == have_310X)) {
          ps_error("traced object has no descriptor in image", 0);
          goto L13590;}
        else {
          goto L13590;}}
      else {
        arg0K0 = thing_309X;
        goto L13478;}}}
  else {
    thing_311X = *((long *) addr_307X);
    if ((3 == (3 & thing_311X))) {
      have_312X = table_ref((Sstob_tableS), thing_311X);
      if ((NULL == have_312X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13543;}
      else {
        goto L13543;}}
    else {
      arg0K0 = thing_311X;
      goto L13534;}}}
 L13900: {
  arg0K0 = (have_304X->new_descriptor);
  goto L13891;}
 L13891: {
  v_313X = arg0K0;
  merged_arg0K0 = v_313X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_18;
#else
  write_descriptor_return_tag = 18;
#endif
  goto write_descriptor;
 write_descriptor_return_18:
  arg1K0 = (addr_302X + 8);
  goto L13886;}
 L13478: {
  v_314X = arg0K0;
  merged_arg0K0 = v_314X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_19;
#else
  write_descriptor_return_tag = 19;
#endif
  goto write_descriptor;
 write_descriptor_return_19:
  if ((3 == (3 & next_299X))) {
    have_315X = table_ref((Sstob_tableS), next_299X);
    if ((NULL == have_315X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L13566;}
    else {
      goto L13566;}}
  else {
    arg0K0 = next_299X;
    goto L13482;}}
 L13590: {
  arg0K0 = (have_310X->new_descriptor);
  goto L13478;}
 L13543: {
  arg0K0 = (have_312X->new_descriptor);
  goto L13534;}
 L13534: {
  v_316X = arg0K0;
  merged_arg0K0 = v_316X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_20;
#else
  write_descriptor_return_tag = 20;
#endif
  goto write_descriptor;
 write_descriptor_return_20:
  arg1K0 = (addr_307X + 8);
  goto L13529;}
 L13566: {
  arg0K0 = (have_315X->new_descriptor);
  goto L13482;}
 L13482: {
  value_317X = arg0K0;
  if ((3 == (3 & value_317X))) {
    merged_arg0K0 = (-4 & value_317X);
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_21;
#else
    write_descriptor_return_tag = 21;
#endif
    goto write_descriptor;
   write_descriptor_return_21:
    goto L14031;}
  else {
    merged_arg0K0 = value_317X;
#ifdef USE_DIRECT_THREADING
    write_descriptor_return_address = &&write_descriptor_return_22;
#else
    write_descriptor_return_tag = 22;
#endif
    goto write_descriptor;
   write_descriptor_return_22:
    goto L14031;}}
 copy_image_data: {
  start_187X = merged_arg1K0;
  size_188X = merged_arg0K1;{
  memmove((void *)(Simage_buffer_pointerS), (void *)start_187X,size_188X);
  Simage_buffer_pointerS = ((Simage_buffer_pointerS) + size_188X);
  if ((4096 == ((Simage_buffer_pointerS) - (Simage_bufferS)))) {
    have_318X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_318X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_318X));
        goto L6919;}
      else {
        goto L6919;}}
    else {
#ifdef USE_DIRECT_THREADING
      goto *copy_image_data_return_address;
#else
      goto copy_image_data_return;
#endif
}}
  else {
#ifdef USE_DIRECT_THREADING
    goto *copy_image_data_return_address;
#else
    goto copy_image_data_return;
#endif
}}
 L6919: {
  Simage_buffer_pointerS = (Simage_bufferS);
#ifdef USE_DIRECT_THREADING
  goto *copy_image_data_return_address;
#else
  goto copy_image_data_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 copy_image_data_return:
  switch (copy_image_data_return_tag) {
  case 0: goto copy_image_data_return_0;
  case 1: goto copy_image_data_return_1;
  default: goto copy_image_data_return_2;
  }
#endif
}

 write_shared_table: {
  table_186X = merged_arg0K0;{
  merged_arg0K0 = (*((long *) (((char *) (-11 + table_186X)))));
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_23;
#else
  write_descriptor_return_tag = 23;
#endif
  goto write_descriptor;
 write_descriptor_return_23:
  arg0K0 = 0;
  goto L12494;}
 L12494: {
  i_319X = arg0K0;
  temp_320X = i_319X == (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-11 + table_186X))))), 8))), 3));
  if (temp_320X) {
    write_shared_table0_return_value = temp_320X;
#ifdef USE_DIRECT_THREADING
    goto *write_shared_table_return_address;
#else
    goto write_shared_table_return;
#endif
}
  else {
    link_321X = *((long *) ((((char *) (-3 + table_186X))) + (PS_SHIFT_LEFT_INLINE(i_319X, 3))));
    if ((0 == (3 & link_321X))) {
      arg0K0 = (3 + (-4 & link_321X));
      goto L12504;}
    else {
      arg0K0 = link_321X;
      goto L12504;}}}
 L12504: {
  shared_322X = arg0K0;
  arg0K0 = shared_322X;
  goto L12538;}
 L12538: {
  next_323X = arg0K0;
  if ((3 == (3 & next_323X))) {
    v_324X = table_ref((Sstob_tableS), next_323X);
    if ((NULL == v_324X)) {
      link_325X = *((long *) ((((char *) (-3 + next_323X))) + 24));
      if ((0 == (3 & link_325X))) {
        arg0K0 = (3 + (-4 & link_325X));
        goto L12538;}
      else {
        arg0K0 = link_325X;
        goto L12538;}}
    else {
      arg0K0 = next_323X;
      goto L12506;}}
  else {
    arg0K0 = next_323X;
    goto L12506;}}
 L12506: {
  thing_326X = arg0K0;
  if ((3 == (3 & thing_326X))) {
    have_327X = table_ref((Sstob_tableS), thing_326X);
    if ((NULL == have_327X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L12557;}
    else {
      goto L12557;}}
  else {
    arg0K0 = thing_326X;
    goto L12508;}}
 L12557: {
  arg0K0 = (have_327X->new_descriptor);
  goto L12508;}
 L12508: {
  value_328X = arg0K0;
  if ((3 == (3 & value_328X))) {
    arg0K0 = (-4 & value_328X);
    goto L12510;}
  else {
    arg0K0 = value_328X;
    goto L12510;}}
 L12510: {
  v_329X = arg0K0;
  merged_arg0K0 = v_329X;
#ifdef USE_DIRECT_THREADING
  write_descriptor_return_address = &&write_descriptor_return_24;
#else
  write_descriptor_return_tag = 24;
#endif
  goto write_descriptor;
 write_descriptor_return_24:
  arg0K0 = (1 + i_319X);
  goto L12494;}
#ifndef USE_DIRECT_THREADING
 write_shared_table_return:
  switch (write_shared_table_return_tag) {
  case 0: goto write_shared_table_return_0;
  default: goto write_shared_table_return_1;
  }
#endif
}

 write_descriptor: {
  descriptor_185X = merged_arg0K0;{
  *((long *) (Simage_buffer_pointerS)) = (long) (descriptor_185X);
  Simage_buffer_pointerS = ((Simage_buffer_pointerS) + 8);
  if ((4096 == ((Simage_buffer_pointerS) - (Simage_bufferS)))) {
    have_330X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_330X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_330X));
        goto L6894;}
      else {
        goto L6894;}}
    else {
#ifdef USE_DIRECT_THREADING
      goto *write_descriptor_return_address;
#else
      goto write_descriptor_return;
#endif
}}
  else {
#ifdef USE_DIRECT_THREADING
    goto *write_descriptor_return_address;
#else
    goto write_descriptor_return;
#endif
}}
 L6894: {
  Simage_buffer_pointerS = (Simage_bufferS);
#ifdef USE_DIRECT_THREADING
  goto *write_descriptor_return_address;
#else
  goto write_descriptor_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 write_descriptor_return:
  switch (write_descriptor_return_tag) {
  case 0: goto write_descriptor_return_0;
  case 1: goto write_descriptor_return_1;
  case 2: goto write_descriptor_return_2;
  case 3: goto write_descriptor_return_3;
  case 4: goto write_descriptor_return_4;
  case 5: goto write_descriptor_return_5;
  case 6: goto write_descriptor_return_6;
  case 7: goto write_descriptor_return_7;
  case 8: goto write_descriptor_return_8;
  case 9: goto write_descriptor_return_9;
  case 10: goto write_descriptor_return_10;
  case 11: goto write_descriptor_return_11;
  case 12: goto write_descriptor_return_12;
  case 13: goto write_descriptor_return_13;
  case 14: goto write_descriptor_return_14;
  case 15: goto write_descriptor_return_15;
  case 16: goto write_descriptor_return_16;
  case 17: goto write_descriptor_return_17;
  case 18: goto write_descriptor_return_18;
  case 19: goto write_descriptor_return_19;
  case 20: goto write_descriptor_return_20;
  case 21: goto write_descriptor_return_21;
  case 22: goto write_descriptor_return_22;
  case 23: goto write_descriptor_return_23;
  default: goto write_descriptor_return_24;
  }
#endif
}

}
void s48_collect(char force_majorP_331X)
{
  char * arg1K1;
  char * arg1K0;
  long arg0K0;
  long x2_352X;
  long h_351X;
  char * a_350X;
  long value_349X;
  char * scan_348X;
  char * next_347X;
  char * end_346X;
  char * start_345X;
  long end_mseconds_344X;
  long end_seconds_343X;
  char v_342X;
  char * end_341X;
  char * x_340X;
  char * end_339X;
  char * start_338X;
  char * temp_337X;
  char * temp_336X;
  char * temp_335X;
  char * temp_334X;
  long start_mseconds_333X;
  long start_seconds_332X;
 {  start_seconds_332X = s48_run_time(&start_mseconds_333X);
  Sfrom_beginS = (Snewspace_beginS);
  Sfrom_endS = (Snewspace_endS);
  temp_334X = s48_SlimitS;
  s48_SlimitS = (Soldspace_limitS);
  Soldspace_limitS = temp_334X;
  temp_335X = s48_ShpS;
  s48_ShpS = (Soldspace_hpS);
  Soldspace_hpS = temp_335X;
  temp_336X = Snewspace_beginS;
  Snewspace_beginS = (Soldspace_beginS);
  Soldspace_beginS = temp_336X;
  temp_337X = Snewspace_endS;
  Snewspace_endS = (Soldspace_endS);
  Soldspace_endS = temp_337X;
  s48_ShpS = (Snewspace_beginS);
  Sweak_pointer_hpS = NULL;s48_gc_root();
  arg1K0 = (Snewspace_beginS);
  goto L14786;}
 L14786: {
  start_338X = arg1K0;
  end_339X = s48_ShpS;s48_trace_locationsB(start_338X, end_339X);
  if (((PS_SHIFT_RIGHT_INLINE(((s48_SlimitS) - (s48_ShpS)), 3)) < 0)) {
    ps_error("GC error: ran out of space in new heap", 0);
    goto L14722;}
  else {
    if ((end_339X < (s48_ShpS))) {
      arg1K0 = end_339X;
      goto L14786;}
    else {
      goto L14722;}}}
 L14722: {
  if (((Sweak_pointer_hpS) == NULL)) {
    goto L14724;}
  else {
    x_340X = Sweak_pointer_limitS;
    end_341X = Sweak_pointer_hpS;
    arg1K0 = (x_340X + -2048);
    arg1K1 = end_341X;
    goto L10304;}}
 L14724: {
  v_342X = (PS_SHIFT_RIGHT_INLINE(((s48_SlimitS) - (s48_ShpS)), 3)) < ((PS_SHIFT_RIGHT_INLINE((7 + ((Snewspace_endS) - (Snewspace_beginS))), 3)) / 10);s48_post_gc_cleanup(1, v_342X);
  Sgc_countS = (1 + (Sgc_countS));
  end_seconds_343X = s48_run_time(&end_mseconds_344X);
  if ((end_mseconds_344X < start_mseconds_333X)) {
    Sgc_secondsS = (-1 + ((Sgc_secondsS) + (end_seconds_343X - start_seconds_332X)));
    Sgc_msecondsS = ((Sgc_msecondsS) + ((1000 + end_mseconds_344X) - start_mseconds_333X));
    return;}
  else {
    Sgc_secondsS = ((Sgc_secondsS) + (end_seconds_343X - start_seconds_332X));
    Sgc_msecondsS = ((Sgc_msecondsS) + (end_mseconds_344X - start_mseconds_333X));
    return;}}
 L10304: {
  start_345X = arg1K0;
  end_346X = arg1K1;
  next_347X = ((char *) (*((long *) (start_345X + 16))));
  arg1K0 = start_345X;
  goto L6538;}
 L6538: {
  scan_348X = arg1K0;
  if ((scan_348X < end_346X)) {
    *((long *) scan_348X) = (long) (2102);
    value_349X = *((long *) (scan_348X + 8));
    if ((3 == (3 & value_349X))) {
      a_350X = ((char *) (-3 + value_349X));
      if ((a_350X < (Sfrom_beginS))) {
        goto L6584;}
      else {
        if ((a_350X < (Sfrom_endS))) {
          if ((3 == (3 & value_349X))) {
            h_351X = *((long *) (((char *) (-11 + value_349X))));
            if ((3 == (3 & h_351X))) {
              arg0K0 = h_351X;
              goto L6579;}
            else {
              arg0K0 = 1;
              goto L6579;}}
          else {
            goto L6584;}}
        else {
          goto L6584;}}}
    else {
      goto L6584;}}
  else {
    if ((next_347X == NULL)) {
      if ((end_341X < (Sweak_pointer_limitS))) {
        *((long *) end_341X) = (long) ((70 + (PS_SHIFT_LEFT_INLINE((-8 & ((Sweak_pointer_limitS) - (end_341X + 8))), 8))));
        goto L14724;}
      else {
        goto L14724;}}
    else {
      arg1K0 = (next_347X + -2048);
      arg1K1 = next_347X;
      goto L10304;}}}
 L6584: {
  arg1K0 = (scan_348X + 16);
  goto L6538;}
 L6579: {
  x2_352X = arg0K0;
  *((long *) (scan_348X + 8)) = (long) (x2_352X);
  goto L6584;}
}
void s48_make_availableAgc(long len_353X)
{
  char x_354X;
 {  x_354X = ((s48_ShpS) + (-8 & (7 + len_353X))) < (s48_SlimitS);
  if (x_354X) {
    goto L14840;}
  else {s48_collect(0);
    goto L14840;}}
 L14840: {
  if ((((s48_ShpS) + (-8 & (7 + len_353X))) < (s48_SlimitS))) {
    return;}
  else {
    ps_error("Scheme 48 heap overflow", 0);
    return;}}
}
char * s48_allocate_tracedAgc(long len_355X)
{
  char * new_357X;
  char x_356X;
 {  x_356X = ((s48_ShpS) + (-8 & (7 + len_355X))) < (s48_SlimitS);
  if (x_356X) {
    goto L14914;}
  else {s48_collect(0);
    goto L14914;}}
 L14914: {
  if ((((s48_ShpS) + (-8 & (7 + len_355X))) < (s48_SlimitS))) {
    new_357X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + len_355X)));
    return new_357X;}
  else {
    return NULL;}}
}
char * s48_allocate_weakAgc(long len_358X)
{
  char * new_360X;
  char x_359X;
 {  x_359X = ((s48_ShpS) + (-8 & (7 + len_358X))) < (s48_SlimitS);
  if (x_359X) {
    goto L14986;}
  else {s48_collect(0);
    goto L14986;}}
 L14986: {
  if ((((s48_ShpS) + (-8 & (7 + len_358X))) < (s48_SlimitS))) {
    new_360X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + len_358X)));
    return new_360X;}
  else {
    return NULL;}}
}
char * s48_allocate_untracedAgc(long len_361X)
{
  char * new_363X;
  char x_362X;
 {  x_362X = ((s48_ShpS) + (-8 & (7 + len_361X))) < (s48_SlimitS);
  if (x_362X) {
    goto L15007;}
  else {s48_collect(0);
    goto L15007;}}
 L15007: {
  if ((((s48_ShpS) + (-8 & (7 + len_361X))) < (s48_SlimitS))) {
    new_363X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + len_361X)));
    return new_363X;}
  else {
    return NULL;}}
}
long s48_allocate_stob(long type_364X, long size_365X)
{
  long arg0K0;
  char * arg1K0;
  char * thing_373X;
  char * new_372X;
  char * new_371X;
  char x_370X;
  char x_369X;
  long needed_368X;
  long length_in_bytes_367X;
  char tracedP_366X;
 {  tracedP_366X = type_364X < 16;
  if (tracedP_366X) {
    arg0K0 = (PS_SHIFT_LEFT_INLINE(size_365X, 3));
    goto L15054;}
  else {
    arg0K0 = size_365X;
    goto L15054;}}
 L15054: {
  length_in_bytes_367X = arg0K0;
  needed_368X = 8 + length_in_bytes_367X;
  if (tracedP_366X) {
    x_369X = ((s48_ShpS) + (-8 & (7 + needed_368X))) < (s48_SlimitS);
    if (x_369X) {
      goto L15120;}
    else {s48_collect(0);
      goto L15120;}}
  else {
    x_370X = ((s48_ShpS) + (-8 & (7 + needed_368X))) < (s48_SlimitS);
    if (x_370X) {
      goto L15139;}
    else {s48_collect(0);
      goto L15139;}}}
 L15120: {
  if ((((s48_ShpS) + (-8 & (7 + needed_368X))) < (s48_SlimitS))) {
    new_371X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + needed_368X)));
    arg1K0 = new_371X;
    goto L15070;}
  else {
    arg1K0 = NULL;
    goto L15070;}}
 L15139: {
  if ((((s48_ShpS) + (-8 & (7 + needed_368X))) < (s48_SlimitS))) {
    new_372X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + needed_368X)));
    arg1K0 = new_372X;
    goto L15070;}
  else {
    arg1K0 = NULL;
    goto L15070;}}
 L15070: {
  thing_373X = arg1K0;
  if ((thing_373X == NULL)) {
    ps_error("insufficient heap space for external allocation", 0);
    goto L15082;}
  else {
    goto L15082;}}
 L15082: {
  *((long *) thing_373X) = (long) ((2 + (PS_SHIFT_LEFT_INLINE(((PS_SHIFT_LEFT_INLINE(length_in_bytes_367X, 6)) + type_364X), 2))));
  return (3 + (((long) (thing_373X + 8))));}
}
long s48_allocate_weak_stob(long type_374X, long size_375X)
{

 {  return s48_allocate_stob(type_374X, size_375X);}
}
long s48_read_image(char *image_filename_376X, long max_heap_size_377X)
{
  struct image_location *arg2K0;
  struct table *arg3K0;
  char *arg5K1;
  char arg4K1;
  char arg4K0;
  char * arg1K0;
  long arg0K2;
  long arg0K1;
  long arg0K0;
  FILE * merged_arg6K2;
  struct table *merged_arg3K1;
  char * merged_arg1K1;
  char * merged_arg1K0;
  long merged_arg0K2;
  long merged_arg0K1;
  long merged_arg0K0;

#ifdef USE_DIRECT_THREADING
  void *relocateD2_return_address;
#else
  int relocateD2_return_tag;
#endif
  long relocateD20_return_value;
#ifdef USE_DIRECT_THREADING
  void *old_Gnew_addr_return_address;
#else
  int old_Gnew_addr_return_tag;
#endif
  char * old_Gnew_addr0_return_value;
#ifdef USE_DIRECT_THREADING
  void *parse_reachable_objects_return_address;
#else
  int parse_reachable_objects_return_tag;
#endif
  long parse_reachable_objects0_return_value;
#ifdef USE_DIRECT_THREADING
  void *alloc_object_return_address;
#else
  int alloc_object_return_tag;
#endif
  char * alloc_object0_return_value;
#ifdef USE_DIRECT_THREADING
  void *allocate_table_return_address;
#else
  int allocate_table_return_tag;
#endif
  long allocate_table0_return_value;
#ifdef USE_DIRECT_THREADING
  void *relocate_binding_table_bibopB_return_address;
#else
  int relocate_binding_table_bibopB_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *relocateD0_return_address;
#else
  int relocateD0_return_tag;
#endif
  long relocateD00_return_value;
#ifdef USE_DIRECT_THREADING
  void *relocate_binding_table_two_spaceB_return_address;
#else
  int relocate_binding_table_two_spaceB_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *reverse_byte_orderB_return_address;
#else
  int reverse_byte_orderB_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *really_read_image_area_return_address;
#else
  int really_read_image_area_return_tag;
#endif
  long really_read_image_area0_return_value;
  long address_378X;
  char * addr_379X;
  long format_380X;
  char * from_addr_381X;
  char * to_addr_382X;
  long image_format_383X;
  char * current_address_384X;
  long size_in_bytes_385X;
  long tab_386X;
  long image_format_387X;
  long table_388X;
  struct table *stob_table_389X;
  long address_390X;
  long table_391X;
  long delta_392X;
  char * start_393X;
  char * end_394X;
  char * new_start_addr_395X;
  long img_heap_size_396X;
  FILE * port_397X;
  struct image_location *image_location_753X;
  long v_752X;
  char * address_751X;
  char * y_750X;
  char * y_749X;
  char * v_748X;
  char * y_747X;
  char * new_address_746X;
  struct image_location *image_location_745X;
  long v_744X;
  struct image_location *image_location_743X;
  long v_742X;
  char * index_741X;
  char * new_address_740X;
  struct image_location *v_739X;
  long v_738X;
  char * address_737X;
  char * new_address_736X;
  char * v_735X;
  char * new_address_734X;
  long size_in_cells_733X;
  struct image_location *v_732X;
  long v_731X;
  char * v_730X;
  char * new_address_729X;
  long size_in_bytes_728X;
  long size_in_cells_727X;
  long header_cell_726X;
  char * x1_725X;
  char * current_addr_724X;
  struct image_location *v_723X;
  long v_722X;
  char * current_addr_721X;
  long size_in_cells_720X;
  long cell_719X;
  char * x1_718X;
  char * current_addr_717X;
  long v_716X;
  struct image_location *val_715X;
  struct image_location *image_location_714X;
  char * new_address_713X;
  char * new_712X;
  char x_711X;
  char * pointer_710X;
  char * new_709X;
  long v_708X;
  char x_707X;
  long size_in_byte_706X;
  long size_705X;
  long cell_704X;
  char * addr_703X;
  long val_702X;
  long next_701X;
  long next_700X;
  long link_699X;
  long entry_698X;
  long value_697X;
  long bucket_696X;
  long bucket_695X;
  long link_694X;
  long i_693X;
  struct image_location *image_location_692X;
  long v_691X;
  char * address_690X;
  long val_689X;
  long next_688X;
  long next_687X;
  long link_686X;
  long entry_685X;
  long value_684X;
  long bucket_683X;
  long bucket_682X;
  long link_681X;
  long i_680X;
  char * next_679X;
  long value_678X;
  long byte_a_677X;
  char * addr_b_676X;
  char * addr_a_675X;
  long j_674X;
  long i_673X;
  char * ptr_672X;
  long status_671X;
  long status_670X;
  char *string_669X;
  char okayP_668X;
  long status_667X;
  char eofP_666X;
  long got_665X;
  long need_664X;
  long val_663X;
  long val_662X;
  long next_661X;
  long next_660X;
  long next_659X;
  long next_658X;
  struct image_location *image_location_657X;
  long v_656X;
  long i_655X;
  long link_654X;
  long entry_653X;
  long link_652X;
  long entry_651X;
  char * address_650X;
  long cell_649X;
  long size_648X;
  char * address_647X;
  struct image_location **values_646X;
  long *keys_645X;
  struct table *table_644X;
  long d_643X;
  long value_642X;
  long value_641X;
  long cell_640X;
  long resumer_records_639X;
  long expr_638X;
  long descriptor_637X;
  char * ptr_636X;
  long bucket_635X;
  long bucket_634X;
  long bucket_633X;
  long bucket_632X;
  long expr_631X;
  long v_630X;
  char * start_629X;
  long link_628X;
  long i_627X;
  long link_626X;
  long i_625X;
  long expr_624X;
  long v_623X;
  long table_622X;
  long expr_621X;
  struct table *stob_table_620X;
  long table_619X;
  long expr_618X;
  long v_617X;
  struct image_location *image_location_616X;
  long v_615X;
  long descriptor_614X;
  long expr_613X;
  char * address_612X;
  long descriptor_611X;
  long expr_610X;
  long descriptor_609X;
  long expr_608X;
  long status_607X;
  long status_606X;
  long status_605X;
  char * v_604X;
  char * v_603X;
  long expr_602X;
  long status_601X;
  long descriptor_600X;
  long expr_599X;
  long status_598X;
  long status_597X;
  long status_596X;
  long status_595X;
  long status_594X;
  long status_593X;
  char eofP_592X;
  char v_591X;
  long expr_590X;
  long status_589X;
  char *string_588X;
  char okayP_587X;
  long status_586X;
  long status_585X;
  long status_584X;
  long status_583X;
  long descriptor_582X;
  long status_581X;
  long expr_580X;
  long expr_579X;
  long expr_578X;
  long status_577X;
  char eofP_576X;
  long got_575X;
  long need_574X;
  struct table *v_573X;
  long status_572X;
  long status_571X;
  long status_570X;
  long status_569X;
  long status_568X;
  long status_567X;
  char eofP_566X;
  char v_565X;
  char *string_564X;
  char okayP_563X;
  long status_562X;
  char eofP_561X;
  long expr_560X;
  struct table *table_559X;
  long i_558X;
  long status_557X;
  char eofP_556X;
  long got_555X;
  long need_554X;
  char * address_553X;
  long cells_552X;
  long status_551X;
  char eofP_550X;
  char ch_549X;
  long we_548X;
  long cells_547X;
  long status_546X;
  long *keys_545X;
  char * v_544X;
  long expr_543X;
  long status_542X;
  char eofP_541X;
  long thing_540X;
  long status_539X;
  long wh_538X;
  long cells_537X;
  long v_536X;
  long byte_a_535X;
  char * addr_b_534X;
  char * addr_a_533X;
  long j_532X;
  long i_531X;
  long status_530X;
  long v_529X;
  char x_528X;
  long size_in_byte_527X;
  char * new_hp_526X;
  long delta_525X;
  char reverse_byte_orderP_524X;
  long status_523X;
  char eofP_522X;
  long thing_521X;
  long status_520X;
  long expr_519X;
  long status_518X;
  char eofP_517X;
  long thing_516X;
  long status_515X;
  long le_514X;
  long cells_513X;
  long status_512X;
  char eofP_511X;
  long thing_510X;
  long status_509X;
  long expr_508X;
  long status_507X;
  char eofP_506X;
  long thing_505X;
  long status_504X;
  long lh_503X;
  long cells_502X;
  long status_501X;
  char eofP_500X;
  long got_499X;
  char * new_addr_498X;
  long status_497X;
  char eofP_496X;
  long thing_495X;
  long status_494X;
  long expr_493X;
  long status_492X;
  char eofP_491X;
  long thing_490X;
  long status_489X;
  long se_488X;
  long cells_487X;
  char * temp_486X;
  char * temp_485X;
  char * temp_484X;
  char * temp_483X;
  long semisize_482X;
  long status_481X;
  char eofP_480X;
  long thing_479X;
  long status_478X;
  long expr_477X;
  long status_476X;
  char eofP_475X;
  long thing_474X;
  long status_473X;
  long sh_472X;
  long cells_471X;
  long cells_470X;
  char * heap_469X;
  long heap_size_468X;
  long status_467X;
  long status_466X;
  char eofP_465X;
  long thing_464X;
  long status_463X;
  long status_462X;
  char eofP_461X;
  long thing_460X;
  long status_459X;
  long sb_458X;
  long cells_457X;
  long status_456X;
  char eofP_455X;
  long thing_454X;
  long status_453X;
  long cells_452X;
  long minimum_size_451X;
  char * image_start_address_450X;
  long format_449X;
  long status_448X;
  long status_447X;
  long status_446X;
  long status_445X;
  long status_444X;
  long status_443X;
  char eofP_442X;
  long thing_441X;
  long status_440X;
  long status_439X;
  char eofP_438X;
  long thing_437X;
  long status_436X;
  long status_435X;
  long status_434X;
  long status_433X;
  long status_432X;
  long status_431X;
  long old_bytes_per_cell_430X;
  long status_429X;
  char eofP_428X;
  char thing_427X;
  long status_426X;
  char eofP_425X;
  long thing_424X;
  long status_423X;
  long format_422X;
  long status_421X;
  char eofP_420X;
  long status_419X;
  char eofP_418X;
  char ch_417X;
  long i_416X;
  long status_415X;
  char eofP_414X;
  long n_413X;
  long status_412X;
  char same_versionP_411X;
  long status_410X;
  char eofP_409X;
  long status_408X;
  char eofP_407X;
  char ch_406X;
  long status_405X;
  long status_404X;
  char eofP_403X;
  char ch_402X;
  long status_401X;
  long status_400X;
  long status_399X;
  FILE * port_398X;
 {  SstatusS = NO_ERRORS;
  SeofPS = 0;
  port_398X = ps_open_input_file(image_filename_376X, &status_399X);
  if ((status_399X == NO_ERRORS)) {
    status_400X = SstatusS;
    if ((status_400X == NO_ERRORS)) {
      if ((SeofPS)) {
        goto L7392;}
      else {
        goto L8915;}}
    else {
      goto L7392;}}
  else {
    ps_write_string("Can't open heap image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    ps_write_string((ps_error_string(status_399X)), (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    return -1;}}
 L7392: {
  status_401X = SstatusS;
  if ((status_401X == NO_ERRORS)) {
    if ((SeofPS)) {
      goto L7443;}
    else {
      goto L8835;}}
  else {
    goto L7443;}}
 L8915: {
  PS_READ_CHAR(port_398X, ch_402X, eofP_403X, status_404X)
  if (eofP_403X) {
    arg4K0 = eofP_403X;
    arg0K1 = status_404X;
    goto L7366;}
  else {
    if ((status_404X == NO_ERRORS)) {
      if ((12 == ch_402X)) {
        arg4K0 = 0;
        arg0K1 = status_404X;
        goto L7366;}
      else {
        goto L8915;}}
    else {
      arg4K0 = eofP_403X;
      arg0K1 = status_404X;
      goto L7366;}}}
 L7443: {
  status_405X = SstatusS;
  if ((status_405X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg4K0 = 0;
      goto L7494;}
    else {
      arg0K0 = 0;
      goto L8769;}}
  else {
    arg4K0 = 0;
    goto L7494;}}
 L8835: {
  PS_READ_CHAR(port_398X, ch_406X, eofP_407X, status_408X)
  if (eofP_407X) {
    arg4K0 = eofP_407X;
    arg0K1 = status_408X;
    goto L7417;}
  else {
    if ((status_408X == NO_ERRORS)) {
      if ((10 == ch_406X)) {
        arg4K0 = 0;
        arg0K1 = status_408X;
        goto L7417;}
      else {
        goto L8835;}}
    else {
      arg4K0 = eofP_407X;
      arg0K1 = status_408X;
      goto L7417;}}}
 L7366: {
  eofP_409X = arg4K0;
  status_410X = arg0K1;
  if (eofP_409X) {
    SeofPS = 1;
    goto L7392;}
  else {
    if ((status_410X == NO_ERRORS)) {
      goto L7392;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_410X;
        goto L7392;}
      else {
        goto L7392;}}}}
 L7494: {
  same_versionP_411X = arg4K0;
  status_412X = SstatusS;
  if ((status_412X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7547;}
    else {
      PS_READ_INTEGER(port_398X, n_413X, eofP_414X, status_415X)
      if (eofP_414X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7547;}
      else {
        if ((status_415X == NO_ERRORS)) {
          arg0K0 = n_413X;
          goto L7547;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_415X;
            arg0K0 = -1;
            goto L7547;}
          else {
            arg0K0 = -1;
            goto L7547;}}}}}
  else {
    arg0K0 = -1;
    goto L7547;}}
 L8769: {
  i_416X = arg0K0;
  PS_READ_CHAR(port_398X, ch_417X, eofP_418X, status_419X)
  if (eofP_418X) {
    arg4K0 = 0;
    arg4K1 = eofP_418X;
    arg0K2 = status_419X;
    goto L7468;}
  else {
    if ((status_419X == NO_ERRORS)) {
      if ((i_416X == (strlen((char *) "Vanilla 40")))) {
        arg4K0 = (10 == ch_417X);
        arg4K1 = 0;
        arg0K2 = status_419X;
        goto L7468;}
      else {
        if ((ch_417X == (*("Vanilla 40" + i_416X)))) {
          arg0K0 = (1 + i_416X);
          goto L8769;}
        else {
          arg4K0 = 0;
          arg4K1 = 0;
          arg0K2 = status_419X;
          goto L7468;}}}
    else {
      arg4K0 = 0;
      arg4K1 = eofP_418X;
      arg0K2 = status_419X;
      goto L7468;}}}
 L7417: {
  eofP_420X = arg4K0;
  status_421X = arg0K1;
  if (eofP_420X) {
    SeofPS = 1;
    goto L7443;}
  else {
    if ((status_421X == NO_ERRORS)) {
      goto L7443;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_421X;
        goto L7443;}
      else {
        goto L7443;}}}}
 L7547: {
  format_422X = arg0K0;
  status_423X = SstatusS;
  if ((status_423X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7600;}
    else {
      PS_READ_INTEGER(port_398X, thing_424X, eofP_425X, status_426X)
      if (eofP_425X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7600;}
      else {
        if ((status_426X == NO_ERRORS)) {
          arg0K0 = thing_424X;
          goto L7600;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_426X;
            arg0K0 = -1;
            goto L7600;}
          else {
            arg0K0 = -1;
            goto L7600;}}}}}
  else {
    arg0K0 = -1;
    goto L7600;}}
 L7468: {
  thing_427X = arg4K0;
  eofP_428X = arg4K1;
  status_429X = arg0K2;
  if (eofP_428X) {
    SeofPS = 1;
    arg4K0 = 0;
    goto L7494;}
  else {
    if ((status_429X == NO_ERRORS)) {
      arg4K0 = thing_427X;
      goto L7494;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_429X;
        arg4K0 = 0;
        goto L7494;}
      else {
        arg4K0 = 0;
        goto L7494;}}}}
 L7600: {
  old_bytes_per_cell_430X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    if ((SeofPS)) {
      SstatusS = EDOM;
      ps_write_string("Premature EOF when reading image file", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_431X = SstatusS;
      if ((status_431X == NO_ERRORS)) {
        goto L8048;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L8048;}}
    else {
      if (same_versionP_411X) {
        if ((0 == format_422X)) {
          goto L8112;}
        else {
          if ((1 == format_422X)) {
            goto L8112;}
          else {
            ps_write_string("Unknown image format", (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            status_432X = SstatusS;
            if ((status_432X == NO_ERRORS)) {
              goto L8123;}
            else {
              ps_write_string((ps_error_string((SstatusS))), (stderr));
              { long ignoreXX;
              PS_WRITE_CHAR(10, (stderr), ignoreXX) }
              goto L8123;}}}}
      else {
        ps_write_string("Format of image is incompatible with this version of system", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        status_433X = SstatusS;
        if ((status_433X == NO_ERRORS)) {
          goto L8082;}
        else {
          ps_write_string((ps_error_string((SstatusS))), (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          goto L8082;}}}}
  else {
    ps_write_string("Error reading from image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_434X = SstatusS;
    if ((status_434X == NO_ERRORS)) {
      goto L8013;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L8013;}}}
 L8048: {
  status_435X = ps_close(port_398X);
  if ((status_435X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}}
 L8112: {
  if ((8 == old_bytes_per_cell_430X)) {
    if ((0 == format_422X)) {
      status_436X = SstatusS;
      if ((status_436X == NO_ERRORS)) {
        if ((SeofPS)) {
          arg0K0 = -1;
          goto L4995;}
        else {
          PS_READ_INTEGER(port_398X, thing_437X, eofP_438X, status_439X)
          if (eofP_438X) {
            SeofPS = 1;
            arg0K0 = -1;
            goto L4995;}
          else {
            if ((status_439X == NO_ERRORS)) {
              arg0K0 = thing_437X;
              goto L4995;}
            else {
              SeofPS = 1;
              if (((SstatusS) == NO_ERRORS)) {
                SstatusS = status_439X;
                arg0K0 = -1;
                goto L4995;}
              else {
                arg0K0 = -1;
                goto L4995;}}}}}
      else {
        arg0K0 = -1;
        goto L4995;}}
    else {
      if ((1 == format_422X)) {
        status_440X = SstatusS;
        if ((status_440X == NO_ERRORS)) {
          if ((SeofPS)) {
            arg0K0 = -1;
            goto L5113;}
          else {
            PS_READ_INTEGER(port_398X, thing_441X, eofP_442X, status_443X)
            if (eofP_442X) {
              SeofPS = 1;
              arg0K0 = -1;
              goto L5113;}
            else {
              if ((status_443X == NO_ERRORS)) {
                arg0K0 = thing_441X;
                goto L5113;}
              else {
                SeofPS = 1;
                if (((SstatusS) == NO_ERRORS)) {
                  SstatusS = status_443X;
                  arg0K0 = -1;
                  goto L5113;}
                else {
                  arg0K0 = -1;
                  goto L5113;}}}}}
        else {
          arg0K0 = -1;
          goto L5113;}}
      else {
        ps_error("check-all-data!: Unknown image format (this can't happen)", 0);
        goto L7642;}}}
  else {
    ps_write_string("Incompatible bytes-per-cell in image", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_444X = SstatusS;
    if ((status_444X == NO_ERRORS)) {
      goto L8157;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L8157;}}}
 L8123: {
  status_445X = ps_close(port_398X);
  if ((status_445X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}}
 L8082: {
  status_446X = ps_close(port_398X);
  if ((status_446X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}}
 L8013: {
  status_447X = ps_close(port_398X);
  if ((status_447X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}}
 L16783: {
  status_448X = arg0K0;
  format_449X = arg0K1;
  if ((0 == status_448X)) {
    if (((SstatusS) == NO_ERRORS)) {
      image_start_address_450X = Simg_start_addrS;
      minimum_size_451X = PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 2);
      if ((max_heap_size_377X < minimum_size_451X)) {
        ps_write_string("Heap size ", (stderr));
        ps_write_integer(max_heap_size_377X, (stderr));
        ps_write_string(" is too small, using ", (stderr));
        ps_write_integer(minimum_size_451X, (stderr));
        ps_write_string(" cells", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        arg0K0 = minimum_size_451X;
        goto L2325;}
      else {
        arg0K0 = max_heap_size_377X;
        goto L2325;}}
    else {
      return -1;}}
  else {
    return -1;}}
 L4995: {
  cells_452X = arg0K0;
  Simg_start_addrS = (((char *) (PS_SHIFT_LEFT_INLINE(cells_452X, 3))));
  status_453X = SstatusS;
  if ((status_453X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5050;}
    else {
      PS_READ_INTEGER(port_398X, thing_454X, eofP_455X, status_456X)
      if (eofP_455X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5050;}
      else {
        if ((status_456X == NO_ERRORS)) {
          arg0K0 = thing_454X;
          goto L5050;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_456X;
            arg0K0 = -1;
            goto L5050;}
          else {
            arg0K0 = -1;
            goto L5050;}}}}}
  else {
    arg0K0 = -1;
    goto L5050;}}
 L5113: {
  cells_457X = arg0K0;
  sb_458X = PS_SHIFT_LEFT_INLINE(cells_457X, 3);
  status_459X = SstatusS;
  if ((status_459X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5168;}
    else {
      PS_READ_INTEGER(port_398X, thing_460X, eofP_461X, status_462X)
      if (eofP_461X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5168;}
      else {
        if ((status_462X == NO_ERRORS)) {
          arg0K0 = thing_460X;
          goto L5168;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_462X;
            arg0K0 = -1;
            goto L5168;}
          else {
            arg0K0 = -1;
            goto L5168;}}}}}
  else {
    arg0K0 = -1;
    goto L5168;}}
 L7642: {
  status_463X = SstatusS;
  if ((status_463X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7693;}
    else {
      PS_READ_INTEGER(port_398X, thing_464X, eofP_465X, status_466X)
      if (eofP_465X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7693;}
      else {
        if ((status_466X == NO_ERRORS)) {
          arg0K0 = thing_464X;
          goto L7693;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_466X;
            arg0K0 = -1;
            goto L7693;}
          else {
            arg0K0 = -1;
            goto L7693;}}}}}
  else {
    arg0K0 = -1;
    goto L7693;}}
 L8157: {
  status_467X = ps_close(port_398X);
  if ((status_467X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_422X;
    goto L16783;}}
 L2325: {
  heap_size_468X = arg0K0;
  heap_469X = (char *)malloc((PS_SHIFT_LEFT_INLINE(heap_size_468X, 4)));
  if ((heap_469X == NULL)) {
    ps_error("unable to allocate heap space", 0);
    goto L2341;}
  else {
    goto L2341;}}
 L5050: {
  cells_470X = arg0K0;
  Simg_end_addrS = (((char *) (PS_SHIFT_LEFT_INLINE(cells_470X, 3))));
  Simg_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((7 + ((Simg_end_addrS) - (Simg_start_addrS))), 3));
  goto L7642;}
 L5168: {
  cells_471X = arg0K0;
  sh_472X = PS_SHIFT_LEFT_INLINE(cells_471X, 3);
  status_473X = SstatusS;
  if ((status_473X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5223;}
    else {
      PS_READ_INTEGER(port_398X, thing_474X, eofP_475X, status_476X)
      if (eofP_475X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5223;}
      else {
        if ((status_476X == NO_ERRORS)) {
          arg0K0 = thing_474X;
          goto L5223;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_476X;
            arg0K0 = -1;
            goto L5223;}
          else {
            arg0K0 = -1;
            goto L5223;}}}}}
  else {
    arg0K0 = -1;
    goto L5223;}}
 L7693: {
  expr_477X = arg0K0;
  SsymbolsS = expr_477X;
  status_478X = SstatusS;
  if ((status_478X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7746;}
    else {
      PS_READ_INTEGER(port_398X, thing_479X, eofP_480X, status_481X)
      if (eofP_480X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7746;}
      else {
        if ((status_481X == NO_ERRORS)) {
          arg0K0 = thing_479X;
          goto L7746;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_481X;
            arg0K0 = -1;
            goto L7746;}
          else {
            arg0K0 = -1;
            goto L7746;}}}}}
  else {
    arg0K0 = -1;
    goto L7746;}}
 L2341: {
  semisize_482X = PS_SHIFT_LEFT_INLINE(heap_size_468X, 3);
  Snewspace_beginS = heap_469X;
  Snewspace_endS = ((Snewspace_beginS) + semisize_482X);
  Soldspace_beginS = (Snewspace_endS);
  Soldspace_endS = ((Soldspace_beginS) + semisize_482X);
  if (((Soldspace_beginS) == image_start_address_450X)) {
    temp_483X = s48_SlimitS;
    s48_SlimitS = (Soldspace_limitS);
    Soldspace_limitS = temp_483X;
    temp_484X = s48_ShpS;
    s48_ShpS = (Soldspace_hpS);
    Soldspace_hpS = temp_484X;
    temp_485X = Snewspace_beginS;
    Snewspace_beginS = (Soldspace_beginS);
    Soldspace_beginS = temp_485X;
    temp_486X = Snewspace_endS;
    Snewspace_endS = (Soldspace_endS);
    Soldspace_endS = temp_486X;
    goto L2365;}
  else {
    goto L2365;}}
 L5223: {
  cells_487X = arg0K0;
  se_488X = PS_SHIFT_LEFT_INLINE(cells_487X, 3);
  status_489X = SstatusS;
  if ((status_489X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5280;}
    else {
      PS_READ_INTEGER(port_398X, thing_490X, eofP_491X, status_492X)
      if (eofP_491X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5280;}
      else {
        if ((status_492X == NO_ERRORS)) {
          arg0K0 = thing_490X;
          goto L5280;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_492X;
            arg0K0 = -1;
            goto L5280;}
          else {
            arg0K0 = -1;
            goto L5280;}}}}}
  else {
    arg0K0 = -1;
    goto L5280;}}
 L7746: {
  expr_493X = arg0K0;
  Simported_bindingsS = expr_493X;
  status_494X = SstatusS;
  if ((status_494X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7799;}
    else {
      PS_READ_INTEGER(port_398X, thing_495X, eofP_496X, status_497X)
      if (eofP_496X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7799;}
      else {
        if ((status_497X == NO_ERRORS)) {
          arg0K0 = thing_495X;
          goto L7799;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_497X;
            arg0K0 = -1;
            goto L7799;}
          else {
            arg0K0 = -1;
            goto L7799;}}}}}
  else {
    arg0K0 = -1;
    goto L7799;}}
 L2365: {
  Soldspace_hpS = (Soldspace_beginS);
  Soldspace_limitS = (Soldspace_endS);
  s48_ShpS = (Snewspace_beginS);
  s48_SlimitS = (Snewspace_endS);
  Snew_heap_start_addrS = (Snewspace_beginS);
  if (((SstatusS) == NO_ERRORS)) {
    new_addr_498X = (char *)malloc(8);
    got_499X = ps_read_block(port_398X, ((char *) new_addr_498X), 8, &eofP_500X, &status_501X);
    if ((status_501X == NO_ERRORS)) {
      if (eofP_500X) {
        goto L4812;}
      else {
        if ((got_499X < 8)) {
          goto L4812;}
        else {
          goto L4764;}}}
    else {
      SstatusS = status_501X;
      goto L4812;}}
  else {
    return -1;}}
 L5280: {
  cells_502X = arg0K0;
  lh_503X = PS_SHIFT_LEFT_INLINE(cells_502X, 3);
  status_504X = SstatusS;
  if ((status_504X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5335;}
    else {
      PS_READ_INTEGER(port_398X, thing_505X, eofP_506X, status_507X)
      if (eofP_506X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5335;}
      else {
        if ((status_507X == NO_ERRORS)) {
          arg0K0 = thing_505X;
          goto L5335;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_507X;
            arg0K0 = -1;
            goto L5335;}
          else {
            arg0K0 = -1;
            goto L5335;}}}}}
  else {
    arg0K0 = -1;
    goto L5335;}}
 L7799: {
  expr_508X = arg0K0;
  Sexported_bindingsS = expr_508X;
  status_509X = SstatusS;
  if ((status_509X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7852;}
    else {
      PS_READ_INTEGER(port_398X, thing_510X, eofP_511X, status_512X)
      if (eofP_511X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7852;}
      else {
        if ((status_512X == NO_ERRORS)) {
          arg0K0 = thing_510X;
          goto L7852;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_512X;
            arg0K0 = -1;
            goto L7852;}
          else {
            arg0K0 = -1;
            goto L7852;}}}}}
  else {
    arg0K0 = -1;
    goto L7852;}}
 L4812: {
  ps_error("byte order check failed", 0);
  goto L4764;}
 L4764: {
  if ((1 == (*((long *) new_addr_498X)))) {
    free(new_addr_498X);
    arg4K0 = 0;
    goto L16851;}
  else {
    arg0K0 = 0;
    arg0K1 = 7;
    goto L4825;}}
 L5335: {
  cells_513X = arg0K0;
  le_514X = PS_SHIFT_LEFT_INLINE(cells_513X, 3);
  status_515X = SstatusS;
  if ((status_515X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5392;}
    else {
      PS_READ_INTEGER(port_398X, thing_516X, eofP_517X, status_518X)
      if (eofP_517X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5392;}
      else {
        if ((status_518X == NO_ERRORS)) {
          arg0K0 = thing_516X;
          goto L5392;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_518X;
            arg0K0 = -1;
            goto L5392;}
          else {
            arg0K0 = -1;
            goto L5392;}}}}}
  else {
    arg0K0 = -1;
    goto L5392;}}
 L7852: {
  expr_519X = arg0K0;
  Sresumer_recordsS = expr_519X;
  status_520X = SstatusS;
  if ((status_520X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L7905;}
    else {
      PS_READ_INTEGER(port_398X, thing_521X, eofP_522X, status_523X)
      if (eofP_522X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L7905;}
      else {
        if ((status_523X == NO_ERRORS)) {
          arg0K0 = thing_521X;
          goto L7905;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_523X;
            arg0K0 = -1;
            goto L7905;}
          else {
            arg0K0 = -1;
            goto L7905;}}}}}
  else {
    arg0K0 = -1;
    goto L7905;}}
 L16851: {
  reverse_byte_orderP_524X = arg4K0;
  if ((0 == format_449X)) {
    delta_525X = (Snew_heap_start_addrS) - (Simg_start_addrS);
    new_hp_526X = (Simg_end_addrS) + delta_525X;
    size_in_byte_527X = PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 3);
    x_528X = ((s48_ShpS) + (-8 & (7 + size_in_byte_527X))) < (s48_SlimitS);
    if (x_528X) {
      goto L15427;}
    else {s48_collect(0);
      goto L15427;}}
  else {
    if ((1 == format_449X)) {
      Sheap_image_pointerS = NULL;
      Ssymbol_addressS = NULL;
      v_529X = SsymbolsS;
      if ((1 == v_529X)) {
        arg1K0 = (Simg_end_addrS);
        goto L7112;}
      else {
        arg1K0 = (((char *) (-11 + (SsymbolsS))));
        goto L7112;}}
    else {
      ps_write_string("unknown image format", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_530X = SstatusS;
      if ((status_530X == NO_ERRORS)) {
        goto L16728;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L16728;}}}}
 L4825: {
  i_531X = arg0K0;
  j_532X = arg0K1;
  if ((i_531X < j_532X)) {
    addr_a_533X = new_addr_498X + i_531X;
    addr_b_534X = new_addr_498X + j_532X;
    byte_a_535X = *((unsigned char *) addr_a_533X);
    *((unsigned char *) addr_a_533X) = (unsigned char) ((*((unsigned char *) addr_b_534X)));
    *((unsigned char *) addr_b_534X) = (unsigned char) (byte_a_535X);
    arg0K0 = (1 + i_531X);
    arg0K1 = (-1 + j_532X);
    goto L4825;}
  else {
    v_536X = *((long *) new_addr_498X);
    if ((1 == v_536X)) {
      goto L4784;}
    else {
      ps_error("Unable to correct byte order", 0);
      goto L4784;}}}
 L5392: {
  cells_537X = arg0K0;
  wh_538X = PS_SHIFT_LEFT_INLINE(cells_537X, 3);
  status_539X = SstatusS;
  if ((status_539X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5447;}
    else {
      PS_READ_INTEGER(port_398X, thing_540X, eofP_541X, status_542X)
      if (eofP_541X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5447;}
      else {
        if ((status_542X == NO_ERRORS)) {
          arg0K0 = thing_540X;
          goto L5447;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_542X;
            arg0K0 = -1;
            goto L5447;}
          else {
            arg0K0 = -1;
            goto L5447;}}}}}
  else {
    arg0K0 = -1;
    goto L5447;}}
 L7905: {
  expr_543X = arg0K0;
  Sstartup_procedureS = expr_543X;
  if (((SstatusS) == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = 0;
      arg0K1 = format_422X;
      goto L16783;}
    else {
      goto L8244;}}
  else {
    arg0K0 = 0;
    arg0K1 = format_422X;
    goto L16783;}}
 L15427: {
  if ((((s48_ShpS) + (-8 & (7 + size_in_byte_527X))) < (s48_SlimitS))) {
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + size_in_byte_527X)));
    goto L15306;}
  else {
    goto L15306;}}
 L7112: {
  v_544X = arg1K0;
  Ssymbol_addressS = v_544X;
  Sheap_object_remaining_cellsS = 0;
  Sheap_object_pointerS = NULL;
  keys_545X = (long*)malloc(sizeof(long) * 4097);
  arg0K0 = 0;
  goto L7135;}
 L16728: {
  status_546X = ps_close(port_398X);
  if ((status_546X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16853;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16853;}}
 L4784: {
  free(new_addr_498X);
  arg4K0 = 1;
  goto L16851;}
 L5447: {
  cells_547X = arg0K0;
  we_548X = PS_SHIFT_LEFT_INLINE(cells_547X, 3);
  Ssmall_img_start_addrS = (((char *) sb_458X));
  Ssmall_img_hp_addrS = (((char *) sh_472X));
  Ssmall_img_end_addrS = (((char *) se_488X));
  Slarge_img_start_addrS = (((char *) se_488X));
  Slarge_img_hp_addrS = (((char *) lh_503X));
  Slarge_img_end_addrS = (((char *) le_514X));
  Sweaks_img_start_addrS = (((char *) le_514X));
  Sweaks_img_hp_addrS = (((char *) wh_538X));
  Sweaks_img_end_addrS = (((char *) we_548X));
  Ssmall_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((sh_472X - sb_458X), 3));
  Slarge_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((lh_503X - se_488X), 3));
  Sweaks_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((wh_538X - le_514X), 3));
  Simg_start_addrS = (((char *) le_514X));
  Simg_end_addrS = (((char *) sh_472X));
  Simg_heap_sizeS = (((PS_SHIFT_RIGHT_INLINE((se_488X - sb_458X), 3)) + (PS_SHIFT_RIGHT_INLINE((le_514X - se_488X), 3))) + (PS_SHIFT_RIGHT_INLINE((we_548X - le_514X), 3)));
  goto L7642;}
 L8244: {
  PS_READ_CHAR(port_398X, ch_549X, eofP_550X, status_551X)
  if (eofP_550X) {
    arg4K0 = eofP_550X;
    arg0K1 = status_551X;
    goto L7932;}
  else {
    if ((status_551X == NO_ERRORS)) {
      if ((12 == ch_549X)) {
        arg4K0 = 0;
        arg0K1 = status_551X;
        goto L7932;}
      else {
        goto L8244;}}
    else {
      arg4K0 = eofP_550X;
      arg0K1 = status_551X;
      goto L7932;}}}
 L15306: {
  cells_552X = Simg_heap_sizeS;
  address_553X = Snew_heap_start_addrS;
  need_554X = PS_SHIFT_LEFT_INLINE(cells_552X, 3);
  got_555X = ps_read_block(port_398X, ((char *) address_553X), need_554X, &eofP_556X, &status_557X);
  if ((status_557X == NO_ERRORS)) {
    if (eofP_556X) {
      arg4K0 = 0;
      arg5K1 = "Premature EOF when reading image file";
      goto L15316;}
    else {
      if ((got_555X < need_554X)) {
        arg4K0 = 0;
        arg5K1 = "Read returned too few bytes";
        goto L15316;}
      else {
        arg4K0 = 1;
        arg5K1 = "";
        goto L15316;}}}
  else {
    SstatusS = status_557X;
    arg4K0 = 0;
    arg5K1 = "Error reading from image file";
    goto L15316;}}
 L7135: {
  i_558X = arg0K0;
  if ((i_558X < 4097)) {
    *(keys_545X + i_558X) = 0;
    arg0K0 = (1 + i_558X);
    goto L7135;}
  else {
    table_559X = (struct table*)malloc(sizeof(struct table));
    if ((NULL == table_559X)) {
      arg3K0 = table_559X;
      goto L7123;}
    else {
      table_559X->keys = keys_545X;
      table_559X->values = ((struct image_location**)malloc(sizeof(struct image_location*) * 4096));
      table_559X->count = 0;
      table_559X->size = 4096;
      arg3K0 = table_559X;
      goto L7123;}}}
 L16853: {
  expr_560X = arg0K0;
  if ((0 == expr_560X)) {
    if (((SstatusS) == NO_ERRORS)) {
      return 0;}
    else {
      return -1;}}
  else {
    return -1;}}
 L7932: {
  eofP_561X = arg4K0;
  status_562X = arg0K1;
  if (eofP_561X) {
    SeofPS = 1;
    arg0K0 = 0;
    arg0K1 = format_422X;
    goto L16783;}
  else {
    if ((status_562X == NO_ERRORS)) {
      arg0K0 = 0;
      arg0K1 = format_422X;
      goto L16783;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_562X;
        arg0K0 = 0;
        arg0K1 = format_422X;
        goto L16783;}
      else {
        arg0K0 = 0;
        arg0K1 = format_422X;
        goto L16783;}}}}
 L15316: {
  okayP_563X = arg4K0;
  string_564X = arg5K1;
  PS_READ_CHAR(port_398X, v_565X, eofP_566X, status_567X)
  if (okayP_563X) {
    if ((status_567X == NO_ERRORS)) {
      if (eofP_566X) {
        status_568X = ps_close(port_398X);
        if ((status_568X == NO_ERRORS)) {
          if (reverse_byte_orderP_524X) {
            merged_arg1K0 = (Snew_heap_start_addrS);
            merged_arg1K1 = new_hp_526X;
#ifdef USE_DIRECT_THREADING
            reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_0;
#else
            reverse_byte_orderB_return_tag = 0;
#endif
            goto reverse_byte_orderB;
           reverse_byte_orderB_return_0:
            goto L15356;}
          else {
            goto L15356;}}
        else {
          ps_write_string("Error closing image file", (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          status_569X = SstatusS;
          if ((status_569X == NO_ERRORS)) {
            goto L15573;}
          else {
            ps_write_string((ps_error_string((SstatusS))), (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            goto L15573;}}}
      else {
        ps_write_string("Image file has extraneous data after image", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        status_570X = SstatusS;
        if ((status_570X == NO_ERRORS)) {
          goto L15541;}
        else {
          ps_write_string((ps_error_string((SstatusS))), (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          goto L15541;}}}
    else {
      ps_write_string("Error reading from image file", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_571X = SstatusS;
      if ((status_571X == NO_ERRORS)) {
        goto L15509;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L15509;}}}
  else {
    ps_write_string(string_564X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_572X = SstatusS;
    if ((status_572X == NO_ERRORS)) {
      goto L15477;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L15477;}}}
 L7123: {
  v_573X = arg3K0;
  Sstob_tableS = v_573X;
  Sheap_image_pointerS = ((char *)malloc((PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 3))));
  if ((0 == format_449X)) {
    need_574X = PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 3);
    got_575X = ps_read_block(port_398X, ((char *) (Sheap_image_pointerS)), need_574X, &eofP_576X, &status_577X);
    if ((status_577X == NO_ERRORS)) {
      if (eofP_576X) {
        arg4K0 = 0;
        arg5K1 = "Premature EOF when reading image file";
        goto L9551;}
      else {
        if ((got_575X < need_574X)) {
          arg4K0 = 0;
          arg5K1 = "Read returned too few bytes";
          goto L9551;}
        else {
          arg4K0 = 1;
          arg5K1 = "";
          goto L9551;}}}
    else {
      SstatusS = status_577X;
      arg4K0 = 0;
      arg5K1 = "Error reading from image file";
      goto L9551;}}
  else {
    if ((1 == format_449X)) {
      merged_arg1K0 = (Sheap_image_pointerS);
      merged_arg0K1 = (Sweaks_img_heap_sizeS);
      merged_arg6K2 = port_398X;
#ifdef USE_DIRECT_THREADING
      really_read_image_area_return_address = &&really_read_image_area_return_0;
#else
      really_read_image_area_return_tag = 0;
#endif
      goto really_read_image_area;
     really_read_image_area_return_0:
      expr_578X = really_read_image_area0_return_value;
      if ((0 == expr_578X)) {
        if (((SstatusS) == NO_ERRORS)) {
          merged_arg1K0 = ((Sheap_image_pointerS) + ((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS)));
          merged_arg0K1 = (Slarge_img_heap_sizeS);
          merged_arg6K2 = port_398X;
#ifdef USE_DIRECT_THREADING
          really_read_image_area_return_address = &&really_read_image_area_return_1;
#else
          really_read_image_area_return_tag = 1;
#endif
          goto really_read_image_area;
         really_read_image_area_return_1:
          expr_579X = really_read_image_area0_return_value;
          if ((0 == expr_579X)) {
            if (((SstatusS) == NO_ERRORS)) {
              merged_arg1K0 = ((Sheap_image_pointerS) + ((Sweaks_img_end_addrS) - (Slarge_img_start_addrS)));
              merged_arg0K1 = (Ssmall_img_heap_sizeS);
              merged_arg6K2 = port_398X;
#ifdef USE_DIRECT_THREADING
              really_read_image_area_return_address = &&really_read_image_area_return_2;
#else
              really_read_image_area_return_tag = 2;
#endif
              goto really_read_image_area;
             really_read_image_area_return_2:
              expr_580X = really_read_image_area0_return_value;
              if ((0 == expr_580X)) {
                if (((SstatusS) == NO_ERRORS)) {
                  arg0K0 = 0;
                  goto L9584;}
                else {
                  arg0K0 = -1;
                  goto L9584;}}
              else {
                arg0K0 = -1;
                goto L9584;}}
            else {
              arg0K0 = -1;
              goto L9584;}}
          else {
            arg0K0 = -1;
            goto L9584;}}
        else {
          arg0K0 = -1;
          goto L9584;}}
      else {
        arg0K0 = -1;
        goto L9584;}}
    else {
      ps_write_string("this can't happen: invalid image format", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_581X = SstatusS;
      if ((status_581X == NO_ERRORS)) {
        goto L9954;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L9954;}}}}
 L15356: {
  if ((0 == delta_525X)) {
    arg0K0 = 0;
    goto L16853;}
  else {
    descriptor_582X = Sstartup_procedureS;
    if ((3 == (3 & descriptor_582X))) {
      arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_582X))) + delta_525X))));
      goto L15365;}
    else {
      arg0K0 = descriptor_582X;
      goto L15365;}}}
 L15573: {
  status_583X = ps_close(port_398X);
  if ((status_583X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16853;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16853;}}
 L15541: {
  status_584X = ps_close(port_398X);
  if ((status_584X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16853;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16853;}}
 L15509: {
  status_585X = ps_close(port_398X);
  if ((status_585X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16853;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16853;}}
 L15477: {
  status_586X = ps_close(port_398X);
  if ((status_586X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16853;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16853;}}
 L9551: {
  okayP_587X = arg4K0;
  string_588X = arg5K1;
  if (okayP_587X) {
    arg0K0 = 0;
    goto L9584;}
  else {
    ps_write_string(string_588X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_589X = SstatusS;
    if ((status_589X == NO_ERRORS)) {
      goto L9819;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L9819;}}}
 L9584: {
  expr_590X = arg0K0;
  if ((0 == expr_590X)) {
    if (((SstatusS) == NO_ERRORS)) {
      PS_READ_CHAR(port_398X, v_591X, eofP_592X, status_593X)
      if ((status_593X == NO_ERRORS)) {
        if (eofP_592X) {
          status_594X = ps_close(port_398X);
          if ((status_594X == NO_ERRORS)) {
            arg0K0 = 0;
            goto L16622;}
          else {
            ps_write_string("Error closing image file", (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            status_595X = SstatusS;
            if ((status_595X == NO_ERRORS)) {
              goto L9702;}
            else {
              ps_write_string((ps_error_string((SstatusS))), (stderr));
              { long ignoreXX;
              PS_WRITE_CHAR(10, (stderr), ignoreXX) }
              goto L9702;}}}
        else {
          ps_write_string("Image file has extraneous data after image", (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          status_596X = SstatusS;
          if ((status_596X == NO_ERRORS)) {
            goto L9670;}
          else {
            ps_write_string((ps_error_string((SstatusS))), (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            goto L9670;}}}
      else {
        ps_write_string("Error reading from image file", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        status_597X = SstatusS;
        if ((status_597X == NO_ERRORS)) {
          goto L9638;}
        else {
          ps_write_string((ps_error_string((SstatusS))), (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          goto L9638;}}}
    else {
      arg0K0 = -1;
      goto L16622;}}
  else {
    arg0K0 = -1;
    goto L16622;}}
 L9954: {
  status_598X = ps_close(port_398X);
  if ((status_598X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L9584;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L9584;}}
 L15365: {
  expr_599X = arg0K0;
  Sstartup_procedureS = expr_599X;
  descriptor_600X = SsymbolsS;
  if ((3 == (3 & descriptor_600X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_600X))) + delta_525X))));
    goto L15371;}
  else {
    arg0K0 = descriptor_600X;
    goto L15371;}}
 L9819: {
  status_601X = ps_close(port_398X);
  if ((status_601X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L9584;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L9584;}}
 L16622: {
  expr_602X = arg0K0;
  if ((0 == expr_602X)) {
    if (((SstatusS) == NO_ERRORS)) {
      if (reverse_byte_orderP_524X) {
        merged_arg1K0 = (Simg_start_addrS);
        merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
        old_Gnew_addr_return_address = &&old_Gnew_addr_return_0;
#else
        old_Gnew_addr_return_tag = 0;
#endif
        goto old_Gnew_addr;
       old_Gnew_addr_return_0:
        v_603X = old_Gnew_addr0_return_value;
        merged_arg1K0 = (Simg_end_addrS);
        merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
        old_Gnew_addr_return_address = &&old_Gnew_addr_return_1;
#else
        old_Gnew_addr_return_tag = 1;
#endif
        goto old_Gnew_addr;
       old_Gnew_addr_return_1:
        v_604X = old_Gnew_addr0_return_value;
        merged_arg1K0 = v_603X;
        merged_arg1K1 = v_604X;
#ifdef USE_DIRECT_THREADING
        reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_1;
#else
        reverse_byte_orderB_return_tag = 1;
#endif
        goto reverse_byte_orderB;
       reverse_byte_orderB_return_1:
        goto L16634;}
      else {
        goto L16634;}}
    else {
      arg0K0 = -1;
      goto L16853;}}
  else {
    arg0K0 = -1;
    goto L16853;}}
 L9702: {
  status_605X = ps_close(port_398X);
  if ((status_605X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16622;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16622;}}
 L9670: {
  status_606X = ps_close(port_398X);
  if ((status_606X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16622;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16622;}}
 L9638: {
  status_607X = ps_close(port_398X);
  if ((status_607X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L16622;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L16622;}}
 L15371: {
  expr_608X = arg0K0;
  SsymbolsS = expr_608X;
  descriptor_609X = Simported_bindingsS;
  if ((3 == (3 & descriptor_609X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_609X))) + delta_525X))));
    goto L15377;}
  else {
    arg0K0 = descriptor_609X;
    goto L15377;}}
 L16634: {
  if ((0 == format_449X)) {
    merged_arg1K0 = (Simg_start_addrS);
    merged_arg1K1 = (Ssymbol_addressS);
    merged_arg0K2 = format_449X;
#ifdef USE_DIRECT_THREADING
    parse_reachable_objects_return_address = &&parse_reachable_objects_return_0;
#else
    parse_reachable_objects_return_tag = 0;
#endif
    goto parse_reachable_objects;
   parse_reachable_objects_return_0:
    goto L16636;}
  else {
    if ((1 == format_449X)) {
      merged_arg1K0 = (Ssmall_img_start_addrS);
      merged_arg1K1 = (Ssymbol_addressS);
      merged_arg0K2 = format_449X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_1;
#else
      parse_reachable_objects_return_tag = 1;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_1:
      merged_arg1K0 = (Slarge_img_start_addrS);
      merged_arg1K1 = ((Slarge_img_start_addrS) + (PS_SHIFT_LEFT_INLINE((Slarge_img_heap_sizeS), 3)));
      merged_arg0K2 = format_449X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_2;
#else
      parse_reachable_objects_return_tag = 2;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_2:
      merged_arg1K0 = (Sweaks_img_start_addrS);
      merged_arg1K1 = ((Sweaks_img_start_addrS) + (PS_SHIFT_LEFT_INLINE((Sweaks_img_heap_sizeS), 3)));
      merged_arg0K2 = format_449X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_3;
#else
      parse_reachable_objects_return_tag = 3;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_3:
      goto L16636;}
    else {
      ps_error("allocate+parse+copy-objects!: Unknown image format", 0);
      goto L16636;}}}
 L15377: {
  expr_610X = arg0K0;
  Simported_bindingsS = expr_610X;
  descriptor_611X = Sexported_bindingsS;
  if ((3 == (3 & descriptor_611X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_611X))) + delta_525X))));
    goto L15383;}
  else {
    arg0K0 = descriptor_611X;
    goto L15383;}}
 L16636: {
  address_612X = ((char *) (-11 + (Sstartup_procedureS)));
  if ((0 == (((long) address_612X)))) {
    arg0K0 = -1;
    goto L15963;}
  else {
    arg0K0 = (((long) address_612X));
    goto L15963;}}
 L15383: {
  expr_613X = arg0K0;
  Sexported_bindingsS = expr_613X;
  descriptor_614X = Sresumer_recordsS;
  if ((3 == (3 & descriptor_614X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_614X))) + delta_525X))));
    goto L15389;}
  else {
    arg0K0 = descriptor_614X;
    goto L15389;}}
 L15963: {
  v_615X = arg0K0;
  image_location_616X = table_ref((Sstob_tableS), v_615X);
  Sstartup_procedureS = (3 + (((long) ((((char *) (image_location_616X->new_descriptor))) + 8))));
  v_617X = SsymbolsS;
  if ((1 == v_617X)) {
    goto L15893;}
  else {
    merged_arg0K0 = (SsymbolsS);
    merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
    allocate_table_return_address = &&allocate_table_return_0;
#else
    allocate_table_return_tag = 0;
#endif
    goto allocate_table;
   allocate_table_return_0:
    expr_618X = allocate_table0_return_value;
    SsymbolsS = expr_618X;
    table_619X = SsymbolsS;
    stob_table_620X = Sstob_tableS;
    if ((3 == (3 & table_619X))) {
      if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + table_619X))))), 2))))) {
        arg0K0 = 0;
        goto L12939;}
      else {
        goto L15893;}}
    else {
      goto L15893;}}}
 L15389: {
  expr_621X = arg0K0;
  Sresumer_recordsS = expr_621X;
  table_622X = SsymbolsS;
  if ((3 == (3 & table_622X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + table_622X))))), 2))))) {
      arg0K0 = 0;
      goto L12741;}
    else {
      goto L15395;}}
  else {
    goto L15395;}}
 L15893: {
  v_623X = Simported_bindingsS;
  if ((1 == v_623X)) {
    goto L15913;}
  else {
    merged_arg0K0 = (Simported_bindingsS);
    merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
    allocate_table_return_address = &&allocate_table_return_1;
#else
    allocate_table_return_tag = 1;
#endif
    goto allocate_table;
   allocate_table_return_1:
    expr_624X = allocate_table0_return_value;
    Simported_bindingsS = expr_624X;
    merged_arg0K0 = (Simported_bindingsS);
    merged_arg3K1 = (Sstob_tableS);
#ifdef USE_DIRECT_THREADING
    relocate_binding_table_bibopB_return_address = &&relocate_binding_table_bibopB_return_0;
#else
    relocate_binding_table_bibopB_return_tag = 0;
#endif
    goto relocate_binding_table_bibopB;
   relocate_binding_table_bibopB_return_0:
    goto L15913;}}
 L12939: {
  i_625X = arg0K0;
  if ((1024 == i_625X)) {
    goto L15893;}
  else {
    link_626X = *((long *) ((((char *) (-3 + table_619X))) + (PS_SHIFT_LEFT_INLINE(i_625X, 3))));
    if ((0 == (3 & link_626X))) {
      arg0K0 = (3 + (-4 & link_626X));
      goto L12945;}
    else {
      arg0K0 = link_626X;
      goto L12945;}}}
 L12741: {
  i_627X = arg0K0;
  if ((1024 == i_627X)) {
    goto L15395;}
  else {
    link_628X = *((long *) ((((char *) (-3 + table_622X))) + (PS_SHIFT_LEFT_INLINE(i_627X, 3))));
    if ((0 == (3 & link_628X))) {
      arg0K0 = (3 + (-4 & link_628X));
      goto L12747;}
    else {
      arg0K0 = link_628X;
      goto L12747;}}}
 L15395: {
  merged_arg0K0 = (Simported_bindingsS);
  merged_arg0K1 = delta_525X;
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_two_spaceB_return_address = &&relocate_binding_table_two_spaceB_return_0;
#else
  relocate_binding_table_two_spaceB_return_tag = 0;
#endif
  goto relocate_binding_table_two_spaceB;
 relocate_binding_table_two_spaceB_return_0:
  merged_arg0K0 = (Sexported_bindingsS);
  merged_arg0K1 = delta_525X;
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_two_spaceB_return_address = &&relocate_binding_table_two_spaceB_return_1;
#else
  relocate_binding_table_two_spaceB_return_tag = 1;
#endif
  goto relocate_binding_table_two_spaceB;
 relocate_binding_table_two_spaceB_return_1:
  start_629X = Snew_heap_start_addrS;
  arg1K0 = start_629X;
  goto L10900;}
 L15913: {
  v_630X = Sexported_bindingsS;
  if ((1 == v_630X)) {
    goto L15933;}
  else {
    merged_arg0K0 = (Sexported_bindingsS);
    merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
    allocate_table_return_address = &&allocate_table_return_2;
#else
    allocate_table_return_tag = 2;
#endif
    goto allocate_table;
   allocate_table_return_2:
    expr_631X = allocate_table0_return_value;
    Sexported_bindingsS = expr_631X;
    merged_arg0K0 = (Sexported_bindingsS);
    merged_arg3K1 = (Sstob_tableS);
#ifdef USE_DIRECT_THREADING
    relocate_binding_table_bibopB_return_address = &&relocate_binding_table_bibopB_return_1;
#else
    relocate_binding_table_bibopB_return_tag = 1;
#endif
    goto relocate_binding_table_bibopB;
   relocate_binding_table_bibopB_return_1:
    goto L15933;}}
 L12945: {
  bucket_632X = arg0K0;
  if ((1 == bucket_632X)) {
    goto L12941;}
  else {
    merged_arg0K0 = bucket_632X;
#ifdef USE_DIRECT_THREADING
    relocateD2_return_address = &&relocateD2_return_0;
#else
    relocateD2_return_tag = 0;
#endif
    goto relocateD2;
   relocateD2_return_0:
    bucket_633X = relocateD20_return_value;
    if ((3 == (3 & bucket_633X))) {
      arg0K0 = (-4 & bucket_633X);
      goto L12950;}
    else {
      arg0K0 = bucket_633X;
      goto L12950;}}}
 L12747: {
  bucket_634X = arg0K0;
  if ((1 == bucket_634X)) {
    goto L12743;}
  else {
    bucket_635X = bucket_634X + delta_525X;
    if ((3 == (3 & bucket_635X))) {
      arg0K0 = (-4 & bucket_635X);
      goto L12752;}
    else {
      arg0K0 = bucket_635X;
      goto L12752;}}}
 L10900: {
  ptr_636X = arg1K0;
  if ((ptr_636X < new_hp_526X)) {
    descriptor_637X = *((long *) ptr_636X);
    if ((3 == (3 & descriptor_637X))) {
      arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_637X))) + delta_525X))));
      goto L10905;}
    else {
      arg0K0 = descriptor_637X;
      goto L10905;}}
  else {
    arg0K0 = 0;
    goto L16853;}}
 L15933: {
  if ((1 == (Sresumer_recordsS))) {
    goto L16638;}
  else {
    merged_arg0K0 = (Sresumer_recordsS);
    merged_arg0K1 = format_449X;
#ifdef USE_DIRECT_THREADING
    allocate_table_return_address = &&allocate_table_return_3;
#else
    allocate_table_return_tag = 3;
#endif
    goto allocate_table;
   allocate_table_return_3:
    expr_638X = allocate_table0_return_value;
    Sresumer_recordsS = expr_638X;
    resumer_records_639X = Sresumer_recordsS;
    cell_640X = *((long *) (((char *) (-11 + resumer_records_639X))));
    if ((2 == (3 & cell_640X))) {
      arg1K0 = (((char *) (-3 + resumer_records_639X)));
      arg0K1 = (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_640X, 8))), 3));
      goto L10041;}
    else {
      ps_error("relocate-resumer-record! - no header", 0);
      goto L16638;}}}
 L12941: {
  arg0K0 = (1 + i_625X);
  goto L12939;}
 L12950: {
  value_641X = arg0K0;
  *((long *) ((((char *) (-3 + table_619X))) + (PS_SHIFT_LEFT_INLINE(i_625X, 3)))) = (long) (value_641X);
  arg0K0 = bucket_633X;
  goto L12957;}
 L12743: {
  arg0K0 = (1 + i_627X);
  goto L12741;}
 L12752: {
  value_642X = arg0K0;
  *((long *) ((((char *) (-3 + table_622X))) + (PS_SHIFT_LEFT_INLINE(i_627X, 3)))) = (long) (value_642X);
  arg0K0 = bucket_635X;
  goto L12759;}
 L10905: {
  d_643X = arg0K0;
  *((long *) ptr_636X) = (long) (d_643X);
  if ((2 == (3 & d_643X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(d_643X, 2))) < 16)) {
      goto L10917;}
    else {
      arg1K0 = (ptr_636X + (8 + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(d_643X, 8))))));
      goto L10900;}}
  else {
    goto L10917;}}
 L16638: {
  table_644X = Sstob_tableS;
  keys_645X = table_644X->keys;
  values_646X = table_644X->values;
  arg0K0 = 0;
  goto L2988;}
 L10041: {
  address_647X = arg1K0;
  size_648X = arg0K1;
  if ((0 == size_648X)) {
    goto L16638;}
  else {
    cell_649X = *((long *) address_647X);
    if ((3 == (3 & cell_649X))) {
      address_650X = ((char *) (-11 + cell_649X));
      if ((0 == (((long) address_650X)))) {
        arg0K0 = -1;
        goto L10106;}
      else {
        arg0K0 = (((long) address_650X));
        goto L10106;}}
    else {
      ps_error("Could this happen?", 0);
      goto L16638;}}}
 L12957: {
  entry_651X = arg0K0;
  link_652X = *((long *) ((((char *) (-3 + entry_651X))) + 8));
  if ((0 == (3 & link_652X))) {
    arg0K0 = (3 + (-4 & link_652X));
    goto L12961;}
  else {
    arg0K0 = link_652X;
    goto L12961;}}
 L12759: {
  entry_653X = arg0K0;
  link_654X = *((long *) ((((char *) (-3 + entry_653X))) + 8));
  if ((0 == (3 & link_654X))) {
    arg0K0 = (3 + (-4 & link_654X));
    goto L12763;}
  else {
    arg0K0 = link_654X;
    goto L12763;}}
 L10917: {
  arg1K0 = (ptr_636X + 8);
  goto L10900;}
 L2988: {
  i_655X = arg0K0;
  if ((i_655X == (table_644X->size))) {
    free(keys_645X);
    free(values_646X);
    free(table_644X);
    free((Sheap_image_pointerS));
    arg0K0 = 0;
    goto L16853;}
  else {
    if ((0 == (*(keys_645X + i_655X)))) {
      goto L2990;}
    else {
      free((*(values_646X + i_655X)));
      goto L2990;}}}
 L10106: {
  v_656X = arg0K0;
  image_location_657X = table_ref((Sstob_tableS), v_656X);
  *((long *) address_647X) = (long) ((3 + (((long) ((((char *) (image_location_657X->new_descriptor))) + 8)))));
  arg1K0 = (address_647X + 8);
  arg0K1 = (-1 + size_648X);
  goto L10041;}
 L12961: {
  next_658X = arg0K0;
  if ((1 == next_658X)) {
    goto L12941;}
  else {
    merged_arg0K0 = next_658X;
#ifdef USE_DIRECT_THREADING
    relocateD2_return_address = &&relocateD2_return_1;
#else
    relocateD2_return_tag = 1;
#endif
    goto relocateD2;
   relocateD2_return_1:
    next_659X = relocateD20_return_value;
    if ((3 == (3 & next_659X))) {
      arg0K0 = (-4 & next_659X);
      goto L12966;}
    else {
      arg0K0 = next_659X;
      goto L12966;}}}
 L12763: {
  next_660X = arg0K0;
  if ((1 == next_660X)) {
    goto L12743;}
  else {
    next_661X = next_660X + delta_525X;
    if ((3 == (3 & next_661X))) {
      arg0K0 = (-4 & next_661X);
      goto L12768;}
    else {
      arg0K0 = next_661X;
      goto L12768;}}}
 L2990: {
  arg0K0 = (1 + i_655X);
  goto L2988;}
 L12966: {
  val_662X = arg0K0;
  *((long *) ((((char *) (-3 + entry_651X))) + 8)) = (long) (val_662X);
  arg0K0 = next_659X;
  goto L12957;}
 L12768: {
  val_663X = arg0K0;
  *((long *) ((((char *) (-3 + entry_653X))) + 8)) = (long) (val_663X);
  arg0K0 = next_661X;
  goto L12759;}
 really_read_image_area: {
  new_start_addr_395X = merged_arg1K0;
  img_heap_size_396X = merged_arg0K1;
  port_397X = merged_arg6K2;{
  need_664X = PS_SHIFT_LEFT_INLINE(img_heap_size_396X, 3);
  got_665X = ps_read_block(port_397X, ((char *) new_start_addr_395X), need_664X, &eofP_666X, &status_667X);
  if ((status_667X == NO_ERRORS)) {
    if (eofP_666X) {
      arg4K0 = 0;
      arg5K1 = "Premature EOF when reading image file";
      goto L4121;}
    else {
      if ((got_665X < need_664X)) {
        arg4K0 = 0;
        arg5K1 = "Read returned too few bytes";
        goto L4121;}
      else {
        arg4K0 = 1;
        arg5K1 = "";
        goto L4121;}}}
  else {
    SstatusS = status_667X;
    arg4K0 = 0;
    arg5K1 = "Error reading from image file";
    goto L4121;}}
 L4121: {
  okayP_668X = arg4K0;
  string_669X = arg5K1;
  if (okayP_668X) {
    really_read_image_area0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *really_read_image_area_return_address;
#else
    goto really_read_image_area_return;
#endif
}
  else {
    ps_write_string(string_669X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_670X = SstatusS;
    if ((status_670X == NO_ERRORS)) {
      goto L4164;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L4164;}}}
 L4164: {
  status_671X = ps_close(port_397X);
  if ((status_671X == NO_ERRORS)) {
    really_read_image_area0_return_value = -1;
#ifdef USE_DIRECT_THREADING
    goto *really_read_image_area_return_address;
#else
    goto really_read_image_area_return;
#endif
}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    really_read_image_area0_return_value = -1;
#ifdef USE_DIRECT_THREADING
    goto *really_read_image_area_return_address;
#else
    goto really_read_image_area_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 really_read_image_area_return:
  switch (really_read_image_area_return_tag) {
  case 0: goto really_read_image_area_return_0;
  case 1: goto really_read_image_area_return_1;
  default: goto really_read_image_area_return_2;
  }
#endif
}

 reverse_byte_orderB: {
  start_393X = merged_arg1K0;
  end_394X = merged_arg1K1;{
  ps_write_string("Correcting byte order of resumed image.", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  arg1K0 = start_393X;
  goto L10794;}
 L10794: {
  ptr_672X = arg1K0;
  if ((ptr_672X < end_394X)) {
    arg0K0 = 0;
    arg0K1 = 7;
    goto L10834;}
  else {
#ifdef USE_DIRECT_THREADING
    goto *reverse_byte_orderB_return_address;
#else
    goto reverse_byte_orderB_return;
#endif
}}
 L10834: {
  i_673X = arg0K0;
  j_674X = arg0K1;
  if ((i_673X < j_674X)) {
    addr_a_675X = ptr_672X + i_673X;
    addr_b_676X = ptr_672X + j_674X;
    byte_a_677X = *((unsigned char *) addr_a_675X);
    *((unsigned char *) addr_a_675X) = (unsigned char) ((*((unsigned char *) addr_b_676X)));
    *((unsigned char *) addr_b_676X) = (unsigned char) (byte_a_677X);
    arg0K0 = (1 + i_673X);
    arg0K1 = (-1 + j_674X);
    goto L10834;}
  else {
    value_678X = *((long *) ptr_672X);
    next_679X = ptr_672X + 8;
    if ((2 == (3 & value_678X))) {
      if (((31 & (PS_SHIFT_RIGHT_INLINE(value_678X, 2))) < 16)) {
        arg1K0 = next_679X;
        goto L10794;}
      else {
        arg1K0 = (next_679X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(value_678X, 8)))));
        goto L10794;}}
    else {
      arg1K0 = next_679X;
      goto L10794;}}}
#ifndef USE_DIRECT_THREADING
 reverse_byte_orderB_return:
  switch (reverse_byte_orderB_return_tag) {
  case 0: goto reverse_byte_orderB_return_0;
  default: goto reverse_byte_orderB_return_1;
  }
#endif
}

 relocate_binding_table_two_spaceB: {
  table_391X = merged_arg0K0;
  delta_392X = merged_arg0K1;{
  if ((3 == (3 & table_391X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + table_391X))))), 2))))) {
      arg0K0 = 0;
      goto L12828;}
    else {
#ifdef USE_DIRECT_THREADING
      goto *relocate_binding_table_two_spaceB_return_address;
#else
      goto relocate_binding_table_two_spaceB_return;
#endif
}}
  else {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_two_spaceB_return_address;
#else
    goto relocate_binding_table_two_spaceB_return;
#endif
}}
 L12828: {
  i_680X = arg0K0;
  if ((1024 == i_680X)) {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_two_spaceB_return_address;
#else
    goto relocate_binding_table_two_spaceB_return;
#endif
}
  else {
    link_681X = *((long *) ((((char *) (-3 + table_391X))) + (PS_SHIFT_LEFT_INLINE(i_680X, 3))));
    if ((0 == (3 & link_681X))) {
      arg0K0 = (3 + (-4 & link_681X));
      goto L12834;}
    else {
      arg0K0 = link_681X;
      goto L12834;}}}
 L12834: {
  bucket_682X = arg0K0;
  if ((1 == bucket_682X)) {
    goto L12830;}
  else {
    bucket_683X = bucket_682X + delta_392X;
    if ((3 == (3 & bucket_683X))) {
      arg0K0 = (-4 & bucket_683X);
      goto L12839;}
    else {
      arg0K0 = bucket_683X;
      goto L12839;}}}
 L12830: {
  arg0K0 = (1 + i_680X);
  goto L12828;}
 L12839: {
  value_684X = arg0K0;
  *((long *) ((((char *) (-3 + table_391X))) + (PS_SHIFT_LEFT_INLINE(i_680X, 3)))) = (long) (value_684X);
  arg0K0 = bucket_683X;
  goto L12846;}
 L12846: {
  entry_685X = arg0K0;
  link_686X = *((long *) ((((char *) (-3 + entry_685X))) + 24));
  if ((0 == (3 & link_686X))) {
    arg0K0 = (3 + (-4 & link_686X));
    goto L12850;}
  else {
    arg0K0 = link_686X;
    goto L12850;}}
 L12850: {
  next_687X = arg0K0;
  if ((1 == next_687X)) {
    goto L12830;}
  else {
    next_688X = next_687X + delta_392X;
    if ((3 == (3 & next_688X))) {
      arg0K0 = (-4 & next_688X);
      goto L12855;}
    else {
      arg0K0 = next_688X;
      goto L12855;}}}
 L12855: {
  val_689X = arg0K0;
  *((long *) ((((char *) (-3 + entry_685X))) + 24)) = (long) (val_689X);
  arg0K0 = next_688X;
  goto L12846;}
#ifndef USE_DIRECT_THREADING
 relocate_binding_table_two_spaceB_return:
  switch (relocate_binding_table_two_spaceB_return_tag) {
  case 0: goto relocate_binding_table_two_spaceB_return_0;
  default: goto relocate_binding_table_two_spaceB_return_1;
  }
#endif
}

 relocateD0: {
  address_390X = merged_arg0K0;{
  address_690X = ((char *) (-11 + address_390X));
  if ((0 == (((long) address_690X)))) {
    arg0K0 = -1;
    goto L13029;}
  else {
    arg0K0 = (((long) address_690X));
    goto L13029;}}
 L13029: {
  v_691X = arg0K0;
  image_location_692X = table_ref(stob_table_389X, v_691X);
  relocateD00_return_value = (3 + (((long) ((((char *) (image_location_692X->new_descriptor))) + 8))));
#ifdef USE_DIRECT_THREADING
  goto *relocateD0_return_address;
#else
  goto relocateD0_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 relocateD0_return:
  switch (relocateD0_return_tag) {
  case 0: goto relocateD0_return_0;
  default: goto relocateD0_return_1;
  }
#endif
}

 relocate_binding_table_bibopB: {
  table_388X = merged_arg0K0;
  stob_table_389X = merged_arg3K1;{
  if ((3 == (3 & table_388X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + table_388X))))), 2))))) {
      arg0K0 = 0;
      goto L13059;}
    else {
#ifdef USE_DIRECT_THREADING
      goto *relocate_binding_table_bibopB_return_address;
#else
      goto relocate_binding_table_bibopB_return;
#endif
}}
  else {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_bibopB_return_address;
#else
    goto relocate_binding_table_bibopB_return;
#endif
}}
 L13059: {
  i_693X = arg0K0;
  if ((1024 == i_693X)) {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_bibopB_return_address;
#else
    goto relocate_binding_table_bibopB_return;
#endif
}
  else {
    link_694X = *((long *) ((((char *) (-3 + table_388X))) + (PS_SHIFT_LEFT_INLINE(i_693X, 3))));
    if ((0 == (3 & link_694X))) {
      arg0K0 = (3 + (-4 & link_694X));
      goto L13065;}
    else {
      arg0K0 = link_694X;
      goto L13065;}}}
 L13065: {
  bucket_695X = arg0K0;
  if ((1 == bucket_695X)) {
    goto L13061;}
  else {
    merged_arg0K0 = bucket_695X;
#ifdef USE_DIRECT_THREADING
    relocateD0_return_address = &&relocateD0_return_0;
#else
    relocateD0_return_tag = 0;
#endif
    goto relocateD0;
   relocateD0_return_0:
    bucket_696X = relocateD00_return_value;
    if ((3 == (3 & bucket_696X))) {
      arg0K0 = (-4 & bucket_696X);
      goto L13070;}
    else {
      arg0K0 = bucket_696X;
      goto L13070;}}}
 L13061: {
  arg0K0 = (1 + i_693X);
  goto L13059;}
 L13070: {
  value_697X = arg0K0;
  *((long *) ((((char *) (-3 + table_388X))) + (PS_SHIFT_LEFT_INLINE(i_693X, 3)))) = (long) (value_697X);
  arg0K0 = bucket_696X;
  goto L13077;}
 L13077: {
  entry_698X = arg0K0;
  link_699X = *((long *) ((((char *) (-3 + entry_698X))) + 24));
  if ((0 == (3 & link_699X))) {
    arg0K0 = (3 + (-4 & link_699X));
    goto L13081;}
  else {
    arg0K0 = link_699X;
    goto L13081;}}
 L13081: {
  next_700X = arg0K0;
  if ((1 == next_700X)) {
    goto L13061;}
  else {
    merged_arg0K0 = next_700X;
#ifdef USE_DIRECT_THREADING
    relocateD0_return_address = &&relocateD0_return_1;
#else
    relocateD0_return_tag = 1;
#endif
    goto relocateD0;
   relocateD0_return_1:
    next_701X = relocateD00_return_value;
    if ((3 == (3 & next_701X))) {
      arg0K0 = (-4 & next_701X);
      goto L13086;}
    else {
      arg0K0 = next_701X;
      goto L13086;}}}
 L13086: {
  val_702X = arg0K0;
  *((long *) ((((char *) (-3 + entry_698X))) + 24)) = (long) (val_702X);
  arg0K0 = next_701X;
  goto L13077;}
#ifndef USE_DIRECT_THREADING
 relocate_binding_table_bibopB_return:
  switch (relocate_binding_table_bibopB_return_tag) {
  case 0: goto relocate_binding_table_bibopB_return_0;
  default: goto relocate_binding_table_bibopB_return_1;
  }
#endif
}

 allocate_table: {
  tab_386X = merged_arg0K0;
  image_format_387X = merged_arg0K1;{
  merged_arg1K0 = (((char *) (-11 + tab_386X)));
  merged_arg0K1 = image_format_387X;
#ifdef USE_DIRECT_THREADING
  old_Gnew_addr_return_address = &&old_Gnew_addr_return_2;
#else
  old_Gnew_addr_return_tag = 2;
#endif
  goto old_Gnew_addr;
 old_Gnew_addr_return_2:
  addr_703X = old_Gnew_addr0_return_value;
  cell_704X = *((long *) addr_703X);
  if ((2 == (3 & cell_704X))) {
    size_705X = PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_704X, 8))), 3);
    size_in_byte_706X = 8 + (PS_SHIFT_LEFT_INLINE(size_705X, 3));
    x_707X = ((s48_ShpS) + (-8 & (7 + size_in_byte_706X))) < (s48_SlimitS);
    if (x_707X) {
      goto L15265;}
    else {s48_collect(0);
      goto L15265;}}
  else {
    ps_error("read-tables! no header", 0);
    allocate_table0_return_value = v_708X;
#ifdef USE_DIRECT_THREADING
    goto *allocate_table_return_address;
#else
    goto allocate_table_return;
#endif
}}
 L15265: {
  if ((((s48_ShpS) + (-8 & (7 + size_in_byte_706X))) < (s48_SlimitS))) {
    new_709X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + size_in_byte_706X)));
    arg1K0 = new_709X;
    goto L15233;}
  else {
    arg1K0 = NULL;
    goto L15233;}}
 L15233: {
  pointer_710X = arg1K0;
  memmove((void *)pointer_710X, (void *)addr_703X,(8 + (PS_SHIFT_LEFT_INLINE(size_705X, 3))));
  allocate_table0_return_value = (3 + (((long) (pointer_710X + 8))));
#ifdef USE_DIRECT_THREADING
  goto *allocate_table_return_address;
#else
  goto allocate_table_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 allocate_table_return:
  switch (allocate_table_return_tag) {
  case 0: goto allocate_table_return_0;
  case 1: goto allocate_table_return_1;
  case 2: goto allocate_table_return_2;
  default: goto allocate_table_return_3;
  }
#endif
}

 alloc_object: {
  current_address_384X = merged_arg1K0;
  size_in_bytes_385X = merged_arg0K1;{
  x_711X = ((s48_ShpS) + (-8 & (7 + size_in_bytes_385X))) < (s48_SlimitS);
  if (x_711X) {
    goto L15172;}
  else {s48_collect(0);
    goto L15172;}}
 L15172: {
  if ((((s48_ShpS) + (-8 & (7 + size_in_bytes_385X))) < (s48_SlimitS))) {
    new_712X = s48_ShpS;
    s48_ShpS = ((s48_ShpS) + (-8 & (7 + size_in_bytes_385X)));
    arg1K0 = new_712X;
    goto L15162;}
  else {
    arg1K0 = NULL;
    goto L15162;}}
 L15162: {
  new_address_713X = arg1K0;
  image_location_714X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_714X)) {
    arg2K0 = image_location_714X;
    goto L15189;}
  else {
    image_location_714X->new_descriptor = (((long) new_address_713X));
    image_location_714X->next = 0;
    arg2K0 = image_location_714X;
    goto L15189;}}
 L15189: {
  val_715X = arg2K0;
  if ((0 == (((long) current_address_384X)))) {
    arg0K0 = -1;
    goto L15191;}
  else {
    arg0K0 = (((long) current_address_384X));
    goto L15191;}}
 L15191: {
  v_716X = arg0K0;table_setB((Sstob_tableS), v_716X, val_715X);
  alloc_object0_return_value = new_address_713X;
#ifdef USE_DIRECT_THREADING
  goto *alloc_object_return_address;
#else
  goto alloc_object_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 alloc_object_return:
  switch (alloc_object_return_tag) {
  case 0: goto alloc_object_return_0;
  case 1: goto alloc_object_return_1;
  case 2: goto alloc_object_return_2;
  default: goto alloc_object_return_3;
  }
#endif
}

 parse_reachable_objects: {
  from_addr_381X = merged_arg1K0;
  to_addr_382X = merged_arg1K1;
  image_format_383X = merged_arg0K2;{
  arg1K0 = from_addr_381X;
  goto L16284;}
 L16284: {
  current_addr_717X = arg1K0;
  if ((current_addr_717X == to_addr_382X)) {
    parse_reachable_objects0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *parse_reachable_objects_return_address;
#else
    goto parse_reachable_objects_return;
#endif
}
  else {
    merged_arg1K0 = current_addr_717X;
    merged_arg0K1 = image_format_383X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_3;
#else
    old_Gnew_addr_return_tag = 3;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_3:
    x1_718X = old_Gnew_addr0_return_value;
    cell_719X = *((long *) x1_718X);
    if ((2 == (3 & cell_719X))) {
      size_in_cells_720X = PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_719X, 8))), 3);
      if ((0 == (Sheap_object_remaining_cellsS))) {
        goto L16309;}
      else {
        ps_error("Encountered an header within an d-vector.", 0);
        goto L16309;}}
    else {
      if ((3 == (3 & cell_719X))) {
        current_addr_721X = ((char *) (-11 + cell_719X));
        if ((0 == (((long) current_addr_721X)))) {
          arg0K0 = -1;
          goto L16511;}
        else {
          arg0K0 = (((long) current_addr_721X));
          goto L16511;}}
      else {
        *((long *) (Sheap_object_pointerS)) = (long) (cell_719X);
        Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
        Sheap_object_pointerS = ((Sheap_object_pointerS) + 8);
        goto L16366;}}}}
 L16309: {
  if ((2 == (3 & cell_719X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(cell_719X, 2))) < 16)) {
      goto L16325;}
    else {
      if ((0 == (((long) current_addr_717X)))) {
        arg0K0 = -1;
        goto L16397;}
      else {
        arg0K0 = (((long) current_addr_717X));
        goto L16397;}}}
  else {
    goto L16325;}}
 L16511: {
  v_722X = arg0K0;
  v_723X = table_ref((Sstob_tableS), v_722X);
  if ((NULL == v_723X)) {
    current_addr_724X = ((char *) (-11 + cell_719X));
    merged_arg1K0 = current_addr_724X;
    merged_arg0K1 = image_format_383X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_4;
#else
    old_Gnew_addr_return_tag = 4;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_4:
    x1_725X = old_Gnew_addr0_return_value;
    header_cell_726X = *((long *) x1_725X);
    size_in_cells_727X = 1 + (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_cell_726X, 8))), 3));
    size_in_bytes_728X = PS_SHIFT_LEFT_INLINE(size_in_cells_727X, 3);
    if ((2 == (3 & header_cell_726X))) {
      if (((31 & (PS_SHIFT_RIGHT_INLINE(header_cell_726X, 2))) < 16)) {
        goto L16143;}
      else {
        merged_arg1K0 = current_addr_724X;
        merged_arg0K1 = (PS_SHIFT_LEFT_INLINE(size_in_cells_727X, 3));
#ifdef USE_DIRECT_THREADING
        alloc_object_return_address = &&alloc_object_return_0;
#else
        alloc_object_return_tag = 0;
#endif
        goto alloc_object;
       alloc_object_return_0:
        new_address_729X = alloc_object0_return_value;
        merged_arg1K0 = current_addr_724X;
        merged_arg0K1 = image_format_383X;
#ifdef USE_DIRECT_THREADING
        old_Gnew_addr_return_address = &&old_Gnew_addr_return_5;
#else
        old_Gnew_addr_return_tag = 5;
#endif
        goto old_Gnew_addr;
       old_Gnew_addr_return_5:
        v_730X = old_Gnew_addr0_return_value;
        memmove((void *)new_address_729X, (void *)v_730X,(PS_SHIFT_LEFT_INLINE(size_in_cells_727X, 3)));
        goto L16357;}}
    else {
      goto L16143;}}
  else {
    goto L16357;}}
 L16366: {
  arg1K0 = (current_addr_717X + 8);
  goto L16284;}
 L16325: {
  if ((0 == (((long) current_addr_717X)))) {
    arg0K0 = -1;
    goto L16424;}
  else {
    arg0K0 = (((long) current_addr_717X));
    goto L16424;}}
 L16397: {
  v_731X = arg0K0;
  v_732X = table_ref((Sstob_tableS), v_731X);
  if ((NULL == v_732X)) {
    size_in_cells_733X = 1 + size_in_cells_720X;
    merged_arg1K0 = current_addr_717X;
    merged_arg0K1 = (PS_SHIFT_LEFT_INLINE(size_in_cells_733X, 3));
#ifdef USE_DIRECT_THREADING
    alloc_object_return_address = &&alloc_object_return_1;
#else
    alloc_object_return_tag = 1;
#endif
    goto alloc_object;
   alloc_object_return_1:
    new_address_734X = alloc_object0_return_value;
    merged_arg1K0 = current_addr_717X;
    merged_arg0K1 = image_format_383X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_6;
#else
    old_Gnew_addr_return_tag = 6;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_6:
    v_735X = old_Gnew_addr0_return_value;
    memmove((void *)new_address_734X, (void *)v_735X,(PS_SHIFT_LEFT_INLINE(size_in_cells_733X, 3)));
    goto L16322;}
  else {
    goto L16322;}}
 L16143: {
  merged_arg1K0 = current_addr_724X;
  merged_arg0K1 = size_in_bytes_728X;
#ifdef USE_DIRECT_THREADING
  alloc_object_return_address = &&alloc_object_return_2;
#else
  alloc_object_return_tag = 2;
#endif
  goto alloc_object;
 alloc_object_return_2:
  new_address_736X = alloc_object0_return_value;
  *((long *) new_address_736X) = (long) ((-1978 + (PS_SHIFT_LEFT_INLINE(size_in_bytes_728X, 8))));
  arg1K0 = (new_address_736X + 8);
  goto L16161;}
 L16357: {
  address_737X = ((char *) (-11 + cell_719X));
  if ((0 == (((long) address_737X)))) {
    arg0K0 = -1;
    goto L16525;}
  else {
    arg0K0 = (((long) address_737X));
    goto L16525;}}
 L16424: {
  v_738X = arg0K0;
  v_739X = table_ref((Sstob_tableS), v_738X);
  if ((NULL == v_739X)) {
    merged_arg1K0 = current_addr_717X;
    merged_arg0K1 = (8 + (PS_SHIFT_LEFT_INLINE(size_in_cells_720X, 3)));
#ifdef USE_DIRECT_THREADING
    alloc_object_return_address = &&alloc_object_return_3;
#else
    alloc_object_return_tag = 3;
#endif
    goto alloc_object;
   alloc_object_return_3:
    new_address_740X = alloc_object0_return_value;
    if ((2 == (3 & cell_719X))) {
      goto L16441;}
    else {
      ps_error("cell was not a header", 0);
      goto L16441;}}
  else {
    if ((0 == (((long) current_addr_717X)))) {
      arg0K0 = -1;
      goto L10166;}
    else {
      arg0K0 = (((long) current_addr_717X));
      goto L10166;}}}
 L16322: {
  arg1K0 = (current_addr_717X + (8 + (PS_SHIFT_LEFT_INLINE(size_in_cells_720X, 3))));
  goto L16284;}
 L16161: {
  index_741X = arg1K0;
  if ((index_741X == (new_address_736X + (-8 & (7 + size_in_bytes_728X))))) {
    goto L16357;}
  else {
    *((long *) index_741X) = (long) (0);
    arg1K0 = (index_741X + 8);
    goto L16161;}}
 L16525: {
  v_742X = arg0K0;
  image_location_743X = table_ref((Sstob_tableS), v_742X);
  *((long *) (Sheap_object_pointerS)) = (long) ((3 + (((long) ((((char *) (image_location_743X->new_descriptor))) + 8)))));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 8);
  goto L16366;}
 L16441: {
  *((long *) new_address_740X) = (long) (cell_719X);
  Sheap_object_pointerS = new_address_740X;
  Sheap_object_remaining_cellsS = (1 + (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_719X, 8))), 3)));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 8);
  goto L16337;}
 L10166: {
  v_744X = arg0K0;
  image_location_745X = table_ref((Sstob_tableS), v_744X);
  new_address_746X = ((char *) (image_location_745X->new_descriptor));
  if ((2 == (3 & cell_719X))) {
    goto L10151;}
  else {
    ps_error("cell was not a header", 0);
    goto L10151;}}
 L16337: {
  arg1K0 = (current_addr_717X + 8);
  goto L16284;}
 L10151: {
  *((long *) new_address_746X) = (long) (cell_719X);
  Sheap_object_pointerS = new_address_746X;
  Sheap_object_remaining_cellsS = (1 + (PS_SHIFT_RIGHT_INLINE((7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_719X, 8))), 3)));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 8);
  goto L16337;}
#ifndef USE_DIRECT_THREADING
 parse_reachable_objects_return:
  switch (parse_reachable_objects_return_tag) {
  case 0: goto parse_reachable_objects_return_0;
  case 1: goto parse_reachable_objects_return_1;
  case 2: goto parse_reachable_objects_return_2;
  default: goto parse_reachable_objects_return_3;
  }
#endif
}

 old_Gnew_addr: {
  addr_379X = merged_arg1K0;
  format_380X = merged_arg0K1;{
  if ((0 == format_380X)) {
    old_Gnew_addr0_return_value = ((Sheap_image_pointerS) + (addr_379X - (Simg_start_addrS)));
#ifdef USE_DIRECT_THREADING
    goto *old_Gnew_addr_return_address;
#else
    goto old_Gnew_addr_return;
#endif
}
  else {
    if ((1 == format_380X)) {
      y_747X = Ssmall_img_start_addrS;
      if ((addr_379X < y_747X)) {
        goto L3091;}
      else {
        if (((Ssmall_img_hp_addrS) < addr_379X)) {
          goto L3091;}
        else {
          SoffsetS = (((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS)) + ((Slarge_img_end_addrS) - (Slarge_img_start_addrS)));
          Sarea_startS = (Ssmall_img_start_addrS);
          goto L3143;}}}
    else {
      ps_error("old->new-addr: Unknown image format", 0);
      old_Gnew_addr0_return_value = v_748X;
#ifdef USE_DIRECT_THREADING
      goto *old_Gnew_addr_return_address;
#else
      goto old_Gnew_addr_return;
#endif
}}}
 L3091: {
  y_749X = Slarge_img_start_addrS;
  if ((addr_379X < y_749X)) {
    goto L3117;}
  else {
    if (((Slarge_img_hp_addrS) < addr_379X)) {
      goto L3117;}
    else {
      SoffsetS = ((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS));
      Sarea_startS = (Slarge_img_start_addrS);
      goto L3143;}}}
 L3143: {
  old_Gnew_addr0_return_value = ((Sheap_image_pointerS) + ((SoffsetS) + (addr_379X - (Sarea_startS))));
#ifdef USE_DIRECT_THREADING
  goto *old_Gnew_addr_return_address;
#else
  goto old_Gnew_addr_return;
#endif
}
 L3117: {
  y_750X = Sweaks_img_start_addrS;
  if ((addr_379X < y_750X)) {
    goto L3139;}
  else {
    if (((Sweaks_img_hp_addrS) < addr_379X)) {
      goto L3139;}
    else {
      SoffsetS = 0;
      Sarea_startS = (Sweaks_img_start_addrS);
      goto L3143;}}}
 L3139: {
  ps_error("Unknown address area!", 0);
  goto L3143;}
#ifndef USE_DIRECT_THREADING
 old_Gnew_addr_return:
  switch (old_Gnew_addr_return_tag) {
  case 0: goto old_Gnew_addr_return_0;
  case 1: goto old_Gnew_addr_return_1;
  case 2: goto old_Gnew_addr_return_2;
  case 3: goto old_Gnew_addr_return_3;
  case 4: goto old_Gnew_addr_return_4;
  case 5: goto old_Gnew_addr_return_5;
  default: goto old_Gnew_addr_return_6;
  }
#endif
}

 relocateD2: {
  address_378X = merged_arg0K0;{
  address_751X = ((char *) (-11 + address_378X));
  if ((0 == (((long) address_751X)))) {
    arg0K0 = -1;
    goto L12909;}
  else {
    arg0K0 = (((long) address_751X));
    goto L12909;}}
 L12909: {
  v_752X = arg0K0;
  image_location_753X = table_ref(stob_table_620X, v_752X);
  relocateD20_return_value = (3 + (((long) ((((char *) (image_location_753X->new_descriptor))) + 8))));
#ifdef USE_DIRECT_THREADING
  goto *relocateD2_return_address;
#else
  goto relocateD2_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 relocateD2_return:
  switch (relocateD2_return_tag) {
  case 0: goto relocateD2_return_0;
  default: goto relocateD2_return_1;
  }
#endif
}

}
void s48_trace_locationsB(char * start_754X, char * end_755X)
{
  char * arg1K0;
  char * arg1K1;
  long arg0K0;
  long newpair_787X;
  char * pair_address_786X;
  long tconc_cdr_785X;
  char * ptr_784X;
  long mask_783X;
  long thing_782X;
  long tconc_car_781X;
  char * trace_ptr_780X;
  char * mask_ptr_779X;
  long new_778X;
  char * data_addr_777X;
  char * frontier_776X;
  long new_thing_775X;
  long thing_774X;
  long tconc_773X;
  long tlc_772X;
  long size_771X;
  char * mask_pointer_770X;
  char * data_pointer_769X;
  long mask_size_768X;
  char * code_pointer_767X;
  long size_766X;
  char * frontier_765X;
  long new_thing_764X;
  char * a_763X;
  long descriptor_762X;
  long h_761X;
  char * a_760X;
  char * next_759X;
  long thing_758X;
  char * frontier_757X;
  char * addr_756X;
 {  arg1K0 = start_754X;
  arg1K1 = (s48_ShpS);
  goto L16944;}
 L16944: {
  addr_756X = arg1K0;
  frontier_757X = arg1K1;
  if ((addr_756X < end_755X)) {
    thing_758X = *((long *) addr_756X);
    next_759X = addr_756X + 8;
    if ((2 == (3 & thing_758X))) {
      if ((2 == (3 & thing_758X))) {
        if (((31 & (PS_SHIFT_RIGHT_INLINE(thing_758X, 2))) < 16)) {
          goto L16961;}
        else {
          arg1K0 = (next_759X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(thing_758X, 8)))));
          arg1K1 = frontier_757X;
          goto L16944;}}
      else {
        goto L16961;}}
    else {
      if ((3 == (3 & thing_758X))) {
        a_760X = ((char *) (-3 + thing_758X));
        if ((a_760X < (Sfrom_beginS))) {
          arg1K0 = next_759X;
          arg1K1 = frontier_757X;
          goto L16944;}
        else {
          if ((a_760X < (Sfrom_endS))) {
            h_761X = *((long *) (((char *) (-11 + thing_758X))));
            if ((3 == (3 & h_761X))) {
              arg0K0 = h_761X;
              arg1K1 = frontier_757X;
              goto L16997;}
            else {
              if ((2102 == h_761X)) {
                descriptor_762X = *((long *) (((char *) (-3 + thing_758X))));
                if ((3 == (3 & descriptor_762X))) {
                  a_763X = ((char *) (-3 + descriptor_762X));
                  if ((a_763X < (Sfrom_beginS))) {
                    goto L17683;}
                  else {
                    if ((a_763X < (Sfrom_endS))) {
                      new_thing_764X = copy_weak_pointer(thing_758X, frontier_757X, &frontier_765X);
                      arg0K0 = new_thing_764X;
                      arg1K1 = frontier_765X;
                      goto L16997;}
                    else {
                      goto L17683;}}}
                else {
                  goto L17683;}}
              else {
                goto L17683;}}}
          else {
            arg1K0 = next_759X;
            arg1K1 = frontier_757X;
            goto L16944;}}}
      else {
        arg1K0 = next_759X;
        arg1K1 = frontier_757X;
        goto L16944;}}}
  else {
    s48_ShpS = frontier_757X;
    return;}}
 L16961: {
  if ((10 == (31 & (PS_SHIFT_RIGHT_INLINE(thing_758X, 2))))) {
    size_766X = -8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(thing_758X, 8)));
    s48_ShpS = frontier_757X;
    code_pointer_767X = (((char *) (-3 + (*((long *) (next_759X + 8)))))) + (PS_SHIFT_RIGHT_INLINE((*((long *) next_759X)), 2));
    mask_size_768X = *((unsigned char *) (code_pointer_767X + -3));
    if ((0 == mask_size_768X)) {s48_trace_locationsB(next_759X, (next_759X + size_766X));
      goto L16972;}
    else {
      data_pointer_769X = next_759X + 24;s48_trace_locationsB(next_759X, data_pointer_769X);
      mask_pointer_770X = code_pointer_767X + -7;
      arg1K0 = (mask_pointer_770X + (0 - mask_size_768X));
      arg1K1 = data_pointer_769X;
      goto L17635;}}
  else {
    if ((15 == (31 & (PS_SHIFT_RIGHT_INLINE(thing_758X, 2))))) {
      size_771X = -8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(thing_758X, 8)));
      s48_ShpS = frontier_757X;s48_trace_locationsB(next_759X, (next_759X + size_771X));
      tlc_772X = 3 + (((long) next_759X));
      tconc_773X = *((long *) ((((char *) (-3 + tlc_772X))) + 16));
      if ((3 == (3 & tconc_773X))) {
        if ((0 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + tconc_773X))))), 2))))) {
          if ((3 == (3 & (*((long *) (((char *) (-3 + tlc_772X)))))))) {
            thing_774X = *((long *) (((char *) (-3 + tconc_773X))));
            if ((3 == (3 & thing_774X))) {
              if ((3 == (3 & (*((long *) (((char *) (-11 + thing_774X)))))))) {
                arg0K0 = (*((long *) (((char *) (-11 + thing_774X)))));
                goto L17182;}
              else {
                arg0K0 = thing_774X;
                goto L17182;}}
            else {
              arg0K0 = thing_774X;
              goto L17182;}}
          else {
            goto L16986;}}
        else {
          goto L16986;}}
      else {
        goto L16986;}}
    else {
      arg1K0 = next_759X;
      arg1K1 = frontier_757X;
      goto L16944;}}}
 L16997: {
  new_thing_775X = arg0K0;
  frontier_776X = arg1K1;
  *((long *) addr_756X) = (long) (new_thing_775X);
  arg1K0 = next_759X;
  arg1K1 = frontier_776X;
  goto L16944;}
 L17683: {
  *((long *) frontier_757X) = (long) (h_761X);
  data_addr_777X = frontier_757X + 8;
  new_778X = 3 + (((long) data_addr_777X));
  *((long *) (((char *) (-11 + thing_758X)))) = (long) (new_778X);
  memmove((void *)data_addr_777X, (void *)(((char *) (-3 + thing_758X))),(PS_SHIFT_RIGHT_LOGICAL_INLINE(h_761X, 8)));
  arg0K0 = new_778X;
  arg1K1 = (data_addr_777X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(h_761X, 8)))));
  goto L16997;}
 L16972: {
  arg1K0 = (next_759X + size_766X);
  arg1K1 = (s48_ShpS);
  goto L16944;}
 L17635: {
  mask_ptr_779X = arg1K0;
  trace_ptr_780X = arg1K1;
  if ((mask_ptr_779X == mask_pointer_770X)) {
    goto L16972;}
  else {
    arg0K0 = (*((unsigned char *) mask_ptr_779X));
    arg1K1 = trace_ptr_780X;
    goto L17642;}}
 L17182: {
  tconc_car_781X = arg0K0;
  thing_782X = *((long *) ((((char *) (-3 + tconc_773X))) + 8));
  if ((3 == (3 & thing_782X))) {
    if ((3 == (3 & (*((long *) (((char *) (-11 + thing_782X)))))))) {
      arg0K0 = (*((long *) (((char *) (-11 + thing_782X)))));
      goto L17186;}
    else {
      arg0K0 = thing_782X;
      goto L17186;}}
  else {
    arg0K0 = thing_782X;
    goto L17186;}}
 L16986: {
  arg1K0 = (next_759X + size_771X);
  arg1K1 = (s48_ShpS);
  goto L16944;}
 L17642: {
  mask_783X = arg0K0;
  ptr_784X = arg1K1;
  if ((0 == mask_783X)) {
    arg1K0 = (mask_ptr_779X + 1);
    arg1K1 = (trace_ptr_780X + 64);
    goto L17635;}
  else {
    if ((1 == (1 & mask_783X))) {s48_trace_locationsB(ptr_784X, (ptr_784X + 8));
      goto L17644;}
    else {
      goto L17644;}}}
 L17186: {
  tconc_cdr_785X = arg0K0;
  if ((3 == (3 & tconc_car_781X))) {
    if ((0 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + tconc_car_781X))))), 2))))) {
      if ((3 == (3 & tconc_cdr_785X))) {
        if ((0 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + tconc_cdr_785X))))), 2))))) {
          pair_address_786X = s48_ShpS;
          *((long *) pair_address_786X) = (long) (4098);
          newpair_787X = 3 + (((long) (pair_address_786X + 8)));
          if ((3 == (3 & newpair_787X))) {
            if ((0 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-11 + newpair_787X))))), 2))))) {
              *((long *) (((char *) (-3 + newpair_787X)))) = (long) (1);
              *((long *) ((((char *) (-3 + newpair_787X))) + 8)) = (long) (1);
              *((long *) (((char *) (-3 + tconc_cdr_785X)))) = (long) (tlc_772X);
              *((long *) ((((char *) (-3 + tconc_cdr_785X))) + 8)) = (long) (newpair_787X);
              *((long *) ((((char *) (-3 + tconc_773X))) + 8)) = (long) (newpair_787X);
              *((long *) ((((char *) (-3 + tlc_772X))) + 16)) = (long) (1);
              s48_ShpS = (pair_address_786X + 24);
              goto L16986;}
            else {
              goto L16986;}}
          else {
            goto L16986;}}
        else {
          goto L16986;}}
      else {
        goto L16986;}}
    else {
      goto L16986;}}
  else {
    goto L16986;}}
 L17644: {
  arg0K0 = (PS_SHIFT_RIGHT_INLINE(mask_783X, 1));
  arg1K1 = (ptr_784X + 8);
  goto L17642;}
}
void s48_trace_stob_contentsB(long stob_788X)
{
  char * start_790X;
  long h_789X;
 {  h_789X = *((long *) (((char *) (-11 + stob_788X))));
  start_790X = ((char *) (-3 + stob_788X));
  s48_trace_locationsB(start_790X, (start_790X + (-8 & (7 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(h_789X, 8))))));
  return;}
}
void s48_trace_continuation_contentsB(char * contents_pointer_791X, char * code_pointer_792X, long mask_size_793X)
{
  char * arg1K0;
  char * arg1K1;
  long arg0K0;
  char * ptr_798X;
  long mask_797X;
  char * trace_ptr_796X;
  char * mask_ptr_795X;
  char * mask_pointer_794X;
 {  mask_pointer_794X = code_pointer_792X + -7;
  arg1K0 = (mask_pointer_794X + (0 - mask_size_793X));
  arg1K1 = contents_pointer_791X;
  goto L17487;}
 L17487: {
  mask_ptr_795X = arg1K0;
  trace_ptr_796X = arg1K1;
  if ((mask_ptr_795X == mask_pointer_794X)) {
    return;}
  else {
    arg0K0 = (*((unsigned char *) mask_ptr_795X));
    arg1K1 = trace_ptr_796X;
    goto L17495;}}
 L17495: {
  mask_797X = arg0K0;
  ptr_798X = arg1K1;
  if ((0 == mask_797X)) {
    arg1K0 = (mask_ptr_795X + 1);
    arg1K1 = (trace_ptr_796X + 64);
    goto L17487;}
  else {
    if ((1 == (1 & mask_797X))) {s48_trace_locationsB(ptr_798X, (ptr_798X + 8));
      goto L17511;}
    else {
      goto L17511;}}}
 L17511: {
  arg0K0 = (PS_SHIFT_RIGHT_INLINE(mask_797X, 1));
  arg1K1 = (ptr_798X + 8);
  goto L17495;}
}void
s48_heap_init(void)
{
Snew_heap_start_addrS = NULL;
Spure_area_countS = 0;
Simpure_area_countS = 0;
Sfinding_typeS = 1;
Sheap_errors_leftS = 0;
Sgc_countS = 0;
Sgc_secondsS = 0;
Sgc_msecondsS = 0;
SstatusS = NO_ERRORS;
SeofPS = 0;
Sstartup_procedureS = 0;
SsymbolsS = 0;
Simported_bindingsS = 0;
Sexported_bindingsS = 0;
Sresumer_recordsS = 0;
Simg_start_addrS = NULL;
Simg_end_addrS = NULL;
Simg_heap_sizeS = 0;
Ssmall_img_start_addrS = NULL;
Ssmall_img_hp_addrS = NULL;
Ssmall_img_end_addrS = NULL;
Ssmall_img_heap_sizeS = 0;
Slarge_img_start_addrS = NULL;
Slarge_img_hp_addrS = NULL;
Slarge_img_end_addrS = NULL;
Slarge_img_heap_sizeS = 0;
Sweaks_img_start_addrS = NULL;
Sweaks_img_hp_addrS = NULL;
Sweaks_img_end_addrS = NULL;
Sweaks_img_heap_sizeS = 0;
SoffsetS = 0;
SinitializingPS = 1;
SstatusS = NO_ERRORS;
image_start_address = 0;
Hthe_record_type270 = 1;
}
