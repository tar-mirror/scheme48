#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "prescheme.h"
#include <string.h>
#include <stdlib.h>
#include "scheme48vm.h"
#include "scheme48heap.h"
#include "scheme48write-barrier.h"
#include "bibop/bibop.h"

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
struct bibop_areas {
  long *small;
  long small_index;
  long *large;
  long large_index;
  long *weaks;
  long weaks_index;
};
static struct image_location *table_ref(struct table*, long);
static long allocate_stob(char, long, long, char);
static long image_alloc(long, long);
static void write_descriptor(long);
static void table_setB(struct table*, long, struct image_location*);
static char resumer_recordP(long);
static long trace_image_value(long);
long s48_max_heap_size(void);
void s48_set_max_heap_sizeB(long);
long s48_min_heap_size(void);
char * s48_get_new_small_start_addr(void);
void s48_set_new_small_start_addrB(char *);
char * s48_get_new_large_start_addr(void);
void s48_set_new_large_start_addrB(char *);
char * s48_get_new_weaks_start_addr(void);
void s48_set_new_weaks_start_addrB(char *);
void s48_initialization_completeB(void);
long s48_startup_procedure(void);
long s48_initial_symbols(void);
long s48_initial_imported_bindings(void);
long s48_initial_exported_bindings(void);
long s48_resumer_records(void);
void s48_initializing_gc_root(void);
void s48_set_image_valuesB(long, long, long, long, long);
void s48_trace_continuation_contentsB(char *, char *, long);
long s48_allocate_stob(long, long);
long s48_allocate_weak_stob(long, long);
long s48_allocate_unmovable_stob(long, long);
void s48_trace_continuation(char *, long);
long s48_read_image(char*, long);
long s48_write_image(long, long, FILE *);
static long Smax_heap_sizeS;
static long Smin_heap_sizeS;
static char * Snew_small_start_addrS;
static char * Snew_large_start_addrS;
static char * Snew_weaks_start_addrS;
static char * *Spure_areasS;
static char * *Simpure_areasS;
static long *Spure_sizesS;
static long *Simpure_sizesS;
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
static long small_image_start_address;
static char * Ssmall_image_beginS;
static char * Ssmall_image_hpS;
static char * Ssmall_image_endS;
static char * Slarge_image_beginS;
static char * Slarge_image_hpS;
static char * Slarge_image_endS;
static char * Sweaks_image_beginS;
static char * Sweaks_image_hpS;
static char * Sweaks_image_endS;
static struct bibop_areas *Sbibop_areasS;
static char Straced_last_stobPS;
static long Sstob_table_obj_nrS;
static struct table *Sstob_tableS;
static long Sfirst_stobS;
static struct image_location *Slast_stobS;
static long Sresumer_countS;
static long Sresumer_recordsS;
static long Sundumpable_recordsS;
static long Sundumpable_countS;

static struct image_location *table_ref(struct table *table_0X, long key_1X)
{
  long arg0K0;
  long next_5X;
  long i_4X;
  long *keys_3X;
  long size_2X;
 {  if ((0 < (table_0X->size))) {
    size_2X = table_0X->size;
    keys_3X = table_0X->keys;
    arg0K0 = ((key_1X ^ ((PS_SHIFT_LEFT_INLINE(key_1X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_1X, 10)))) & (-1 + size_2X));
    goto L3657;}
  else {
    return (NULL);}}
 L3657: {
  i_4X = arg0K0;
  next_5X = *(keys_3X + i_4X);
  if ((key_1X == next_5X)) {
    return (*((table_0X->values) + i_4X));}
  else {
    if ((0 == next_5X)) {
      if ((i_4X == (table_0X->size))) {
        arg0K0 = 0;
        goto L3657;}
      else {
        return (NULL);}}
    else {
      arg0K0 = (1 + i_4X);
      goto L3657;}}}
}
static long allocate_stob(char weakP_6X, long type_7X, long size_8X, char unmovableP_9X)
{
  long arg0K0;
  char * arg1K0;
  char * thing_18X;
  char * v_17X;
  char * v_16X;
  char * v_15X;
  char * v_14X;
  char * v_13X;
  long needed_12X;
  long length_in_bytes_11X;
  char tracedP_10X;
 {  tracedP_10X = type_7X < 16;
  if (tracedP_10X) {
    arg0K0 = (PS_SHIFT_LEFT_INLINE(size_8X, 2));
    goto L4145;}
  else {
    arg0K0 = size_8X;
    goto L4145;}}
 L4145: {
  length_in_bytes_11X = arg0K0;
  needed_12X = 4 + length_in_bytes_11X;
  if (weakP_6X) {
    v_13X = s48_allocate_weakAgc(needed_12X);
    arg1K0 = v_13X;
    goto L4173;}
  else {
    if (tracedP_10X) {
      if (unmovableP_9X) {
        v_14X = s48_allocate_untraced_unmovableAgc(needed_12X);
        arg1K0 = v_14X;
        goto L4173;}
      else {
        v_15X = s48_allocate_tracedAgc(needed_12X);
        arg1K0 = v_15X;
        goto L4173;}}
    else {
      if (unmovableP_9X) {
        v_16X = s48_allocate_untraced_unmovableAgc(needed_12X);
        arg1K0 = v_16X;
        goto L4173;}
      else {
        v_17X = s48_allocate_untracedAgc(needed_12X);
        arg1K0 = v_17X;
        goto L4173;}}}}
 L4173: {
  thing_18X = arg1K0;
  if ((thing_18X == NULL)) {
    ps_error("insufficient heap space for external allocation", 0);
    goto L4185;}
  else {
    goto L4185;}}
 L4185: {
  *((long *) thing_18X) = (long) ((2 + (PS_SHIFT_LEFT_INLINE(((PS_SHIFT_LEFT_INLINE(length_in_bytes_11X, 6)) + type_7X), 2))));
  return (3 + (((long) (thing_18X + 4))));}
}
static long image_alloc(long type_size_19X, long length_in_a_units_20X)
{
  char * arg1K0;
  char * data_addr_22X;
  char * image_hp_21X;
 {  if ((0 == type_size_19X)) {
    arg1K0 = (Ssmall_image_hpS);
    goto L4237;}
  else {
    if ((1 == type_size_19X)) {
      arg1K0 = (Slarge_image_hpS);
      goto L4237;}
    else {
      if ((2 == type_size_19X)) {
        arg1K0 = (Sweaks_image_hpS);
        goto L4237;}
      else {
        ps_error("invalid area tag", 0);
        arg1K0 = (Sweaks_image_hpS);
        goto L4237;}}}}
 L4237: {
  image_hp_21X = arg1K0;
  data_addr_22X = image_hp_21X + 4;
  if ((0 == type_size_19X)) {
    Ssmall_image_hpS = (data_addr_22X + length_in_a_units_20X);
    goto L4256;}
  else {
    if ((1 == type_size_19X)) {
      Slarge_image_hpS = (data_addr_22X + length_in_a_units_20X);
      goto L4256;}
    else {
      if ((2 == type_size_19X)) {
        Sweaks_image_hpS = (data_addr_22X + length_in_a_units_20X);
        goto L4256;}
      else {
        goto L4256;}}}}
 L4256: {
  return (3 + (((long) data_addr_22X)));}
}
static void write_descriptor(long descriptor_23X)
{
  long have_24X;
 {  *((long *) (Simage_buffer_pointerS)) = (long) (descriptor_23X);
  Simage_buffer_pointerS = ((Simage_buffer_pointerS) + 4);
  if ((4096 == ((Simage_buffer_pointerS) - (Simage_bufferS)))) {
    have_24X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_24X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_24X));
        goto L7383;}
      else {
        goto L7383;}}
    else {
      return;}}
  else {
    return;}}
 L7383: {
  Simage_buffer_pointerS = (Simage_bufferS);
  return;}
}
static void table_setB(struct table *table_25X, long key_26X, struct image_location *value_27X)
{
  long arg0K0;
  long i_43X;
  struct image_location *value_42X;
  long key_41X;
  long i_40X;
  long i_39X;
  struct image_location **new_values_38X;
  long *new_keys_37X;
  long new_size_36X;
  struct image_location **old_values_35X;
  long v_34X;
  long old_size_33X;
  long *old_keys_32X;
  long next_31X;
  long i_30X;
  long *keys_29X;
  long size_28X;
 {  if ((0 < (table_25X->size))) {
    size_28X = table_25X->size;
    keys_29X = table_25X->keys;
    arg0K0 = ((key_26X ^ ((PS_SHIFT_LEFT_INLINE(key_26X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_26X, 10)))) & (-1 + size_28X));
    goto L7774;}
  else {
    return;}}
 L7774: {
  i_30X = arg0K0;
  next_31X = *(keys_29X + i_30X);
  if ((key_26X == next_31X)) {
    *((table_25X->values) + i_30X) = value_27X;
    return;}
  else {
    if ((0 == next_31X)) {
      if ((i_30X == (table_25X->size))) {
        arg0K0 = 0;
        goto L7774;}
      else {
        *((table_25X->keys) + i_30X) = key_26X;
        *((table_25X->values) + i_30X) = value_27X;
        table_25X->count = (1 + (table_25X->count));
        if (((table_25X->count) == ((table_25X->size) / 3))) {
          old_keys_32X = table_25X->keys;
          old_size_33X = table_25X->size;
          v_34X = table_25X->size;
          old_values_35X = table_25X->values;
          new_size_36X = PS_SHIFT_LEFT_INLINE(v_34X, 1);
          new_keys_37X = (long*)malloc(sizeof(long) * (1 + new_size_36X));
          new_values_38X = (struct image_location**)malloc(sizeof(struct image_location*) * new_size_36X);
          if ((NULL == new_keys_37X)) {
            goto L5281;}
          else {
            if ((NULL == new_values_38X)) {
              goto L5281;}
            else {
              table_25X->keys = new_keys_37X;
              table_25X->values = new_values_38X;
              table_25X->size = new_size_36X;
              table_25X->count = 0;
              arg0K0 = 0;
              goto L5434;}}}
        else {
          return;}}}
    else {
      arg0K0 = (1 + i_30X);
      goto L7774;}}}
 L5281: {
  if ((NULL == new_keys_37X)) {
    goto L5289;}
  else {
    free(new_keys_37X);
    goto L5289;}}
 L5434: {
  i_39X = arg0K0;
  if ((i_39X < (1 + new_size_36X))) {
    *(new_keys_37X + i_39X) = 0;
    arg0K0 = (1 + i_39X);
    goto L5434;}
  else {
    arg0K0 = 0;
    goto L5312;}}
 L5289: {
  if ((NULL == new_values_38X)) {
    goto L5297;}
  else {
    free(new_values_38X);
    goto L5297;}}
 L5312: {
  i_40X = arg0K0;
  if ((i_40X == old_size_33X)) {
    free(old_keys_32X);
    free(old_values_35X);
    return;}
  else {
    key_41X = *(old_keys_32X + i_40X);
    if ((0 == key_41X)) {
      goto L5328;}
    else {
      value_42X = *(old_values_35X + i_40X);
      arg0K0 = ((key_41X ^ ((PS_SHIFT_LEFT_INLINE(key_41X, 1)) ^ (PS_SHIFT_RIGHT_INLINE(key_41X, 10)))) & (-1 + new_size_36X));
      goto L5456;}}}
 L5297: {
  table_25X->size = 0;
  return;}
 L5328: {
  arg0K0 = (1 + i_40X);
  goto L5312;}
 L5456: {
  i_43X = arg0K0;
  if ((0 == (*(new_keys_37X + i_43X)))) {
    if ((i_43X == new_size_36X)) {
      arg0K0 = 0;
      goto L5456;}
    else {
      *(new_keys_37X + i_43X) = key_41X;
      *(new_values_38X + i_43X) = value_42X;
      goto L5328;}}
  else {
    arg0K0 = (1 + i_43X);
    goto L5456;}}
}
static char resumer_recordP(long stob_44X)
{
  long type_45X;
 {  if ((3 == (3 & stob_44X))) {
    if ((9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + stob_44X))))), 2))))) {
      type_45X = *((long *) (((char *) (-3 + stob_44X))));
      if ((3 == (3 & type_45X))) {
        if ((9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + type_45X))))), 2))))) {
          return (3 == (3 & (*((long *) ((((char *) (-3 + type_45X))) + 4)))));}
        else {
          return 0;}}
      else {
        return 0;}}
    else {
      return 0;}}
  else {
    return 0;}}
}
static long trace_image_value(long thing_46X)
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
  long x_47X;
  long header_70X;
  long type_size_69X;
  char v_68X;
  long x_67X;
  struct image_location *new_66X;
  long new_descriptor_65X;
  struct image_location *new_64X;
  struct image_location *image_location_63X;
  long new_descriptor_62X;
  long v_61X;
  long h_60X;
  long stob_59X;
  struct image_location *image_location_58X;
  long new_alias_57X;
  char * addr_56X;
  long x_55X;
  long i_54X;
  long len_53X;
  long vector_52X;
  char v_51X;
  long type_50X;
  char v_49X;
  struct image_location *have_48X;
 {  if ((3 == (3 & thing_46X))) {
    have_48X = table_ref((Sstob_tableS), thing_46X);
    if ((NULL == have_48X)) {
      merged_arg0K0 = thing_46X;
#ifdef USE_DIRECT_THREADING
      gc_recordP_return_address = &&gc_recordP_return_0;
#else
      gc_recordP_return_tag = 0;
#endif
      goto gc_recordP;
     gc_recordP_return_0:
      v_49X = gc_recordP0_return_value;
      if (v_49X) {
        type_50X = *((long *) (((char *) (-3 + thing_46X))));
        merged_arg0K0 = type_50X;
#ifdef USE_DIRECT_THREADING
        gc_recordP_return_address = &&gc_recordP_return_1;
#else
        gc_recordP_return_tag = 1;
#endif
        goto gc_recordP;
       gc_recordP_return_1:
        v_51X = gc_recordP0_return_value;
        if (v_51X) {
          if ((1 == (*((long *) ((((char *) (-3 + type_50X))) + 4))))) {
            if (((Sundumpable_countS) < (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-7 + (Sundumpable_recordsS)))))), 8))), 2)))) {
              vector_52X = Sundumpable_recordsS;
              len_53X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-7 + vector_52X))))), 8))), 2);
              arg0K0 = 0;
              goto L12325;}
            else {
              goto L15656;}}
          else {
            arg0K0 = thing_46X;
            goto L12850;}}
        else {
          arg0K0 = thing_46X;
          goto L12850;}}
      else {
        arg0K0 = thing_46X;
        goto L12850;}}
    else {
      return (have_48X->new_descriptor);}}
  else {
    return thing_46X;}}
 L12325: {
  i_54X = arg0K0;
  if ((i_54X == len_53X)) {
    x_55X = Sundumpable_recordsS;
    addr_56X = (((char *) (-3 + x_55X))) + (PS_SHIFT_LEFT_INLINE((Sundumpable_countS), 2));S48_WRITE_BARRIER(x_55X, addr_56X, thing_46X);
    *((long *) addr_56X) = (long) (thing_46X);
    Sundumpable_countS = (1 + (Sundumpable_countS));
    goto L15656;}
  else {
    if (((*((long *) ((((char *) (-3 + vector_52X))) + (PS_SHIFT_LEFT_INLINE(i_54X, 2))))) == thing_46X)) {
      goto L15656;}
    else {
      arg0K0 = (1 + i_54X);
      goto L12325;}}}
 L15656: {
  new_alias_57X = trace_image_value((*((long *) ((((char *) (-3 + thing_46X))) + 4))));
  image_location_58X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_58X)) {
    arg2K0 = image_location_58X;
    goto L15660;}
  else {
    image_location_58X->new_descriptor = new_alias_57X;
    image_location_58X->next = 0;
    arg2K0 = image_location_58X;
    goto L15660;}}
 L12850: {
  stob_59X = arg0K0;
  h_60X = *((long *) (((char *) (-7 + stob_59X))));
  v_61X = s48_area_type_size(stob_59X);
  new_descriptor_62X = image_alloc(v_61X, (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(h_60X, 8)))));
  image_location_63X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_63X)) {
    arg0K0 = new_descriptor_62X;
    arg2K1 = image_location_63X;
    goto L12854;}
  else {
    image_location_63X->new_descriptor = new_descriptor_62X;
    image_location_63X->next = 0;
    arg0K0 = new_descriptor_62X;
    arg2K1 = image_location_63X;
    goto L12854;}}
 L15660: {
  new_64X = arg2K0;
  if ((NULL == new_64X)) {
    (Sstob_tableS)->size = 0;
    return new_alias_57X;}
  else {table_setB((Sstob_tableS), thing_46X, new_64X);
    return new_alias_57X;}}
 L12854: {
  new_descriptor_65X = arg0K0;
  new_66X = arg2K1;
  if ((NULL == new_66X)) {
    (Sstob_tableS)->size = 0;
    return new_descriptor_65X;}
  else {
    x_67X = Sfirst_stobS;
    if ((1 == x_67X)) {
      Sfirst_stobS = stob_59X;
      goto L12875;}
    else {
      (Slast_stobS)->next = stob_59X;
      goto L12875;}}}
 L12875: {
  Slast_stobS = new_66X;
  new_66X->next = 1;table_setB((Sstob_tableS), stob_59X, new_66X);
  v_68X = resumer_recordP(stob_59X);
  if (v_68X) {
    Sresumer_countS = (1 + (Sresumer_countS));
    goto L12891;}
  else {
    goto L12891;}}
 L12891: {
  Sstob_table_obj_nrS = (1 + (Sstob_table_obj_nrS));
  type_size_69X = s48_area_type_size(stob_59X);
  if ((0 == type_size_69X)) {
    if ((Straced_last_stobPS)) {
      *(((Sbibop_areasS)->small) + ((Sbibop_areasS)->small_index)) = 1;
      (Sbibop_areasS)->small_index = (1 + ((Sbibop_areasS)->small_index));
      goto L4436;}
    else {
      goto L4436;}}
  else {
    if ((1 == type_size_69X)) {
      if ((Straced_last_stobPS)) {
        *(((Sbibop_areasS)->large) + ((Sbibop_areasS)->large_index)) = 1;
        (Sbibop_areasS)->large_index = (1 + ((Sbibop_areasS)->large_index));
        goto L4452;}
      else {
        goto L4452;}}
    else {
      if ((2 == type_size_69X)) {
        if ((Straced_last_stobPS)) {
          *(((Sbibop_areasS)->weaks) + ((Sbibop_areasS)->weaks_index)) = 1;
          (Sbibop_areasS)->weaks_index = (1 + ((Sbibop_areasS)->weaks_index));
          goto L4468;}
        else {
          goto L4468;}}
      else {
        ps_error("Unexpected area type size!", 0);
        return new_descriptor_65X;}}}}
 L4436: {
  *(((Sbibop_areasS)->small) + ((Sbibop_areasS)->small_index)) = stob_59X;
  (Sbibop_areasS)->small_index = (1 + ((Sbibop_areasS)->small_index));
  return new_descriptor_65X;}
 L4452: {
  *(((Sbibop_areasS)->large) + ((Sbibop_areasS)->large_index)) = stob_59X;
  (Sbibop_areasS)->large_index = (1 + ((Sbibop_areasS)->large_index));
  return new_descriptor_65X;}
 L4468: {
  *(((Sbibop_areasS)->weaks) + ((Sbibop_areasS)->weaks_index)) = stob_59X;
  (Sbibop_areasS)->weaks_index = (1 + ((Sbibop_areasS)->weaks_index));
  return new_descriptor_65X;}
 gc_recordP: {
  x_47X = merged_arg0K0;{
  if ((3 == (3 & x_47X))) {
    header_70X = *((long *) (((char *) (-7 + x_47X))));
    if ((3 == (3 & header_70X))) {
      if ((3 == (3 & header_70X))) {
        gc_recordP0_return_value = (9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + header_70X))))), 2))));
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
      if ((3 == (3 & x_47X))) {
        gc_recordP0_return_value = (9 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + x_47X))))), 2))));
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
long s48_max_heap_size(void)
{

 {  return (Smax_heap_sizeS);}
}
void s48_set_max_heap_sizeB(long size_71X)
{

 {  Smax_heap_sizeS = size_71X;
  return;}
}
long s48_min_heap_size(void)
{

 {  return (Smin_heap_sizeS);}
}
char * s48_get_new_small_start_addr(void)
{

 {  return (Snew_small_start_addrS);}
}
void s48_set_new_small_start_addrB(char * addr_72X)
{

 {  Snew_small_start_addrS = addr_72X;
  return;}
}
char * s48_get_new_large_start_addr(void)
{

 {  return (Snew_large_start_addrS);}
}
void s48_set_new_large_start_addrB(char * addr_73X)
{

 {  Snew_large_start_addrS = addr_73X;
  return;}
}
char * s48_get_new_weaks_start_addr(void)
{

 {  return (Snew_weaks_start_addrS);}
}
void s48_set_new_weaks_start_addrB(char * addr_74X)
{

 {  Snew_weaks_start_addrS = addr_74X;
  return;}
}
void s48_initialization_completeB(void)
{

 {  SinitializingPS = 0;
  return;}
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
void s48_initializing_gc_root(void)
{
  long expr_79X;
  long expr_78X;
  long expr_77X;
  long expr_76X;
  long expr_75X;
 {  if ((SinitializingPS)) {
    expr_75X = s48_trace_value((Sstartup_procedureS));
    Sstartup_procedureS = expr_75X;
    expr_76X = s48_trace_value((SsymbolsS));
    SsymbolsS = expr_76X;
    expr_77X = s48_trace_value((Simported_bindingsS));
    Simported_bindingsS = expr_77X;
    expr_78X = s48_trace_value((Sexported_bindingsS));
    Sexported_bindingsS = expr_78X;
    expr_79X = s48_trace_value((Sresumer_recordsS));
    Sresumer_recordsS = expr_79X;
    return;}
  else {
    return;}}
}
void s48_set_image_valuesB(long startup_proc_80X, long symbols_81X, long imports_82X, long exports_83X, long records_84X)
{

 {  Sstartup_procedureS = startup_proc_80X;
  SsymbolsS = symbols_81X;
  Simported_bindingsS = imports_82X;
  Sexported_bindingsS = exports_83X;
  Sresumer_recordsS = records_84X;
  return;}
}
void s48_trace_continuation_contentsB(char * contents_pointer_85X, char * code_pointer_86X, long mask_size_87X)
{
  char * arg1K0;
  char * arg1K1;
  long arg0K0;
  char * ptr_92X;
  long mask_91X;
  char * trace_ptr_90X;
  char * mask_ptr_89X;
  char * mask_pointer_88X;
 {  mask_pointer_88X = code_pointer_86X + -7;
  arg1K0 = (mask_pointer_88X + (0 - mask_size_87X));
  arg1K1 = contents_pointer_85X;
  goto L3100;}
 L3100: {
  mask_ptr_89X = arg1K0;
  trace_ptr_90X = arg1K1;
  if ((mask_ptr_89X == mask_pointer_88X)) {
    return;}
  else {
    arg0K0 = (*((unsigned char *) mask_ptr_89X));
    arg1K1 = trace_ptr_90X;
    goto L3108;}}
 L3108: {
  mask_91X = arg0K0;
  ptr_92X = arg1K1;
  if ((0 == mask_91X)) {
    arg1K0 = (mask_ptr_89X + 1);
    arg1K1 = (trace_ptr_90X + 32);
    goto L3100;}
  else {
    if ((1 == (1 & mask_91X))) {s48_trace_locationsB(ptr_92X, (ptr_92X + 4));
      goto L3124;}
    else {
      goto L3124;}}}
 L3124: {
  arg0K0 = (PS_SHIFT_RIGHT_INLINE(mask_91X, 1));
  arg1K1 = (ptr_92X + 4);
  goto L3108;}
}
long s48_allocate_stob(long type_93X, long size_94X)
{

 {  return allocate_stob(0, type_93X, size_94X, 0);}
}
long s48_allocate_weak_stob(long type_95X, long size_96X)
{

 {  return allocate_stob(1, type_95X, size_96X, 0);}
}
long s48_allocate_unmovable_stob(long type_97X, long size_98X)
{

 {  return allocate_stob(0, type_97X, size_98X, 1);}
}
void s48_trace_continuation(char * contents_pointer_99X, long size_100X)
{
  char * arg1K0;
  char * arg1K1;
  long arg0K0;
  char * ptr_108X;
  long mask_107X;
  char * trace_ptr_106X;
  char * mask_ptr_105X;
  char * mask_pointer_104X;
  char * data_pointer_103X;
  long mask_size_102X;
  char * code_pointer_101X;
 {  code_pointer_101X = (((char *) (-3 + (*((long *) (contents_pointer_99X + 4)))))) + (PS_SHIFT_RIGHT_INLINE((*((long *) contents_pointer_99X)), 2));
  mask_size_102X = *((unsigned char *) (code_pointer_101X + -3));
  if ((0 == mask_size_102X)) {s48_trace_locationsB(contents_pointer_99X, (contents_pointer_99X + size_100X));
    return;}
  else {
    data_pointer_103X = contents_pointer_99X + 12;s48_trace_locationsB(contents_pointer_99X, data_pointer_103X);
    mask_pointer_104X = code_pointer_101X + -7;
    arg1K0 = (mask_pointer_104X + (0 - mask_size_102X));
    arg1K1 = data_pointer_103X;
    goto L9553;}}
 L9553: {
  mask_ptr_105X = arg1K0;
  trace_ptr_106X = arg1K1;
  if ((mask_ptr_105X == mask_pointer_104X)) {
    return;}
  else {
    arg0K0 = (*((unsigned char *) mask_ptr_105X));
    arg1K1 = trace_ptr_106X;
    goto L9560;}}
 L9560: {
  mask_107X = arg0K0;
  ptr_108X = arg1K1;
  if ((0 == mask_107X)) {
    arg1K0 = (mask_ptr_105X + 1);
    arg1K1 = (trace_ptr_106X + 32);
    goto L9553;}
  else {
    if ((1 == (1 & mask_107X))) {s48_trace_locationsB(ptr_108X, (ptr_108X + 4));
      goto L9562;}
    else {
      goto L9562;}}}
 L9562: {
  arg0K0 = (PS_SHIFT_RIGHT_INLINE(mask_107X, 1));
  arg1K1 = (ptr_108X + 4);
  goto L9560;}
}
long s48_read_image(char *image_filename_109X, long max_heap_size_110X)
{
  struct image_location *arg2K0;
  struct table *arg5K0;
  char *arg4K1;
  char arg3K1;
  char arg3K0;
  char * arg1K0;
  long arg0K2;
  long arg0K1;
  long arg0K0;
  FILE * merged_arg6K2;
  struct table *merged_arg5K1;
  char * merged_arg1K2;
  char * merged_arg1K1;
  char * merged_arg1K0;
  long merged_arg0K2;
  long merged_arg0K1;
  long merged_arg0K0;

#ifdef USE_DIRECT_THREADING
  void *relocateD1_return_address;
#else
  int relocateD1_return_tag;
#endif
  long relocateD10_return_value;
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
  void *relocate_image_return_address;
#else
  int relocate_image_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *reverse_byte_orderB_return_address;
#else
  int reverse_byte_orderB_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *read_image_area_return_address;
#else
  int read_image_area_return_tag;
#endif
  long read_image_area0_return_value;
#ifdef USE_DIRECT_THREADING
  void *really_read_image_area_return_address;
#else
  int really_read_image_area_return_tag;
#endif
  long really_read_image_area0_return_value;
  long address_111X;
  char * addr_112X;
  long format_113X;
  char * from_addr_114X;
  char * to_addr_115X;
  long image_format_116X;
  long table_117X;
  struct table *stob_table_118X;
  long address_119X;
  long table_120X;
  long delta_121X;
  long delta_122X;
  char * start_123X;
  char * end_124X;
  char * start_125X;
  char * end_126X;
  char * new_start_addr_127X;
  long img_heap_size_128X;
  FILE * port_129X;
  char * new_start_addr_130X;
  long img_heap_size_131X;
  FILE * port_132X;
  struct image_location *image_location_518X;
  long v_517X;
  char * address_516X;
  char * y_515X;
  char * y_514X;
  char * v_513X;
  char * y_512X;
  char * index_511X;
  long v_510X;
  long v_509X;
  char * v_508X;
  long v_507X;
  char * new_address_506X;
  struct image_location *image_location_505X;
  long v_504X;
  struct image_location *val_503X;
  struct image_location *image_location_502X;
  long v_501X;
  char * v_500X;
  long v_499X;
  struct image_location *val_498X;
  struct image_location *val_497X;
  struct image_location *image_location_496X;
  char * new_address_495X;
  struct image_location *v_494X;
  long v_493X;
  char * address_492X;
  struct image_location *val_491X;
  struct image_location *image_location_490X;
  char * new_address_489X;
  struct image_location *image_location_488X;
  char * new_address_487X;
  long size_in_cells_486X;
  struct image_location *v_485X;
  long v_484X;
  struct image_location *image_location_483X;
  char * new_address_482X;
  long size_in_bytes_481X;
  long size_in_cells_480X;
  long header_cell_479X;
  char * x1_478X;
  char * current_addr_477X;
  struct image_location *v_476X;
  long v_475X;
  char * current_addr_474X;
  long size_in_cells_473X;
  long cell_472X;
  char * x1_471X;
  char * current_addr_470X;
  char * addr_469X;
  long val_468X;
  long next_467X;
  long next_466X;
  long link_465X;
  long entry_464X;
  char * addr_463X;
  long value_462X;
  long bucket_461X;
  long bucket_460X;
  long link_459X;
  long i_458X;
  struct image_location *image_location_457X;
  long v_456X;
  char * address_455X;
  char * addr_454X;
  long val_453X;
  long next_452X;
  long next_451X;
  long link_450X;
  long entry_449X;
  char * addr_448X;
  long value_447X;
  long bucket_446X;
  long bucket_445X;
  long link_444X;
  long i_443X;
  long d_442X;
  long descriptor_441X;
  char * ptr_440X;
  char * next_439X;
  long value_438X;
  long byte_a_437X;
  char * addr_b_436X;
  char * addr_a_435X;
  long j_434X;
  long i_433X;
  char * ptr_432X;
  long status_431X;
  long status_430X;
  char *string_429X;
  char okayP_428X;
  long status_427X;
  char eofP_426X;
  long got_425X;
  long need_424X;
  long status_423X;
  long status_422X;
  char *string_421X;
  char okayP_420X;
  long status_419X;
  char eofP_418X;
  long got_417X;
  long need_416X;
  char * addr_415X;
  long val_414X;
  long next_413X;
  long next_412X;
  struct image_location *image_location_411X;
  long v_410X;
  char * addr_409X;
  long val_408X;
  long link_407X;
  long entry_406X;
  char * address_405X;
  long cell_404X;
  long size_403X;
  char * address_402X;
  long i_401X;
  long next_400X;
  long next_399X;
  char * addr_398X;
  long value_397X;
  long cell_396X;
  long resumer_records_395X;
  long expr_394X;
  struct image_location **values_393X;
  long *keys_392X;
  struct table *table_391X;
  long link_390X;
  long entry_389X;
  long bucket_388X;
  long bucket_387X;
  long expr_386X;
  long v_385X;
  char * pointer_384X;
  long size_383X;
  long cell_382X;
  char * addr_381X;
  long tab_380X;
  char * addr_379X;
  long value_378X;
  long link_377X;
  long i_376X;
  long expr_375X;
  long v_374X;
  char * pointer_373X;
  long size_372X;
  long cell_371X;
  char * addr_370X;
  long tab_369X;
  long v_368X;
  long bucket_367X;
  long bucket_366X;
  struct table *stob_table_365X;
  long table_364X;
  long expr_363X;
  long v_362X;
  char * pointer_361X;
  long size_360X;
  long cell_359X;
  char * addr_358X;
  long tab_357X;
  long v_356X;
  long link_355X;
  long i_354X;
  long v_353X;
  char * pointer_352X;
  long size_351X;
  long cell_350X;
  char * addr_349X;
  long tab_348X;
  long v_347X;
  struct image_location *image_location_346X;
  long v_345X;
  long table_344X;
  long expr_343X;
  char * address_342X;
  long descriptor_341X;
  long expr_340X;
  long status_339X;
  char eofP_338X;
  long descriptor_337X;
  long expr_336X;
  long status_335X;
  long status_334X;
  long status_333X;
  char * v_332X;
  char * v_331X;
  long expr_330X;
  long status_329X;
  long status_328X;
  char eofP_327X;
  char ch_326X;
  long we_325X;
  long cells_324X;
  long descriptor_323X;
  long expr_322X;
  long status_321X;
  long status_320X;
  long status_319X;
  long status_318X;
  long status_317X;
  long status_316X;
  char eofP_315X;
  char v_314X;
  long expr_313X;
  long status_312X;
  char *string_311X;
  char okayP_310X;
  long expr_309X;
  long status_308X;
  char eofP_307X;
  long thing_306X;
  long status_305X;
  long wh_304X;
  long cells_303X;
  long descriptor_302X;
  long expr_301X;
  long status_300X;
  long expr_299X;
  long expr_298X;
  long expr_297X;
  long status_296X;
  char eofP_295X;
  long got_294X;
  long need_293X;
  struct table *v_292X;
  long status_291X;
  char eofP_290X;
  long thing_289X;
  long status_288X;
  long expr_287X;
  long status_286X;
  char eofP_285X;
  long thing_284X;
  long status_283X;
  long le_282X;
  long cells_281X;
  long status_280X;
  long status_279X;
  long status_278X;
  long expr_277X;
  long descriptor_276X;
  struct table *table_275X;
  long i_274X;
  long status_273X;
  char eofP_272X;
  long thing_271X;
  long status_270X;
  long expr_269X;
  long status_268X;
  char eofP_267X;
  long thing_266X;
  long status_265X;
  long lh_264X;
  long cells_263X;
  long status_262X;
  long status_261X;
  long status_260X;
  long status_259X;
  long status_258X;
  long status_257X;
  long status_256X;
  long *keys_255X;
  char * v_254X;
  long status_253X;
  char eofP_252X;
  long thing_251X;
  long status_250X;
  long expr_249X;
  long status_248X;
  char eofP_247X;
  long thing_246X;
  long status_245X;
  long se_244X;
  long cells_243X;
  long v_242X;
  long byte_a_241X;
  char * addr_b_240X;
  char * addr_a_239X;
  long j_238X;
  long i_237X;
  long status_236X;
  long status_235X;
  long status_234X;
  long status_233X;
  long status_232X;
  long status_231X;
  char eofP_230X;
  char v_229X;
  long expr_228X;
  long expr_227X;
  long expr_226X;
  char * weaks_new_hp_225X;
  long weaks_delta_224X;
  char * large_new_hp_223X;
  long large_delta_222X;
  char * small_new_hp_221X;
  long small_delta_220X;
  long v_219X;
  char reverse_byte_orderP_218X;
  long status_217X;
  char eofP_216X;
  long thing_215X;
  long status_214X;
  long expr_213X;
  long status_212X;
  char eofP_211X;
  long thing_210X;
  long status_209X;
  long sh_208X;
  long cells_207X;
  long cells_206X;
  long status_205X;
  long status_204X;
  char eofP_203X;
  long thing_202X;
  long status_201X;
  long status_200X;
  char eofP_199X;
  long thing_198X;
  long status_197X;
  long sb_196X;
  long cells_195X;
  long status_194X;
  char eofP_193X;
  long thing_192X;
  long status_191X;
  long cells_190X;
  long status_189X;
  char eofP_188X;
  long got_187X;
  char * new_addr_186X;
  long image_size_185X;
  long format_184X;
  long status_183X;
  long status_182X;
  long status_181X;
  long status_180X;
  long status_179X;
  long status_178X;
  char eofP_177X;
  long thing_176X;
  long status_175X;
  long status_174X;
  char eofP_173X;
  long thing_172X;
  long status_171X;
  long status_170X;
  long status_169X;
  long status_168X;
  long status_167X;
  long status_166X;
  long old_bytes_per_cell_165X;
  long status_164X;
  char eofP_163X;
  char thing_162X;
  long status_161X;
  char eofP_160X;
  long thing_159X;
  long status_158X;
  long format_157X;
  long status_156X;
  char eofP_155X;
  long status_154X;
  char eofP_153X;
  char ch_152X;
  long i_151X;
  long status_150X;
  char eofP_149X;
  long n_148X;
  long status_147X;
  char same_versionP_146X;
  long status_145X;
  char eofP_144X;
  long status_143X;
  char eofP_142X;
  char ch_141X;
  long status_140X;
  long status_139X;
  char eofP_138X;
  char ch_137X;
  long status_136X;
  long status_135X;
  long status_134X;
  FILE * port_133X;
 {  SstatusS = NO_ERRORS;
  SeofPS = 0;
  port_133X = ps_open_input_file(image_filename_109X, &status_134X);
  if ((status_134X == NO_ERRORS)) {
    status_135X = SstatusS;
    if ((status_135X == NO_ERRORS)) {
      if ((SeofPS)) {
        goto L7879;}
      else {
        goto L9402;}}
    else {
      goto L7879;}}
  else {
    ps_write_string("Can't open heap image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    ps_write_string((ps_error_string(status_134X)), (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    return -1;}}
 L7879: {
  status_136X = SstatusS;
  if ((status_136X == NO_ERRORS)) {
    if ((SeofPS)) {
      goto L7930;}
    else {
      goto L9322;}}
  else {
    goto L7930;}}
 L9402: {
  PS_READ_CHAR(port_133X, ch_137X, eofP_138X, status_139X)
  if (eofP_138X) {
    arg3K0 = eofP_138X;
    arg0K1 = status_139X;
    goto L7853;}
  else {
    if ((status_139X == NO_ERRORS)) {
      if ((12 == ch_137X)) {
        arg3K0 = 0;
        arg0K1 = status_139X;
        goto L7853;}
      else {
        goto L9402;}}
    else {
      arg3K0 = eofP_138X;
      arg0K1 = status_139X;
      goto L7853;}}}
 L7930: {
  status_140X = SstatusS;
  if ((status_140X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg3K0 = 0;
      goto L7981;}
    else {
      arg0K0 = 0;
      goto L9256;}}
  else {
    arg3K0 = 0;
    goto L7981;}}
 L9322: {
  PS_READ_CHAR(port_133X, ch_141X, eofP_142X, status_143X)
  if (eofP_142X) {
    arg3K0 = eofP_142X;
    arg0K1 = status_143X;
    goto L7904;}
  else {
    if ((status_143X == NO_ERRORS)) {
      if ((10 == ch_141X)) {
        arg3K0 = 0;
        arg0K1 = status_143X;
        goto L7904;}
      else {
        goto L9322;}}
    else {
      arg3K0 = eofP_142X;
      arg0K1 = status_143X;
      goto L7904;}}}
 L7853: {
  eofP_144X = arg3K0;
  status_145X = arg0K1;
  if (eofP_144X) {
    SeofPS = 1;
    goto L7879;}
  else {
    if ((status_145X == NO_ERRORS)) {
      goto L7879;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_145X;
        goto L7879;}
      else {
        goto L7879;}}}}
 L7981: {
  same_versionP_146X = arg3K0;
  status_147X = SstatusS;
  if ((status_147X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8034;}
    else {
      PS_READ_INTEGER(port_133X, n_148X, eofP_149X, status_150X)
      if (eofP_149X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8034;}
      else {
        if ((status_150X == NO_ERRORS)) {
          arg0K0 = n_148X;
          goto L8034;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_150X;
            arg0K0 = -1;
            goto L8034;}
          else {
            arg0K0 = -1;
            goto L8034;}}}}}
  else {
    arg0K0 = -1;
    goto L8034;}}
 L9256: {
  i_151X = arg0K0;
  PS_READ_CHAR(port_133X, ch_152X, eofP_153X, status_154X)
  if (eofP_153X) {
    arg3K0 = 0;
    arg3K1 = eofP_153X;
    arg0K2 = status_154X;
    goto L7955;}
  else {
    if ((status_154X == NO_ERRORS)) {
      if ((i_151X == (strlen((char *) "Vanilla 40")))) {
        arg3K0 = (10 == ch_152X);
        arg3K1 = 0;
        arg0K2 = status_154X;
        goto L7955;}
      else {
        if ((ch_152X == (*("Vanilla 40" + i_151X)))) {
          arg0K0 = (1 + i_151X);
          goto L9256;}
        else {
          arg3K0 = 0;
          arg3K1 = 0;
          arg0K2 = status_154X;
          goto L7955;}}}
    else {
      arg3K0 = 0;
      arg3K1 = eofP_153X;
      arg0K2 = status_154X;
      goto L7955;}}}
 L7904: {
  eofP_155X = arg3K0;
  status_156X = arg0K1;
  if (eofP_155X) {
    SeofPS = 1;
    goto L7930;}
  else {
    if ((status_156X == NO_ERRORS)) {
      goto L7930;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_156X;
        goto L7930;}
      else {
        goto L7930;}}}}
 L8034: {
  format_157X = arg0K0;
  status_158X = SstatusS;
  if ((status_158X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8087;}
    else {
      PS_READ_INTEGER(port_133X, thing_159X, eofP_160X, status_161X)
      if (eofP_160X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8087;}
      else {
        if ((status_161X == NO_ERRORS)) {
          arg0K0 = thing_159X;
          goto L8087;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_161X;
            arg0K0 = -1;
            goto L8087;}
          else {
            arg0K0 = -1;
            goto L8087;}}}}}
  else {
    arg0K0 = -1;
    goto L8087;}}
 L7955: {
  thing_162X = arg3K0;
  eofP_163X = arg3K1;
  status_164X = arg0K2;
  if (eofP_163X) {
    SeofPS = 1;
    arg3K0 = 0;
    goto L7981;}
  else {
    if ((status_164X == NO_ERRORS)) {
      arg3K0 = thing_162X;
      goto L7981;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_164X;
        arg3K0 = 0;
        goto L7981;}
      else {
        arg3K0 = 0;
        goto L7981;}}}}
 L8087: {
  old_bytes_per_cell_165X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    if ((SeofPS)) {
      SstatusS = EDOM;
      ps_write_string("Premature EOF when reading image file", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_166X = SstatusS;
      if ((status_166X == NO_ERRORS)) {
        goto L8535;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L8535;}}
    else {
      if (same_versionP_146X) {
        if ((0 == format_157X)) {
          goto L8599;}
        else {
          if ((1 == format_157X)) {
            goto L8599;}
          else {
            ps_write_string("Unknown image format", (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            status_167X = SstatusS;
            if ((status_167X == NO_ERRORS)) {
              goto L8610;}
            else {
              ps_write_string((ps_error_string((SstatusS))), (stderr));
              { long ignoreXX;
              PS_WRITE_CHAR(10, (stderr), ignoreXX) }
              goto L8610;}}}}
      else {
        ps_write_string("Format of image is incompatible with this version of system", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        status_168X = SstatusS;
        if ((status_168X == NO_ERRORS)) {
          goto L8569;}
        else {
          ps_write_string((ps_error_string((SstatusS))), (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          goto L8569;}}}}
  else {
    ps_write_string("Error reading from image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_169X = SstatusS;
    if ((status_169X == NO_ERRORS)) {
      goto L8500;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L8500;}}}
 L8535: {
  status_170X = ps_close(port_133X);
  if ((status_170X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}}
 L8599: {
  if ((4 == old_bytes_per_cell_165X)) {
    if ((0 == format_157X)) {
      status_171X = SstatusS;
      if ((status_171X == NO_ERRORS)) {
        if ((SeofPS)) {
          arg0K0 = -1;
          goto L5765;}
        else {
          PS_READ_INTEGER(port_133X, thing_172X, eofP_173X, status_174X)
          if (eofP_173X) {
            SeofPS = 1;
            arg0K0 = -1;
            goto L5765;}
          else {
            if ((status_174X == NO_ERRORS)) {
              arg0K0 = thing_172X;
              goto L5765;}
            else {
              SeofPS = 1;
              if (((SstatusS) == NO_ERRORS)) {
                SstatusS = status_174X;
                arg0K0 = -1;
                goto L5765;}
              else {
                arg0K0 = -1;
                goto L5765;}}}}}
      else {
        arg0K0 = -1;
        goto L5765;}}
    else {
      if ((1 == format_157X)) {
        status_175X = SstatusS;
        if ((status_175X == NO_ERRORS)) {
          if ((SeofPS)) {
            arg0K0 = -1;
            goto L5883;}
          else {
            PS_READ_INTEGER(port_133X, thing_176X, eofP_177X, status_178X)
            if (eofP_177X) {
              SeofPS = 1;
              arg0K0 = -1;
              goto L5883;}
            else {
              if ((status_178X == NO_ERRORS)) {
                arg0K0 = thing_176X;
                goto L5883;}
              else {
                SeofPS = 1;
                if (((SstatusS) == NO_ERRORS)) {
                  SstatusS = status_178X;
                  arg0K0 = -1;
                  goto L5883;}
                else {
                  arg0K0 = -1;
                  goto L5883;}}}}}
        else {
          arg0K0 = -1;
          goto L5883;}}
      else {
        ps_error("check-all-data!: Unknown image format (this can't happen)", 0);
        goto L8129;}}}
  else {
    ps_write_string("Incompatible bytes-per-cell in image", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_179X = SstatusS;
    if ((status_179X == NO_ERRORS)) {
      goto L8644;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L8644;}}}
 L8610: {
  status_180X = ps_close(port_133X);
  if ((status_180X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}}
 L8569: {
  status_181X = ps_close(port_133X);
  if ((status_181X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}}
 L8500: {
  status_182X = ps_close(port_133X);
  if ((status_182X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}}
 L15143: {
  status_183X = arg0K0;
  format_184X = arg0K1;
  if ((0 == status_183X)) {
    if (((SstatusS) == NO_ERRORS)) {
      image_size_185X = Simg_heap_sizeS;
      Smax_heap_sizeS = max_heap_size_110X;
      Smin_heap_sizeS = (PS_SHIFT_LEFT_INLINE(image_size_185X, 2));s48_initialize_bibop_heap();
      Spure_areasS = ((char **)malloc(sizeof(char *) * 0));
      Simpure_areasS = (Spure_areasS);
      Spure_sizesS = ((long*)malloc(sizeof(long) * 0));
      Simpure_sizesS = (Spure_sizesS);s48_initialize_image_areas(((Ssmall_img_end_addrS) - (Ssmall_img_start_addrS)), ((Ssmall_img_hp_addrS) - (Ssmall_img_start_addrS)), ((Slarge_img_end_addrS) - (Slarge_img_start_addrS)), ((Slarge_img_hp_addrS) - (Slarge_img_start_addrS)), ((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS)), ((Sweaks_img_hp_addrS) - (Sweaks_img_start_addrS)));s48_check_heap_sizeB();
      if (((SstatusS) == NO_ERRORS)) {
        new_addr_186X = (char *)malloc(4);
        got_187X = ps_read_block(port_133X, ((char *) new_addr_186X), 4, &eofP_188X, &status_189X);
        if ((status_189X == NO_ERRORS)) {
          if (eofP_188X) {
            goto L5588;}
          else {
            if ((got_187X < 4)) {
              goto L5588;}
            else {
              goto L5540;}}}
        else {
          SstatusS = status_189X;
          goto L5588;}}
      else {
        return -1;}}
    else {
      return -1;}}
  else {
    return -1;}}
 L5765: {
  cells_190X = arg0K0;
  Simg_start_addrS = (((char *) (PS_SHIFT_LEFT_INLINE(cells_190X, 2))));
  status_191X = SstatusS;
  if ((status_191X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5820;}
    else {
      PS_READ_INTEGER(port_133X, thing_192X, eofP_193X, status_194X)
      if (eofP_193X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5820;}
      else {
        if ((status_194X == NO_ERRORS)) {
          arg0K0 = thing_192X;
          goto L5820;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_194X;
            arg0K0 = -1;
            goto L5820;}
          else {
            arg0K0 = -1;
            goto L5820;}}}}}
  else {
    arg0K0 = -1;
    goto L5820;}}
 L5883: {
  cells_195X = arg0K0;
  sb_196X = PS_SHIFT_LEFT_INLINE(cells_195X, 2);
  status_197X = SstatusS;
  if ((status_197X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5938;}
    else {
      PS_READ_INTEGER(port_133X, thing_198X, eofP_199X, status_200X)
      if (eofP_199X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5938;}
      else {
        if ((status_200X == NO_ERRORS)) {
          arg0K0 = thing_198X;
          goto L5938;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_200X;
            arg0K0 = -1;
            goto L5938;}
          else {
            arg0K0 = -1;
            goto L5938;}}}}}
  else {
    arg0K0 = -1;
    goto L5938;}}
 L8129: {
  status_201X = SstatusS;
  if ((status_201X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8180;}
    else {
      PS_READ_INTEGER(port_133X, thing_202X, eofP_203X, status_204X)
      if (eofP_203X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8180;}
      else {
        if ((status_204X == NO_ERRORS)) {
          arg0K0 = thing_202X;
          goto L8180;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_204X;
            arg0K0 = -1;
            goto L8180;}
          else {
            arg0K0 = -1;
            goto L8180;}}}}}
  else {
    arg0K0 = -1;
    goto L8180;}}
 L8644: {
  status_205X = ps_close(port_133X);
  if ((status_205X == NO_ERRORS)) {
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    arg0K1 = format_157X;
    goto L15143;}}
 L5588: {
  ps_error("byte order check failed", 0);
  goto L5540;}
 L5540: {
  if ((1 == (*((long *) new_addr_186X)))) {
    free(new_addr_186X);
    arg3K0 = 0;
    goto L15206;}
  else {
    arg0K0 = 0;
    arg0K1 = 3;
    goto L5601;}}
 L5820: {
  cells_206X = arg0K0;
  Simg_end_addrS = (((char *) (PS_SHIFT_LEFT_INLINE(cells_206X, 2))));
  Simg_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((3 + ((Simg_end_addrS) - (Simg_start_addrS))), 2));
  goto L8129;}
 L5938: {
  cells_207X = arg0K0;
  sh_208X = PS_SHIFT_LEFT_INLINE(cells_207X, 2);
  status_209X = SstatusS;
  if ((status_209X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L5993;}
    else {
      PS_READ_INTEGER(port_133X, thing_210X, eofP_211X, status_212X)
      if (eofP_211X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L5993;}
      else {
        if ((status_212X == NO_ERRORS)) {
          arg0K0 = thing_210X;
          goto L5993;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_212X;
            arg0K0 = -1;
            goto L5993;}
          else {
            arg0K0 = -1;
            goto L5993;}}}}}
  else {
    arg0K0 = -1;
    goto L5993;}}
 L8180: {
  expr_213X = arg0K0;
  SsymbolsS = expr_213X;
  status_214X = SstatusS;
  if ((status_214X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8233;}
    else {
      PS_READ_INTEGER(port_133X, thing_215X, eofP_216X, status_217X)
      if (eofP_216X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8233;}
      else {
        if ((status_217X == NO_ERRORS)) {
          arg0K0 = thing_215X;
          goto L8233;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_217X;
            arg0K0 = -1;
            goto L8233;}
          else {
            arg0K0 = -1;
            goto L8233;}}}}}
  else {
    arg0K0 = -1;
    goto L8233;}}
 L15206: {
  reverse_byte_orderP_218X = arg3K0;
  if ((0 == format_184X)) {
    Sheap_image_pointerS = NULL;
    Ssymbol_addressS = NULL;
    v_219X = SsymbolsS;
    if ((1 == v_219X)) {
      arg1K0 = (Simg_end_addrS);
      goto L7615;}
    else {
      arg1K0 = (((char *) (-7 + (SsymbolsS))));
      goto L7615;}}
  else {
    if ((1 == format_184X)) {
      small_delta_220X = (Snew_small_start_addrS) - (Ssmall_img_start_addrS);
      small_new_hp_221X = (Ssmall_img_hp_addrS) + small_delta_220X;
      large_delta_222X = (Snew_large_start_addrS) - (Slarge_img_start_addrS);
      large_new_hp_223X = (Slarge_img_hp_addrS) + large_delta_222X;
      weaks_delta_224X = (Snew_weaks_start_addrS) - (Sweaks_img_start_addrS);
      weaks_new_hp_225X = (Sweaks_img_hp_addrS) + weaks_delta_224X;
      merged_arg1K0 = (Snew_weaks_start_addrS);
      merged_arg0K1 = (Sweaks_img_heap_sizeS);
      merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
      read_image_area_return_address = &&read_image_area_return_0;
#else
      read_image_area_return_tag = 0;
#endif
      goto read_image_area;
     read_image_area_return_0:
      expr_226X = read_image_area0_return_value;
      if ((0 == expr_226X)) {
        if (((SstatusS) == NO_ERRORS)) {
          merged_arg1K0 = (Snew_large_start_addrS);
          merged_arg0K1 = (Slarge_img_heap_sizeS);
          merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
          read_image_area_return_address = &&read_image_area_return_1;
#else
          read_image_area_return_tag = 1;
#endif
          goto read_image_area;
         read_image_area_return_1:
          expr_227X = read_image_area0_return_value;
          if ((0 == expr_227X)) {
            if (((SstatusS) == NO_ERRORS)) {
              merged_arg1K0 = (Snew_small_start_addrS);
              merged_arg0K1 = (Ssmall_img_heap_sizeS);
              merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
              read_image_area_return_address = &&read_image_area_return_2;
#else
              read_image_area_return_tag = 2;
#endif
              goto read_image_area;
             read_image_area_return_2:
              expr_228X = read_image_area0_return_value;
              if ((0 == expr_228X)) {
                if (((SstatusS) == NO_ERRORS)) {
                  PS_READ_CHAR(port_133X, v_229X, eofP_230X, status_231X)
                  if ((status_231X == NO_ERRORS)) {
                    if (eofP_230X) {
                      status_232X = ps_close(port_133X);
                      if ((status_232X == NO_ERRORS)) {
                        if (reverse_byte_orderP_218X) {
                          merged_arg1K0 = (Snew_small_start_addrS);
                          merged_arg1K1 = small_new_hp_221X;
#ifdef USE_DIRECT_THREADING
                          reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_0;
#else
                          reverse_byte_orderB_return_tag = 0;
#endif
                          goto reverse_byte_orderB;
                         reverse_byte_orderB_return_0:
                          merged_arg1K0 = (Snew_large_start_addrS);
                          merged_arg1K1 = large_new_hp_223X;
#ifdef USE_DIRECT_THREADING
                          reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_1;
#else
                          reverse_byte_orderB_return_tag = 1;
#endif
                          goto reverse_byte_orderB;
                         reverse_byte_orderB_return_1:
                          merged_arg1K0 = (Snew_weaks_start_addrS);
                          merged_arg1K1 = weaks_new_hp_225X;
#ifdef USE_DIRECT_THREADING
                          reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_2;
#else
                          reverse_byte_orderB_return_tag = 2;
#endif
                          goto reverse_byte_orderB;
                         reverse_byte_orderB_return_2:
                          goto L13661;}
                        else {
                          goto L13661;}}
                      else {
                        ps_write_string("Error closing image file", (stderr));
                        { long ignoreXX;
                        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                        status_233X = SstatusS;
                        if ((status_233X == NO_ERRORS)) {
                          goto L13970;}
                        else {
                          ps_write_string((ps_error_string((SstatusS))), (stderr));
                          { long ignoreXX;
                          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                          goto L13970;}}}
                    else {
                      ps_write_string("Image file has extraneous data after image", (stderr));
                      { long ignoreXX;
                      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                      status_234X = SstatusS;
                      if ((status_234X == NO_ERRORS)) {
                        goto L13938;}
                      else {
                        ps_write_string((ps_error_string((SstatusS))), (stderr));
                        { long ignoreXX;
                        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                        goto L13938;}}}
                  else {
                    ps_write_string("Error reading from image file", (stderr));
                    { long ignoreXX;
                    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                    status_235X = SstatusS;
                    if ((status_235X == NO_ERRORS)) {
                      goto L13906;}
                    else {
                      ps_write_string((ps_error_string((SstatusS))), (stderr));
                      { long ignoreXX;
                      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
                      goto L13906;}}}
                else {
                  goto L13620;}}
              else {
                goto L13620;}}
            else {
              goto L13614;}}
          else {
            goto L13614;}}
        else {
          goto L13608;}}
      else {
        goto L13608;}}
    else {
      ps_write_string("invalid image format", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_236X = SstatusS;
      if ((status_236X == NO_ERRORS)) {
        goto L15093;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L15093;}}}}
 L5601: {
  i_237X = arg0K0;
  j_238X = arg0K1;
  if ((i_237X < j_238X)) {
    addr_a_239X = new_addr_186X + i_237X;
    addr_b_240X = new_addr_186X + j_238X;
    byte_a_241X = *((unsigned char *) addr_a_239X);
    *((unsigned char *) addr_a_239X) = (unsigned char) ((*((unsigned char *) addr_b_240X)));
    *((unsigned char *) addr_b_240X) = (unsigned char) (byte_a_241X);
    arg0K0 = (1 + i_237X);
    arg0K1 = (-1 + j_238X);
    goto L5601;}
  else {
    v_242X = *((long *) new_addr_186X);
    if ((1 == v_242X)) {
      goto L5560;}
    else {
      ps_error("Unable to correct byte order", 0);
      goto L5560;}}}
 L5993: {
  cells_243X = arg0K0;
  se_244X = PS_SHIFT_LEFT_INLINE(cells_243X, 2);
  status_245X = SstatusS;
  if ((status_245X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L6050;}
    else {
      PS_READ_INTEGER(port_133X, thing_246X, eofP_247X, status_248X)
      if (eofP_247X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L6050;}
      else {
        if ((status_248X == NO_ERRORS)) {
          arg0K0 = thing_246X;
          goto L6050;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_248X;
            arg0K0 = -1;
            goto L6050;}
          else {
            arg0K0 = -1;
            goto L6050;}}}}}
  else {
    arg0K0 = -1;
    goto L6050;}}
 L8233: {
  expr_249X = arg0K0;
  Simported_bindingsS = expr_249X;
  status_250X = SstatusS;
  if ((status_250X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8286;}
    else {
      PS_READ_INTEGER(port_133X, thing_251X, eofP_252X, status_253X)
      if (eofP_252X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8286;}
      else {
        if ((status_253X == NO_ERRORS)) {
          arg0K0 = thing_251X;
          goto L8286;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_253X;
            arg0K0 = -1;
            goto L8286;}
          else {
            arg0K0 = -1;
            goto L8286;}}}}}
  else {
    arg0K0 = -1;
    goto L8286;}}
 L7615: {
  v_254X = arg1K0;
  Ssymbol_addressS = v_254X;
  Sheap_object_remaining_cellsS = 0;
  Sheap_object_pointerS = NULL;
  keys_255X = (long*)malloc(sizeof(long) * 4097);
  arg0K0 = 0;
  goto L7638;}
 L13661: {
  if ((small_delta_220X == large_delta_222X)) {
    if ((large_delta_222X == weaks_delta_224X)) {
      goto L13677;}
    else {
      goto L13990;}}
  else {
    goto L13990;}}
 L13970: {
  status_256X = ps_close(port_133X);
  if ((status_256X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L13938: {
  status_257X = ps_close(port_133X);
  if ((status_257X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L13906: {
  status_258X = ps_close(port_133X);
  if ((status_258X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L13620: {
  ps_write_string("error reading small area from image", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  status_259X = SstatusS;
  if ((status_259X == NO_ERRORS)) {
    goto L13874;}
  else {
    ps_write_string((ps_error_string((SstatusS))), (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    goto L13874;}}
 L13614: {
  ps_write_string("error reading large area from image", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  status_260X = SstatusS;
  if ((status_260X == NO_ERRORS)) {
    goto L13835;}
  else {
    ps_write_string((ps_error_string((SstatusS))), (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    goto L13835;}}
 L13608: {
  ps_write_string("error reading weaks area from image", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  status_261X = SstatusS;
  if ((status_261X == NO_ERRORS)) {
    goto L13796;}
  else {
    ps_write_string((ps_error_string((SstatusS))), (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    goto L13796;}}
 L15093: {
  status_262X = ps_close(port_133X);
  if ((status_262X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L5560: {
  free(new_addr_186X);
  arg3K0 = 1;
  goto L15206;}
 L6050: {
  cells_263X = arg0K0;
  lh_264X = PS_SHIFT_LEFT_INLINE(cells_263X, 2);
  status_265X = SstatusS;
  if ((status_265X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L6105;}
    else {
      PS_READ_INTEGER(port_133X, thing_266X, eofP_267X, status_268X)
      if (eofP_267X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L6105;}
      else {
        if ((status_268X == NO_ERRORS)) {
          arg0K0 = thing_266X;
          goto L6105;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_268X;
            arg0K0 = -1;
            goto L6105;}
          else {
            arg0K0 = -1;
            goto L6105;}}}}}
  else {
    arg0K0 = -1;
    goto L6105;}}
 L8286: {
  expr_269X = arg0K0;
  Sexported_bindingsS = expr_269X;
  status_270X = SstatusS;
  if ((status_270X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8339;}
    else {
      PS_READ_INTEGER(port_133X, thing_271X, eofP_272X, status_273X)
      if (eofP_272X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8339;}
      else {
        if ((status_273X == NO_ERRORS)) {
          arg0K0 = thing_271X;
          goto L8339;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_273X;
            arg0K0 = -1;
            goto L8339;}
          else {
            arg0K0 = -1;
            goto L8339;}}}}}
  else {
    arg0K0 = -1;
    goto L8339;}}
 L7638: {
  i_274X = arg0K0;
  if ((i_274X < 4097)) {
    *(keys_255X + i_274X) = 0;
    arg0K0 = (1 + i_274X);
    goto L7638;}
  else {
    table_275X = (struct table*)malloc(sizeof(struct table));
    if ((NULL == table_275X)) {
      arg5K0 = table_275X;
      goto L7626;}
    else {
      table_275X->keys = keys_255X;
      table_275X->values = ((struct image_location**)malloc(sizeof(struct image_location*) * 4096));
      table_275X->count = 0;
      table_275X->size = 4096;
      arg5K0 = table_275X;
      goto L7626;}}}
 L13677: {
  if ((0 == small_delta_220X)) {
    goto L13731;}
  else {
    descriptor_276X = Sstartup_procedureS;
    if ((3 == (3 & descriptor_276X))) {
      arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_276X))) + small_delta_220X))));
      goto L13686;}
    else {
      arg0K0 = descriptor_276X;
      goto L13686;}}}
 L13990: {
  ps_error("Bug: Cannot load image, because the deltas of all parts aren't equal. Notify the authors.", 0);
  goto L13677;}
 L15208: {
  expr_277X = arg0K0;
  if ((0 == expr_277X)) {
    if (((SstatusS) == NO_ERRORS)) {
      return 0;}
    else {
      return -1;}}
  else {
    return -1;}}
 L13874: {
  status_278X = ps_close(port_133X);
  if ((status_278X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L13835: {
  status_279X = ps_close(port_133X);
  if ((status_279X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L13796: {
  status_280X = ps_close(port_133X);
  if ((status_280X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L15208;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L15208;}}
 L6105: {
  cells_281X = arg0K0;
  le_282X = PS_SHIFT_LEFT_INLINE(cells_281X, 2);
  status_283X = SstatusS;
  if ((status_283X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L6162;}
    else {
      PS_READ_INTEGER(port_133X, thing_284X, eofP_285X, status_286X)
      if (eofP_285X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L6162;}
      else {
        if ((status_286X == NO_ERRORS)) {
          arg0K0 = thing_284X;
          goto L6162;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_286X;
            arg0K0 = -1;
            goto L6162;}
          else {
            arg0K0 = -1;
            goto L6162;}}}}}
  else {
    arg0K0 = -1;
    goto L6162;}}
 L8339: {
  expr_287X = arg0K0;
  Sresumer_recordsS = expr_287X;
  status_288X = SstatusS;
  if ((status_288X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L8392;}
    else {
      PS_READ_INTEGER(port_133X, thing_289X, eofP_290X, status_291X)
      if (eofP_290X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L8392;}
      else {
        if ((status_291X == NO_ERRORS)) {
          arg0K0 = thing_289X;
          goto L8392;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_291X;
            arg0K0 = -1;
            goto L8392;}
          else {
            arg0K0 = -1;
            goto L8392;}}}}}
  else {
    arg0K0 = -1;
    goto L8392;}}
 L7626: {
  v_292X = arg5K0;
  Sstob_tableS = v_292X;
  Sheap_image_pointerS = ((char *)malloc((PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 2))));
  if ((0 == format_184X)) {
    need_293X = PS_SHIFT_LEFT_INLINE((Simg_heap_sizeS), 2);
    got_294X = ps_read_block(port_133X, ((char *) (Sheap_image_pointerS)), need_293X, &eofP_295X, &status_296X);
    if ((status_296X == NO_ERRORS)) {
      if (eofP_295X) {
        arg3K0 = 0;
        arg4K1 = "Premature EOF when reading image file";
        goto L9918;}
      else {
        if ((got_294X < need_293X)) {
          arg3K0 = 0;
          arg4K1 = "Read returned too few bytes";
          goto L9918;}
        else {
          arg3K0 = 1;
          arg4K1 = "";
          goto L9918;}}}
    else {
      SstatusS = status_296X;
      arg3K0 = 0;
      arg4K1 = "Error reading from image file";
      goto L9918;}}
  else {
    if ((1 == format_184X)) {
      merged_arg1K0 = (Snew_weaks_start_addrS);
      merged_arg0K1 = (Sweaks_img_heap_sizeS);
      merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
      really_read_image_area_return_address = &&really_read_image_area_return_0;
#else
      really_read_image_area_return_tag = 0;
#endif
      goto really_read_image_area;
     really_read_image_area_return_0:
      expr_297X = really_read_image_area0_return_value;
      if ((0 == expr_297X)) {
        if (((SstatusS) == NO_ERRORS)) {
          merged_arg1K0 = (Snew_large_start_addrS);
          merged_arg0K1 = (Slarge_img_heap_sizeS);
          merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
          really_read_image_area_return_address = &&really_read_image_area_return_1;
#else
          really_read_image_area_return_tag = 1;
#endif
          goto really_read_image_area;
         really_read_image_area_return_1:
          expr_298X = really_read_image_area0_return_value;
          if ((0 == expr_298X)) {
            if (((SstatusS) == NO_ERRORS)) {
              merged_arg1K0 = (Snew_small_start_addrS);
              merged_arg0K1 = (Ssmall_img_heap_sizeS);
              merged_arg6K2 = port_133X;
#ifdef USE_DIRECT_THREADING
              really_read_image_area_return_address = &&really_read_image_area_return_2;
#else
              really_read_image_area_return_tag = 2;
#endif
              goto really_read_image_area;
             really_read_image_area_return_2:
              expr_299X = really_read_image_area0_return_value;
              if ((0 == expr_299X)) {
                if (((SstatusS) == NO_ERRORS)) {
                  arg0K0 = 0;
                  goto L9951;}
                else {
                  arg0K0 = -1;
                  goto L9951;}}
              else {
                arg0K0 = -1;
                goto L9951;}}
            else {
              arg0K0 = -1;
              goto L9951;}}
          else {
            arg0K0 = -1;
            goto L9951;}}
        else {
          arg0K0 = -1;
          goto L9951;}}
      else {
        arg0K0 = -1;
        goto L9951;}}
    else {
      ps_write_string("this can't happen: invalid image format", (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      status_300X = SstatusS;
      if ((status_300X == NO_ERRORS)) {
        goto L10321;}
      else {
        ps_write_string((ps_error_string((SstatusS))), (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        goto L10321;}}}}
 L13731: {
  if ((0 == large_delta_222X)) {
    goto L13743;}
  else {
    merged_arg0K0 = large_delta_222X;
    merged_arg1K1 = (Snew_large_start_addrS);
    merged_arg1K2 = large_new_hp_223X;
#ifdef USE_DIRECT_THREADING
    relocate_image_return_address = &&relocate_image_return_0;
#else
    relocate_image_return_tag = 0;
#endif
    goto relocate_image;
   relocate_image_return_0:
    goto L13743;}}
 L13686: {
  expr_301X = arg0K0;
  Sstartup_procedureS = expr_301X;
  descriptor_302X = SsymbolsS;
  if ((3 == (3 & descriptor_302X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_302X))) + small_delta_220X))));
    goto L13692;}
  else {
    arg0K0 = descriptor_302X;
    goto L13692;}}
 L6162: {
  cells_303X = arg0K0;
  wh_304X = PS_SHIFT_LEFT_INLINE(cells_303X, 2);
  status_305X = SstatusS;
  if ((status_305X == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = -1;
      goto L6217;}
    else {
      PS_READ_INTEGER(port_133X, thing_306X, eofP_307X, status_308X)
      if (eofP_307X) {
        SeofPS = 1;
        arg0K0 = -1;
        goto L6217;}
      else {
        if ((status_308X == NO_ERRORS)) {
          arg0K0 = thing_306X;
          goto L6217;}
        else {
          SeofPS = 1;
          if (((SstatusS) == NO_ERRORS)) {
            SstatusS = status_308X;
            arg0K0 = -1;
            goto L6217;}
          else {
            arg0K0 = -1;
            goto L6217;}}}}}
  else {
    arg0K0 = -1;
    goto L6217;}}
 L8392: {
  expr_309X = arg0K0;
  Sstartup_procedureS = expr_309X;
  if (((SstatusS) == NO_ERRORS)) {
    if ((SeofPS)) {
      arg0K0 = 0;
      arg0K1 = format_157X;
      goto L15143;}
    else {
      goto L8731;}}
  else {
    arg0K0 = 0;
    arg0K1 = format_157X;
    goto L15143;}}
 L9918: {
  okayP_310X = arg3K0;
  string_311X = arg4K1;
  if (okayP_310X) {
    arg0K0 = 0;
    goto L9951;}
  else {
    ps_write_string(string_311X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_312X = SstatusS;
    if ((status_312X == NO_ERRORS)) {
      goto L10186;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L10186;}}}
 L9951: {
  expr_313X = arg0K0;
  if ((0 == expr_313X)) {
    if (((SstatusS) == NO_ERRORS)) {
      PS_READ_CHAR(port_133X, v_314X, eofP_315X, status_316X)
      if ((status_316X == NO_ERRORS)) {
        if (eofP_315X) {
          status_317X = ps_close(port_133X);
          if ((status_317X == NO_ERRORS)) {
            arg0K0 = 0;
            goto L14989;}
          else {
            ps_write_string("Error closing image file", (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            status_318X = SstatusS;
            if ((status_318X == NO_ERRORS)) {
              goto L10069;}
            else {
              ps_write_string((ps_error_string((SstatusS))), (stderr));
              { long ignoreXX;
              PS_WRITE_CHAR(10, (stderr), ignoreXX) }
              goto L10069;}}}
        else {
          ps_write_string("Image file has extraneous data after image", (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          status_319X = SstatusS;
          if ((status_319X == NO_ERRORS)) {
            goto L10037;}
          else {
            ps_write_string((ps_error_string((SstatusS))), (stderr));
            { long ignoreXX;
            PS_WRITE_CHAR(10, (stderr), ignoreXX) }
            goto L10037;}}}
      else {
        ps_write_string("Error reading from image file", (stderr));
        { long ignoreXX;
        PS_WRITE_CHAR(10, (stderr), ignoreXX) }
        status_320X = SstatusS;
        if ((status_320X == NO_ERRORS)) {
          goto L10005;}
        else {
          ps_write_string((ps_error_string((SstatusS))), (stderr));
          { long ignoreXX;
          PS_WRITE_CHAR(10, (stderr), ignoreXX) }
          goto L10005;}}}
    else {
      arg0K0 = -1;
      goto L14989;}}
  else {
    arg0K0 = -1;
    goto L14989;}}
 L10321: {
  status_321X = ps_close(port_133X);
  if ((status_321X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L9951;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L9951;}}
 L13743: {
  if ((0 == weaks_delta_224X)) {
    arg0K0 = 0;
    goto L15208;}
  else {
    merged_arg0K0 = weaks_delta_224X;
    merged_arg1K1 = (Snew_weaks_start_addrS);
    merged_arg1K2 = weaks_new_hp_225X;
#ifdef USE_DIRECT_THREADING
    relocate_image_return_address = &&relocate_image_return_1;
#else
    relocate_image_return_tag = 1;
#endif
    goto relocate_image;
   relocate_image_return_1:
    arg0K0 = 0;
    goto L15208;}}
 L13692: {
  expr_322X = arg0K0;
  SsymbolsS = expr_322X;
  descriptor_323X = Simported_bindingsS;
  if ((3 == (3 & descriptor_323X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_323X))) + small_delta_220X))));
    goto L13698;}
  else {
    arg0K0 = descriptor_323X;
    goto L13698;}}
 L6217: {
  cells_324X = arg0K0;
  we_325X = PS_SHIFT_LEFT_INLINE(cells_324X, 2);
  Ssmall_img_start_addrS = (((char *) sb_196X));
  Ssmall_img_hp_addrS = (((char *) sh_208X));
  Ssmall_img_end_addrS = (((char *) se_244X));
  Slarge_img_start_addrS = (((char *) se_244X));
  Slarge_img_hp_addrS = (((char *) lh_264X));
  Slarge_img_end_addrS = (((char *) le_282X));
  Sweaks_img_start_addrS = (((char *) le_282X));
  Sweaks_img_hp_addrS = (((char *) wh_304X));
  Sweaks_img_end_addrS = (((char *) we_325X));
  Ssmall_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((sh_208X - sb_196X), 2));
  Slarge_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((lh_264X - se_244X), 2));
  Sweaks_img_heap_sizeS = (PS_SHIFT_RIGHT_INLINE((wh_304X - le_282X), 2));
  Simg_start_addrS = (((char *) le_282X));
  Simg_end_addrS = (((char *) sh_208X));
  Simg_heap_sizeS = (((PS_SHIFT_RIGHT_INLINE((se_244X - sb_196X), 2)) + (PS_SHIFT_RIGHT_INLINE((le_282X - se_244X), 2))) + (PS_SHIFT_RIGHT_INLINE((we_325X - le_282X), 2)));
  goto L8129;}
 L8731: {
  PS_READ_CHAR(port_133X, ch_326X, eofP_327X, status_328X)
  if (eofP_327X) {
    arg3K0 = eofP_327X;
    arg0K1 = status_328X;
    goto L8419;}
  else {
    if ((status_328X == NO_ERRORS)) {
      if ((12 == ch_326X)) {
        arg3K0 = 0;
        arg0K1 = status_328X;
        goto L8419;}
      else {
        goto L8731;}}
    else {
      arg3K0 = eofP_327X;
      arg0K1 = status_328X;
      goto L8419;}}}
 L10186: {
  status_329X = ps_close(port_133X);
  if ((status_329X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L9951;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L9951;}}
 L14989: {
  expr_330X = arg0K0;
  if ((0 == expr_330X)) {
    if (((SstatusS) == NO_ERRORS)) {
      if (reverse_byte_orderP_218X) {
        merged_arg1K0 = (Simg_start_addrS);
        merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
        old_Gnew_addr_return_address = &&old_Gnew_addr_return_0;
#else
        old_Gnew_addr_return_tag = 0;
#endif
        goto old_Gnew_addr;
       old_Gnew_addr_return_0:
        v_331X = old_Gnew_addr0_return_value;
        merged_arg1K0 = (Simg_end_addrS);
        merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
        old_Gnew_addr_return_address = &&old_Gnew_addr_return_1;
#else
        old_Gnew_addr_return_tag = 1;
#endif
        goto old_Gnew_addr;
       old_Gnew_addr_return_1:
        v_332X = old_Gnew_addr0_return_value;
        merged_arg1K0 = v_331X;
        merged_arg1K1 = v_332X;
#ifdef USE_DIRECT_THREADING
        reverse_byte_orderB_return_address = &&reverse_byte_orderB_return_3;
#else
        reverse_byte_orderB_return_tag = 3;
#endif
        goto reverse_byte_orderB;
       reverse_byte_orderB_return_3:
        goto L15001;}
      else {
        goto L15001;}}
    else {
      arg0K0 = -1;
      goto L15208;}}
  else {
    arg0K0 = -1;
    goto L15208;}}
 L10069: {
  status_333X = ps_close(port_133X);
  if ((status_333X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L14989;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L14989;}}
 L10037: {
  status_334X = ps_close(port_133X);
  if ((status_334X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L14989;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L14989;}}
 L10005: {
  status_335X = ps_close(port_133X);
  if ((status_335X == NO_ERRORS)) {
    arg0K0 = -1;
    goto L14989;}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    arg0K0 = -1;
    goto L14989;}}
 L13698: {
  expr_336X = arg0K0;
  Simported_bindingsS = expr_336X;
  descriptor_337X = Sexported_bindingsS;
  if ((3 == (3 & descriptor_337X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_337X))) + small_delta_220X))));
    goto L13704;}
  else {
    arg0K0 = descriptor_337X;
    goto L13704;}}
 L8419: {
  eofP_338X = arg3K0;
  status_339X = arg0K1;
  if (eofP_338X) {
    SeofPS = 1;
    arg0K0 = 0;
    arg0K1 = format_157X;
    goto L15143;}
  else {
    if ((status_339X == NO_ERRORS)) {
      arg0K0 = 0;
      arg0K1 = format_157X;
      goto L15143;}
    else {
      SeofPS = 1;
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = status_339X;
        arg0K0 = 0;
        arg0K1 = format_157X;
        goto L15143;}
      else {
        arg0K0 = 0;
        arg0K1 = format_157X;
        goto L15143;}}}}
 L15001: {
  if ((0 == format_184X)) {
    merged_arg1K0 = (Simg_start_addrS);
    merged_arg1K1 = (Ssymbol_addressS);
    merged_arg0K2 = format_184X;
#ifdef USE_DIRECT_THREADING
    parse_reachable_objects_return_address = &&parse_reachable_objects_return_0;
#else
    parse_reachable_objects_return_tag = 0;
#endif
    goto parse_reachable_objects;
   parse_reachable_objects_return_0:
    goto L15003;}
  else {
    if ((1 == format_184X)) {
      merged_arg1K0 = (Ssmall_img_start_addrS);
      merged_arg1K1 = (Ssymbol_addressS);
      merged_arg0K2 = format_184X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_1;
#else
      parse_reachable_objects_return_tag = 1;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_1:
      merged_arg1K0 = (Slarge_img_start_addrS);
      merged_arg1K1 = ((Slarge_img_start_addrS) + (PS_SHIFT_LEFT_INLINE((Slarge_img_heap_sizeS), 2)));
      merged_arg0K2 = format_184X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_2;
#else
      parse_reachable_objects_return_tag = 2;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_2:
      merged_arg1K0 = (Sweaks_img_start_addrS);
      merged_arg1K1 = ((Sweaks_img_start_addrS) + (PS_SHIFT_LEFT_INLINE((Sweaks_img_heap_sizeS), 2)));
      merged_arg0K2 = format_184X;
#ifdef USE_DIRECT_THREADING
      parse_reachable_objects_return_address = &&parse_reachable_objects_return_3;
#else
      parse_reachable_objects_return_tag = 3;
#endif
      goto parse_reachable_objects;
     parse_reachable_objects_return_3:
      goto L15003;}
    else {
      ps_error("allocate+parse+copy-objects!: Unknown image format", 0);
      goto L15003;}}}
 L13704: {
  expr_340X = arg0K0;
  Sexported_bindingsS = expr_340X;
  descriptor_341X = Sresumer_recordsS;
  if ((3 == (3 & descriptor_341X))) {
    arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_341X))) + small_delta_220X))));
    goto L13710;}
  else {
    arg0K0 = descriptor_341X;
    goto L13710;}}
 L15003: {
  address_342X = ((char *) (-7 + (Sstartup_procedureS)));
  if ((0 == (((long) address_342X)))) {
    arg0K0 = -1;
    goto L14397;}
  else {
    arg0K0 = (((long) address_342X));
    goto L14397;}}
 L13710: {
  expr_343X = arg0K0;
  Sresumer_recordsS = expr_343X;
  table_344X = SsymbolsS;
  if ((3 == (3 & table_344X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + table_344X))))), 2))))) {
      arg0K0 = 0;
      goto L12397;}
    else {
      goto L13716;}}
  else {
    goto L13716;}}
 L14397: {
  v_345X = arg0K0;
  image_location_346X = table_ref((Sstob_tableS), v_345X);
  Sstartup_procedureS = (3 + (((long) ((((char *) (image_location_346X->new_descriptor))) + 4))));
  v_347X = SsymbolsS;
  if ((1 == v_347X)) {
    goto L14327;}
  else {
    tab_348X = SsymbolsS;
    merged_arg1K0 = (((char *) (-7 + tab_348X)));
    merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_2;
#else
    old_Gnew_addr_return_tag = 2;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_2:
    addr_349X = old_Gnew_addr0_return_value;
    cell_350X = *((long *) addr_349X);
    if ((2 == (3 & cell_350X))) {
      size_351X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_350X, 8))), 2);
      pointer_352X = s48_allocate_tracedAgc((4 + (PS_SHIFT_LEFT_INLINE(size_351X, 2))));
      memmove((void *)pointer_352X, (void *)addr_349X,(4 + (PS_SHIFT_LEFT_INLINE(size_351X, 2))));
      arg0K0 = (3 + (((long) (pointer_352X + 4))));
      goto L14318;}
    else {
      ps_error("read-tables! no header", 0);
      arg0K0 = v_353X;
      goto L14318;}}}
 L12397: {
  i_354X = arg0K0;
  if ((1024 == i_354X)) {
    goto L13716;}
  else {
    link_355X = *((long *) ((((char *) (-3 + table_344X))) + (PS_SHIFT_LEFT_INLINE(i_354X, 2))));
    if ((0 == (3 & link_355X))) {
      arg0K0 = (3 + (-4 & link_355X));
      goto L12403;}
    else {
      arg0K0 = link_355X;
      goto L12403;}}}
 L13716: {
  merged_arg0K0 = (Simported_bindingsS);
  merged_arg0K1 = small_delta_220X;
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_two_spaceB_return_address = &&relocate_binding_table_two_spaceB_return_0;
#else
  relocate_binding_table_two_spaceB_return_tag = 0;
#endif
  goto relocate_binding_table_two_spaceB;
 relocate_binding_table_two_spaceB_return_0:
  merged_arg0K0 = (Sexported_bindingsS);
  merged_arg0K1 = small_delta_220X;
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_two_spaceB_return_address = &&relocate_binding_table_two_spaceB_return_1;
#else
  relocate_binding_table_two_spaceB_return_tag = 1;
#endif
  goto relocate_binding_table_two_spaceB;
 relocate_binding_table_two_spaceB_return_1:
  merged_arg0K0 = small_delta_220X;
  merged_arg1K1 = (Snew_small_start_addrS);
  merged_arg1K2 = small_new_hp_221X;
#ifdef USE_DIRECT_THREADING
  relocate_image_return_address = &&relocate_image_return_2;
#else
  relocate_image_return_tag = 2;
#endif
  goto relocate_image;
 relocate_image_return_2:
  goto L13731;}
 L14327: {
  v_356X = Simported_bindingsS;
  if ((1 == v_356X)) {
    goto L14347;}
  else {
    tab_357X = Simported_bindingsS;
    merged_arg1K0 = (((char *) (-7 + tab_357X)));
    merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_3;
#else
    old_Gnew_addr_return_tag = 3;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_3:
    addr_358X = old_Gnew_addr0_return_value;
    cell_359X = *((long *) addr_358X);
    if ((2 == (3 & cell_359X))) {
      size_360X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_359X, 8))), 2);
      pointer_361X = s48_allocate_tracedAgc((4 + (PS_SHIFT_LEFT_INLINE(size_360X, 2))));
      memmove((void *)pointer_361X, (void *)addr_358X,(4 + (PS_SHIFT_LEFT_INLINE(size_360X, 2))));
      arg0K0 = (3 + (((long) (pointer_361X + 4))));
      goto L14338;}
    else {
      ps_error("read-tables! no header", 0);
      arg0K0 = v_362X;
      goto L14338;}}}
 L14318: {
  expr_363X = arg0K0;
  SsymbolsS = expr_363X;
  table_364X = SsymbolsS;
  stob_table_365X = Sstob_tableS;
  if ((3 == (3 & table_364X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + table_364X))))), 2))))) {
      arg0K0 = 0;
      goto L12611;}
    else {
      goto L14327;}}
  else {
    goto L14327;}}
 L12403: {
  bucket_366X = arg0K0;
  if ((1 == bucket_366X)) {
    goto L12399;}
  else {
    bucket_367X = bucket_366X + small_delta_220X;
    if ((3 == (3 & bucket_367X))) {
      arg0K0 = (-4 & bucket_367X);
      goto L12408;}
    else {
      arg0K0 = bucket_367X;
      goto L12408;}}}
 L14347: {
  v_368X = Sexported_bindingsS;
  if ((1 == v_368X)) {
    goto L14367;}
  else {
    tab_369X = Sexported_bindingsS;
    merged_arg1K0 = (((char *) (-7 + tab_369X)));
    merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_4;
#else
    old_Gnew_addr_return_tag = 4;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_4:
    addr_370X = old_Gnew_addr0_return_value;
    cell_371X = *((long *) addr_370X);
    if ((2 == (3 & cell_371X))) {
      size_372X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_371X, 8))), 2);
      pointer_373X = s48_allocate_tracedAgc((4 + (PS_SHIFT_LEFT_INLINE(size_372X, 2))));
      memmove((void *)pointer_373X, (void *)addr_370X,(4 + (PS_SHIFT_LEFT_INLINE(size_372X, 2))));
      arg0K0 = (3 + (((long) (pointer_373X + 4))));
      goto L14358;}
    else {
      ps_error("read-tables! no header", 0);
      arg0K0 = v_374X;
      goto L14358;}}}
 L14338: {
  expr_375X = arg0K0;
  Simported_bindingsS = expr_375X;
  merged_arg0K0 = (Simported_bindingsS);
  merged_arg5K1 = (Sstob_tableS);
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_bibopB_return_address = &&relocate_binding_table_bibopB_return_0;
#else
  relocate_binding_table_bibopB_return_tag = 0;
#endif
  goto relocate_binding_table_bibopB;
 relocate_binding_table_bibopB_return_0:
  goto L14347;}
 L12611: {
  i_376X = arg0K0;
  if ((1024 == i_376X)) {
    goto L14327;}
  else {
    link_377X = *((long *) ((((char *) (-3 + table_364X))) + (PS_SHIFT_LEFT_INLINE(i_376X, 2))));
    if ((0 == (3 & link_377X))) {
      arg0K0 = (3 + (-4 & link_377X));
      goto L12617;}
    else {
      arg0K0 = link_377X;
      goto L12617;}}}
 L12399: {
  arg0K0 = (1 + i_354X);
  goto L12397;}
 L12408: {
  value_378X = arg0K0;
  addr_379X = (((char *) (-3 + table_344X))) + (PS_SHIFT_LEFT_INLINE(i_354X, 2));S48_WRITE_BARRIER(table_344X, addr_379X, value_378X);
  *((long *) addr_379X) = (long) (value_378X);
  arg0K0 = bucket_367X;
  goto L12419;}
 L14367: {
  if ((1 == (Sresumer_recordsS))) {
    goto L15005;}
  else {
    tab_380X = Sresumer_recordsS;
    merged_arg1K0 = (((char *) (-7 + tab_380X)));
    merged_arg0K1 = format_184X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_5;
#else
    old_Gnew_addr_return_tag = 5;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_5:
    addr_381X = old_Gnew_addr0_return_value;
    cell_382X = *((long *) addr_381X);
    if ((2 == (3 & cell_382X))) {
      size_383X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_382X, 8))), 2);
      pointer_384X = s48_allocate_tracedAgc((4 + (PS_SHIFT_LEFT_INLINE(size_383X, 2))));
      memmove((void *)pointer_384X, (void *)addr_381X,(4 + (PS_SHIFT_LEFT_INLINE(size_383X, 2))));
      arg0K0 = (3 + (((long) (pointer_384X + 4))));
      goto L14378;}
    else {
      ps_error("read-tables! no header", 0);
      arg0K0 = v_385X;
      goto L14378;}}}
 L14358: {
  expr_386X = arg0K0;
  Sexported_bindingsS = expr_386X;
  merged_arg0K0 = (Sexported_bindingsS);
  merged_arg5K1 = (Sstob_tableS);
#ifdef USE_DIRECT_THREADING
  relocate_binding_table_bibopB_return_address = &&relocate_binding_table_bibopB_return_1;
#else
  relocate_binding_table_bibopB_return_tag = 1;
#endif
  goto relocate_binding_table_bibopB;
 relocate_binding_table_bibopB_return_1:
  goto L14367;}
 L12617: {
  bucket_387X = arg0K0;
  if ((1 == bucket_387X)) {
    goto L12613;}
  else {
    merged_arg0K0 = bucket_387X;
#ifdef USE_DIRECT_THREADING
    relocateD1_return_address = &&relocateD1_return_0;
#else
    relocateD1_return_tag = 0;
#endif
    goto relocateD1;
   relocateD1_return_0:
    bucket_388X = relocateD10_return_value;
    if ((3 == (3 & bucket_388X))) {
      arg0K0 = (-4 & bucket_388X);
      goto L12622;}
    else {
      arg0K0 = bucket_388X;
      goto L12622;}}}
 L12419: {
  entry_389X = arg0K0;
  link_390X = *((long *) ((((char *) (-3 + entry_389X))) + 4));
  if ((0 == (3 & link_390X))) {
    arg0K0 = (3 + (-4 & link_390X));
    goto L12423;}
  else {
    arg0K0 = link_390X;
    goto L12423;}}
 L15005: {
  table_391X = Sstob_tableS;
  keys_392X = table_391X->keys;
  values_393X = table_391X->values;
  arg0K0 = 0;
  goto L3608;}
 L14378: {
  expr_394X = arg0K0;
  Sresumer_recordsS = expr_394X;
  resumer_records_395X = Sresumer_recordsS;
  cell_396X = *((long *) (((char *) (-7 + resumer_records_395X))));
  if ((2 == (3 & cell_396X))) {
    arg1K0 = (((char *) (-3 + resumer_records_395X)));
    arg0K1 = (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_396X, 8))), 2));
    goto L10474;}
  else {
    ps_error("relocate-resumer-record! - no header", 0);
    goto L15005;}}
 L12613: {
  arg0K0 = (1 + i_376X);
  goto L12611;}
 L12622: {
  value_397X = arg0K0;
  addr_398X = (((char *) (-3 + table_364X))) + (PS_SHIFT_LEFT_INLINE(i_376X, 2));S48_WRITE_BARRIER(table_364X, addr_398X, value_397X);
  *((long *) addr_398X) = (long) (value_397X);
  arg0K0 = bucket_388X;
  goto L12633;}
 L12423: {
  next_399X = arg0K0;
  if ((1 == next_399X)) {
    goto L12399;}
  else {
    next_400X = next_399X + small_delta_220X;
    if ((3 == (3 & next_400X))) {
      arg0K0 = (-4 & next_400X);
      goto L12428;}
    else {
      arg0K0 = next_400X;
      goto L12428;}}}
 L3608: {
  i_401X = arg0K0;
  if ((i_401X == (table_391X->size))) {
    free(keys_392X);
    free(values_393X);
    free(table_391X);
    free((Sheap_image_pointerS));
    arg0K0 = 0;
    goto L15208;}
  else {
    if ((0 == (*(keys_392X + i_401X)))) {
      goto L3610;}
    else {
      free((*(values_393X + i_401X)));
      goto L3610;}}}
 L10474: {
  address_402X = arg1K0;
  size_403X = arg0K1;
  if ((0 == size_403X)) {
    goto L15005;}
  else {
    cell_404X = *((long *) address_402X);
    if ((3 == (3 & cell_404X))) {
      address_405X = ((char *) (-7 + cell_404X));
      if ((0 == (((long) address_405X)))) {
        arg0K0 = -1;
        goto L10539;}
      else {
        arg0K0 = (((long) address_405X));
        goto L10539;}}
    else {
      ps_error("Could this happen?", 0);
      goto L15005;}}}
 L12633: {
  entry_406X = arg0K0;
  link_407X = *((long *) ((((char *) (-3 + entry_406X))) + 4));
  if ((0 == (3 & link_407X))) {
    arg0K0 = (3 + (-4 & link_407X));
    goto L12637;}
  else {
    arg0K0 = link_407X;
    goto L12637;}}
 L12428: {
  val_408X = arg0K0;
  addr_409X = (((char *) (-3 + entry_389X))) + 4;S48_WRITE_BARRIER(entry_389X, addr_409X, val_408X);
  *((long *) addr_409X) = (long) (val_408X);
  arg0K0 = next_400X;
  goto L12419;}
 L3610: {
  arg0K0 = (1 + i_401X);
  goto L3608;}
 L10539: {
  v_410X = arg0K0;
  image_location_411X = table_ref((Sstob_tableS), v_410X);
  *((long *) address_402X) = (long) ((3 + (((long) ((((char *) (image_location_411X->new_descriptor))) + 4)))));
  arg1K0 = (address_402X + 4);
  arg0K1 = (-1 + size_403X);
  goto L10474;}
 L12637: {
  next_412X = arg0K0;
  if ((1 == next_412X)) {
    goto L12613;}
  else {
    merged_arg0K0 = next_412X;
#ifdef USE_DIRECT_THREADING
    relocateD1_return_address = &&relocateD1_return_1;
#else
    relocateD1_return_tag = 1;
#endif
    goto relocateD1;
   relocateD1_return_1:
    next_413X = relocateD10_return_value;
    if ((3 == (3 & next_413X))) {
      arg0K0 = (-4 & next_413X);
      goto L12642;}
    else {
      arg0K0 = next_413X;
      goto L12642;}}}
 L12642: {
  val_414X = arg0K0;
  addr_415X = (((char *) (-3 + entry_406X))) + 4;S48_WRITE_BARRIER(entry_406X, addr_415X, val_414X);
  *((long *) addr_415X) = (long) (val_414X);
  arg0K0 = next_413X;
  goto L12633;}
 really_read_image_area: {
  new_start_addr_130X = merged_arg1K0;
  img_heap_size_131X = merged_arg0K1;
  port_132X = merged_arg6K2;{
  need_416X = PS_SHIFT_LEFT_INLINE(img_heap_size_131X, 2);
  got_417X = ps_read_block(port_132X, ((char *) new_start_addr_130X), need_416X, &eofP_418X, &status_419X);
  if ((status_419X == NO_ERRORS)) {
    if (eofP_418X) {
      arg3K0 = 0;
      arg4K1 = "Premature EOF when reading image file";
      goto L4574;}
    else {
      if ((got_417X < need_416X)) {
        arg3K0 = 0;
        arg4K1 = "Read returned too few bytes";
        goto L4574;}
      else {
        arg3K0 = 1;
        arg4K1 = "";
        goto L4574;}}}
  else {
    SstatusS = status_419X;
    arg3K0 = 0;
    arg4K1 = "Error reading from image file";
    goto L4574;}}
 L4574: {
  okayP_420X = arg3K0;
  string_421X = arg4K1;
  if (okayP_420X) {
    really_read_image_area0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *really_read_image_area_return_address;
#else
    goto really_read_image_area_return;
#endif
}
  else {
    ps_write_string(string_421X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_422X = SstatusS;
    if ((status_422X == NO_ERRORS)) {
      goto L4617;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L4617;}}}
 L4617: {
  status_423X = ps_close(port_132X);
  if ((status_423X == NO_ERRORS)) {
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

 read_image_area: {
  new_start_addr_127X = merged_arg1K0;
  img_heap_size_128X = merged_arg0K1;
  port_129X = merged_arg6K2;{
  need_424X = PS_SHIFT_LEFT_INLINE(img_heap_size_128X, 2);
  got_425X = ps_read_block(port_129X, ((char *) new_start_addr_127X), need_424X, &eofP_426X, &status_427X);
  if ((status_427X == NO_ERRORS)) {
    if (eofP_426X) {
      arg3K0 = 0;
      arg4K1 = "Premature EOF when reading image file";
      goto L4660;}
    else {
      if ((got_425X < need_424X)) {
        arg3K0 = 0;
        arg4K1 = "Read returned too few bytes";
        goto L4660;}
      else {
        arg3K0 = 1;
        arg4K1 = "";
        goto L4660;}}}
  else {
    SstatusS = status_427X;
    arg3K0 = 0;
    arg4K1 = "Error reading from image file";
    goto L4660;}}
 L4660: {
  okayP_428X = arg3K0;
  string_429X = arg4K1;
  if (okayP_428X) {
    read_image_area0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *read_image_area_return_address;
#else
    goto read_image_area_return;
#endif
}
  else {
    ps_write_string(string_429X, (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    status_430X = SstatusS;
    if ((status_430X == NO_ERRORS)) {
      goto L4703;}
    else {
      ps_write_string((ps_error_string((SstatusS))), (stderr));
      { long ignoreXX;
      PS_WRITE_CHAR(10, (stderr), ignoreXX) }
      goto L4703;}}}
 L4703: {
  status_431X = ps_close(port_129X);
  if ((status_431X == NO_ERRORS)) {
    read_image_area0_return_value = -1;
#ifdef USE_DIRECT_THREADING
    goto *read_image_area_return_address;
#else
    goto read_image_area_return;
#endif
}
  else {
    ps_write_string("Error closing image file", (stderr));
    { long ignoreXX;
    PS_WRITE_CHAR(10, (stderr), ignoreXX) }
    read_image_area0_return_value = -1;
#ifdef USE_DIRECT_THREADING
    goto *read_image_area_return_address;
#else
    goto read_image_area_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 read_image_area_return:
  switch (read_image_area_return_tag) {
  case 0: goto read_image_area_return_0;
  case 1: goto read_image_area_return_1;
  default: goto read_image_area_return_2;
  }
#endif
}

 reverse_byte_orderB: {
  start_125X = merged_arg1K0;
  end_126X = merged_arg1K1;{
  ps_write_string("Correcting byte order of resumed image.", (stderr));
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
  arg1K0 = start_125X;
  goto L10857;}
 L10857: {
  ptr_432X = arg1K0;
  if ((ptr_432X < end_126X)) {
    arg0K0 = 0;
    arg0K1 = 3;
    goto L10897;}
  else {
#ifdef USE_DIRECT_THREADING
    goto *reverse_byte_orderB_return_address;
#else
    goto reverse_byte_orderB_return;
#endif
}}
 L10897: {
  i_433X = arg0K0;
  j_434X = arg0K1;
  if ((i_433X < j_434X)) {
    addr_a_435X = ptr_432X + i_433X;
    addr_b_436X = ptr_432X + j_434X;
    byte_a_437X = *((unsigned char *) addr_a_435X);
    *((unsigned char *) addr_a_435X) = (unsigned char) ((*((unsigned char *) addr_b_436X)));
    *((unsigned char *) addr_b_436X) = (unsigned char) (byte_a_437X);
    arg0K0 = (1 + i_433X);
    arg0K1 = (-1 + j_434X);
    goto L10897;}
  else {
    value_438X = *((long *) ptr_432X);
    next_439X = ptr_432X + 4;
    if ((2 == (3 & value_438X))) {
      if (((31 & (PS_SHIFT_RIGHT_INLINE(value_438X, 2))) < 16)) {
        arg1K0 = next_439X;
        goto L10857;}
      else {
        arg1K0 = (next_439X + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(value_438X, 8)))));
        goto L10857;}}
    else {
      arg1K0 = next_439X;
      goto L10857;}}}
#ifndef USE_DIRECT_THREADING
 reverse_byte_orderB_return:
  switch (reverse_byte_orderB_return_tag) {
  case 0: goto reverse_byte_orderB_return_0;
  case 1: goto reverse_byte_orderB_return_1;
  case 2: goto reverse_byte_orderB_return_2;
  default: goto reverse_byte_orderB_return_3;
  }
#endif
}

 relocate_image: {
  delta_122X = merged_arg0K0;
  start_123X = merged_arg1K1;
  end_124X = merged_arg1K2;{
  arg1K0 = start_123X;
  goto L10963;}
 L10963: {
  ptr_440X = arg1K0;
  if ((ptr_440X < end_124X)) {
    descriptor_441X = *((long *) ptr_440X);
    if ((3 == (3 & descriptor_441X))) {
      arg0K0 = (3 + (((long) ((((char *) (-3 + descriptor_441X))) + delta_122X))));
      goto L10968;}
    else {
      arg0K0 = descriptor_441X;
      goto L10968;}}
  else {
#ifdef USE_DIRECT_THREADING
    goto *relocate_image_return_address;
#else
    goto relocate_image_return;
#endif
}}
 L10968: {
  d_442X = arg0K0;
  *((long *) ptr_440X) = (long) (d_442X);
  if ((2 == (3 & d_442X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(d_442X, 2))) < 16)) {
      goto L10980;}
    else {
      arg1K0 = (ptr_440X + (4 + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(d_442X, 8))))));
      goto L10963;}}
  else {
    goto L10980;}}
 L10980: {
  arg1K0 = (ptr_440X + 4);
  goto L10963;}
#ifndef USE_DIRECT_THREADING
 relocate_image_return:
  switch (relocate_image_return_tag) {
  case 0: goto relocate_image_return_0;
  case 1: goto relocate_image_return_1;
  default: goto relocate_image_return_2;
  }
#endif
}

 relocate_binding_table_two_spaceB: {
  table_120X = merged_arg0K0;
  delta_121X = merged_arg0K1;{
  if ((3 == (3 & table_120X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + table_120X))))), 2))))) {
      arg0K0 = 0;
      goto L12492;}
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
 L12492: {
  i_443X = arg0K0;
  if ((1024 == i_443X)) {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_two_spaceB_return_address;
#else
    goto relocate_binding_table_two_spaceB_return;
#endif
}
  else {
    link_444X = *((long *) ((((char *) (-3 + table_120X))) + (PS_SHIFT_LEFT_INLINE(i_443X, 2))));
    if ((0 == (3 & link_444X))) {
      arg0K0 = (3 + (-4 & link_444X));
      goto L12498;}
    else {
      arg0K0 = link_444X;
      goto L12498;}}}
 L12498: {
  bucket_445X = arg0K0;
  if ((1 == bucket_445X)) {
    goto L12494;}
  else {
    bucket_446X = bucket_445X + delta_121X;
    if ((3 == (3 & bucket_446X))) {
      arg0K0 = (-4 & bucket_446X);
      goto L12503;}
    else {
      arg0K0 = bucket_446X;
      goto L12503;}}}
 L12494: {
  arg0K0 = (1 + i_443X);
  goto L12492;}
 L12503: {
  value_447X = arg0K0;
  addr_448X = (((char *) (-3 + table_120X))) + (PS_SHIFT_LEFT_INLINE(i_443X, 2));S48_WRITE_BARRIER(table_120X, addr_448X, value_447X);
  *((long *) addr_448X) = (long) (value_447X);
  arg0K0 = bucket_446X;
  goto L12514;}
 L12514: {
  entry_449X = arg0K0;
  link_450X = *((long *) ((((char *) (-3 + entry_449X))) + 12));
  if ((0 == (3 & link_450X))) {
    arg0K0 = (3 + (-4 & link_450X));
    goto L12518;}
  else {
    arg0K0 = link_450X;
    goto L12518;}}
 L12518: {
  next_451X = arg0K0;
  if ((1 == next_451X)) {
    goto L12494;}
  else {
    next_452X = next_451X + delta_121X;
    if ((3 == (3 & next_452X))) {
      arg0K0 = (-4 & next_452X);
      goto L12523;}
    else {
      arg0K0 = next_452X;
      goto L12523;}}}
 L12523: {
  val_453X = arg0K0;
  addr_454X = (((char *) (-3 + entry_449X))) + 12;S48_WRITE_BARRIER(entry_449X, addr_454X, val_453X);
  *((long *) addr_454X) = (long) (val_453X);
  arg0K0 = next_452X;
  goto L12514;}
#ifndef USE_DIRECT_THREADING
 relocate_binding_table_two_spaceB_return:
  switch (relocate_binding_table_two_spaceB_return_tag) {
  case 0: goto relocate_binding_table_two_spaceB_return_0;
  default: goto relocate_binding_table_two_spaceB_return_1;
  }
#endif
}

 relocateD0: {
  address_119X = merged_arg0K0;{
  address_455X = ((char *) (-7 + address_119X));
  if ((0 == (((long) address_455X)))) {
    arg0K0 = -1;
    goto L12709;}
  else {
    arg0K0 = (((long) address_455X));
    goto L12709;}}
 L12709: {
  v_456X = arg0K0;
  image_location_457X = table_ref(stob_table_118X, v_456X);
  relocateD00_return_value = (3 + (((long) ((((char *) (image_location_457X->new_descriptor))) + 4))));
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
  table_117X = merged_arg0K0;
  stob_table_118X = merged_arg5K1;{
  if ((3 == (3 & table_117X))) {
    if ((2 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + table_117X))))), 2))))) {
      arg0K0 = 0;
      goto L12739;}
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
 L12739: {
  i_458X = arg0K0;
  if ((1024 == i_458X)) {
#ifdef USE_DIRECT_THREADING
    goto *relocate_binding_table_bibopB_return_address;
#else
    goto relocate_binding_table_bibopB_return;
#endif
}
  else {
    link_459X = *((long *) ((((char *) (-3 + table_117X))) + (PS_SHIFT_LEFT_INLINE(i_458X, 2))));
    if ((0 == (3 & link_459X))) {
      arg0K0 = (3 + (-4 & link_459X));
      goto L12745;}
    else {
      arg0K0 = link_459X;
      goto L12745;}}}
 L12745: {
  bucket_460X = arg0K0;
  if ((1 == bucket_460X)) {
    goto L12741;}
  else {
    merged_arg0K0 = bucket_460X;
#ifdef USE_DIRECT_THREADING
    relocateD0_return_address = &&relocateD0_return_0;
#else
    relocateD0_return_tag = 0;
#endif
    goto relocateD0;
   relocateD0_return_0:
    bucket_461X = relocateD00_return_value;
    if ((3 == (3 & bucket_461X))) {
      arg0K0 = (-4 & bucket_461X);
      goto L12750;}
    else {
      arg0K0 = bucket_461X;
      goto L12750;}}}
 L12741: {
  arg0K0 = (1 + i_458X);
  goto L12739;}
 L12750: {
  value_462X = arg0K0;
  addr_463X = (((char *) (-3 + table_117X))) + (PS_SHIFT_LEFT_INLINE(i_458X, 2));S48_WRITE_BARRIER(table_117X, addr_463X, value_462X);
  *((long *) addr_463X) = (long) (value_462X);
  arg0K0 = bucket_461X;
  goto L12761;}
 L12761: {
  entry_464X = arg0K0;
  link_465X = *((long *) ((((char *) (-3 + entry_464X))) + 12));
  if ((0 == (3 & link_465X))) {
    arg0K0 = (3 + (-4 & link_465X));
    goto L12765;}
  else {
    arg0K0 = link_465X;
    goto L12765;}}
 L12765: {
  next_466X = arg0K0;
  if ((1 == next_466X)) {
    goto L12741;}
  else {
    merged_arg0K0 = next_466X;
#ifdef USE_DIRECT_THREADING
    relocateD0_return_address = &&relocateD0_return_1;
#else
    relocateD0_return_tag = 1;
#endif
    goto relocateD0;
   relocateD0_return_1:
    next_467X = relocateD00_return_value;
    if ((3 == (3 & next_467X))) {
      arg0K0 = (-4 & next_467X);
      goto L12770;}
    else {
      arg0K0 = next_467X;
      goto L12770;}}}
 L12770: {
  val_468X = arg0K0;
  addr_469X = (((char *) (-3 + entry_464X))) + 12;S48_WRITE_BARRIER(entry_464X, addr_469X, val_468X);
  *((long *) addr_469X) = (long) (val_468X);
  arg0K0 = next_467X;
  goto L12761;}
#ifndef USE_DIRECT_THREADING
 relocate_binding_table_bibopB_return:
  switch (relocate_binding_table_bibopB_return_tag) {
  case 0: goto relocate_binding_table_bibopB_return_0;
  default: goto relocate_binding_table_bibopB_return_1;
  }
#endif
}

 parse_reachable_objects: {
  from_addr_114X = merged_arg1K0;
  to_addr_115X = merged_arg1K1;
  image_format_116X = merged_arg0K2;{
  arg1K0 = from_addr_114X;
  goto L13296;}
 L13296: {
  current_addr_470X = arg1K0;
  if ((current_addr_470X == to_addr_115X)) {
    parse_reachable_objects0_return_value = 0;
#ifdef USE_DIRECT_THREADING
    goto *parse_reachable_objects_return_address;
#else
    goto parse_reachable_objects_return;
#endif
}
  else {
    merged_arg1K0 = current_addr_470X;
    merged_arg0K1 = image_format_116X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_6;
#else
    old_Gnew_addr_return_tag = 6;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_6:
    x1_471X = old_Gnew_addr0_return_value;
    cell_472X = *((long *) x1_471X);
    if ((2 == (3 & cell_472X))) {
      size_in_cells_473X = PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_472X, 8))), 2);
      if ((0 == (Sheap_object_remaining_cellsS))) {
        goto L13321;}
      else {
        ps_error("Encountered an header within an d-vector.", 0);
        goto L13321;}}
    else {
      if ((3 == (3 & cell_472X))) {
        current_addr_474X = ((char *) (-7 + cell_472X));
        if ((0 == (((long) current_addr_474X)))) {
          arg0K0 = -1;
          goto L13523;}
        else {
          arg0K0 = (((long) current_addr_474X));
          goto L13523;}}
      else {
        *((long *) (Sheap_object_pointerS)) = (long) (cell_472X);
        Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
        Sheap_object_pointerS = ((Sheap_object_pointerS) + 4);
        goto L13378;}}}}
 L13321: {
  if ((2 == (3 & cell_472X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(cell_472X, 2))) < 16)) {
      goto L13337;}
    else {
      if ((0 == (((long) current_addr_470X)))) {
        arg0K0 = -1;
        goto L13409;}
      else {
        arg0K0 = (((long) current_addr_470X));
        goto L13409;}}}
  else {
    goto L13337;}}
 L13523: {
  v_475X = arg0K0;
  v_476X = table_ref((Sstob_tableS), v_475X);
  if ((NULL == v_476X)) {
    current_addr_477X = ((char *) (-7 + cell_472X));
    merged_arg1K0 = current_addr_477X;
    merged_arg0K1 = image_format_116X;
#ifdef USE_DIRECT_THREADING
    old_Gnew_addr_return_address = &&old_Gnew_addr_return_7;
#else
    old_Gnew_addr_return_tag = 7;
#endif
    goto old_Gnew_addr;
   old_Gnew_addr_return_7:
    x1_478X = old_Gnew_addr0_return_value;
    header_cell_479X = *((long *) x1_478X);
    size_in_cells_480X = 1 + (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_cell_479X, 8))), 2));
    size_in_bytes_481X = PS_SHIFT_LEFT_INLINE(size_in_cells_480X, 2);
    if ((2 == (3 & header_cell_479X))) {
      if (((31 & (PS_SHIFT_RIGHT_INLINE(header_cell_479X, 2))) < 16)) {
        goto L12102;}
      else {
        new_address_482X = s48_allocate_tracedAgc((PS_SHIFT_LEFT_INLINE(size_in_cells_480X, 2)));
        image_location_483X = (struct image_location*)malloc(sizeof(struct image_location));
        if ((NULL == image_location_483X)) {
          arg2K0 = image_location_483X;
          goto L12161;}
        else {
          image_location_483X->new_descriptor = (((long) new_address_482X));
          image_location_483X->next = 0;
          arg2K0 = image_location_483X;
          goto L12161;}}}
    else {
      goto L12102;}}
  else {
    goto L13369;}}
 L13378: {
  arg1K0 = (current_addr_470X + 4);
  goto L13296;}
 L13337: {
  if ((0 == (((long) current_addr_470X)))) {
    arg0K0 = -1;
    goto L13456;}
  else {
    arg0K0 = (((long) current_addr_470X));
    goto L13456;}}
 L13409: {
  v_484X = arg0K0;
  v_485X = table_ref((Sstob_tableS), v_484X);
  if ((NULL == v_485X)) {
    size_in_cells_486X = 1 + size_in_cells_473X;
    new_address_487X = s48_allocate_tracedAgc((PS_SHIFT_LEFT_INLINE(size_in_cells_486X, 2)));
    image_location_488X = (struct image_location*)malloc(sizeof(struct image_location));
    if ((NULL == image_location_488X)) {
      arg2K0 = image_location_488X;
      goto L13427;}
    else {
      image_location_488X->new_descriptor = (((long) new_address_487X));
      image_location_488X->next = 0;
      arg2K0 = image_location_488X;
      goto L13427;}}
  else {
    goto L13334;}}
 L12102: {
  new_address_489X = s48_allocate_tracedAgc(size_in_bytes_481X);
  image_location_490X = (struct image_location*)malloc(sizeof(struct image_location));
  if ((NULL == image_location_490X)) {
    arg2K0 = image_location_490X;
    goto L12189;}
  else {
    image_location_490X->new_descriptor = (((long) new_address_489X));
    image_location_490X->next = 0;
    arg2K0 = image_location_490X;
    goto L12189;}}
 L12161: {
  val_491X = arg2K0;
  if ((0 == (((long) current_addr_477X)))) {
    arg0K0 = -1;
    goto L12163;}
  else {
    arg0K0 = (((long) current_addr_477X));
    goto L12163;}}
 L13369: {
  address_492X = ((char *) (-7 + cell_472X));
  if ((0 == (((long) address_492X)))) {
    arg0K0 = -1;
    goto L13537;}
  else {
    arg0K0 = (((long) address_492X));
    goto L13537;}}
 L13456: {
  v_493X = arg0K0;
  v_494X = table_ref((Sstob_tableS), v_493X);
  if ((NULL == v_494X)) {
    new_address_495X = s48_allocate_tracedAgc((4 + (PS_SHIFT_LEFT_INLINE(size_in_cells_473X, 2))));
    image_location_496X = (struct image_location*)malloc(sizeof(struct image_location));
    if ((NULL == image_location_496X)) {
      arg2K0 = image_location_496X;
      goto L11372;}
    else {
      image_location_496X->new_descriptor = (((long) new_address_495X));
      image_location_496X->next = 0;
      arg2K0 = image_location_496X;
      goto L11372;}}
  else {
    if ((0 == (((long) current_addr_470X)))) {
      arg0K0 = -1;
      goto L10599;}
    else {
      arg0K0 = (((long) current_addr_470X));
      goto L10599;}}}
 L13427: {
  val_497X = arg2K0;
  if ((0 == (((long) current_addr_470X)))) {
    arg0K0 = -1;
    goto L13429;}
  else {
    arg0K0 = (((long) current_addr_470X));
    goto L13429;}}
 L13334: {
  arg1K0 = (current_addr_470X + (4 + (PS_SHIFT_LEFT_INLINE(size_in_cells_473X, 2))));
  goto L13296;}
 L12189: {
  val_498X = arg2K0;
  if ((0 == (((long) current_addr_477X)))) {
    arg0K0 = -1;
    goto L12191;}
  else {
    arg0K0 = (((long) current_addr_477X));
    goto L12191;}}
 L12163: {
  v_499X = arg0K0;table_setB((Sstob_tableS), v_499X, val_491X);
  merged_arg1K0 = current_addr_477X;
  merged_arg0K1 = image_format_116X;
#ifdef USE_DIRECT_THREADING
  old_Gnew_addr_return_address = &&old_Gnew_addr_return_8;
#else
  old_Gnew_addr_return_tag = 8;
#endif
  goto old_Gnew_addr;
 old_Gnew_addr_return_8:
  v_500X = old_Gnew_addr0_return_value;
  memmove((void *)new_address_482X, (void *)v_500X,(PS_SHIFT_LEFT_INLINE(size_in_cells_480X, 2)));
  goto L13369;}
 L13537: {
  v_501X = arg0K0;
  image_location_502X = table_ref((Sstob_tableS), v_501X);
  *((long *) (Sheap_object_pointerS)) = (long) ((3 + (((long) ((((char *) (image_location_502X->new_descriptor))) + 4)))));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 4);
  goto L13378;}
 L11372: {
  val_503X = arg2K0;
  if ((0 == (((long) current_addr_470X)))) {
    arg0K0 = -1;
    goto L11374;}
  else {
    arg0K0 = (((long) current_addr_470X));
    goto L11374;}}
 L10599: {
  v_504X = arg0K0;
  image_location_505X = table_ref((Sstob_tableS), v_504X);
  new_address_506X = ((char *) (image_location_505X->new_descriptor));
  if ((2 == (3 & cell_472X))) {
    goto L10584;}
  else {
    ps_error("cell was not a header", 0);
    goto L10584;}}
 L13429: {
  v_507X = arg0K0;table_setB((Sstob_tableS), v_507X, val_497X);
  merged_arg1K0 = current_addr_470X;
  merged_arg0K1 = image_format_116X;
#ifdef USE_DIRECT_THREADING
  old_Gnew_addr_return_address = &&old_Gnew_addr_return_9;
#else
  old_Gnew_addr_return_tag = 9;
#endif
  goto old_Gnew_addr;
 old_Gnew_addr_return_9:
  v_508X = old_Gnew_addr0_return_value;
  memmove((void *)new_address_487X, (void *)v_508X,(PS_SHIFT_LEFT_INLINE(size_in_cells_486X, 2)));
  goto L13334;}
 L12191: {
  v_509X = arg0K0;table_setB((Sstob_tableS), v_509X, val_498X);
  *((long *) new_address_489X) = (long) ((-954 + (PS_SHIFT_LEFT_INLINE(size_in_bytes_481X, 8))));
  arg1K0 = (new_address_489X + 4);
  goto L12120;}
 L11374: {
  v_510X = arg0K0;table_setB((Sstob_tableS), v_510X, val_503X);
  if ((2 == (3 & cell_472X))) {
    goto L11351;}
  else {
    ps_error("cell was not a header", 0);
    goto L11351;}}
 L10584: {
  *((long *) new_address_506X) = (long) (cell_472X);
  Sheap_object_pointerS = new_address_506X;
  Sheap_object_remaining_cellsS = (1 + (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_472X, 8))), 2)));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 4);
  goto L13349;}
 L12120: {
  index_511X = arg1K0;
  if ((index_511X == (new_address_489X + (-4 & (3 + size_in_bytes_481X))))) {
    goto L13369;}
  else {
    *((long *) index_511X) = (long) (0);
    arg1K0 = (index_511X + 4);
    goto L12120;}}
 L11351: {
  *((long *) new_address_495X) = (long) (cell_472X);
  Sheap_object_pointerS = new_address_495X;
  Sheap_object_remaining_cellsS = (1 + (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(cell_472X, 8))), 2)));
  Sheap_object_remaining_cellsS = (-1 + (Sheap_object_remaining_cellsS));
  Sheap_object_pointerS = ((Sheap_object_pointerS) + 4);
  goto L13349;}
 L13349: {
  arg1K0 = (current_addr_470X + 4);
  goto L13296;}
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
  addr_112X = merged_arg1K0;
  format_113X = merged_arg0K1;{
  if ((0 == format_113X)) {
    old_Gnew_addr0_return_value = ((Sheap_image_pointerS) + (addr_112X - (Simg_start_addrS)));
#ifdef USE_DIRECT_THREADING
    goto *old_Gnew_addr_return_address;
#else
    goto old_Gnew_addr_return;
#endif
}
  else {
    if ((1 == format_113X)) {
      y_512X = Ssmall_img_start_addrS;
      if ((addr_112X < y_512X)) {
        goto L3742;}
      else {
        if (((Ssmall_img_hp_addrS) < addr_112X)) {
          goto L3742;}
        else {
          SoffsetS = (((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS)) + ((Slarge_img_end_addrS) - (Slarge_img_start_addrS)));
          Sarea_startS = (Ssmall_img_start_addrS);
          goto L3794;}}}
    else {
      ps_error("old->new-addr: Unknown image format", 0);
      old_Gnew_addr0_return_value = v_513X;
#ifdef USE_DIRECT_THREADING
      goto *old_Gnew_addr_return_address;
#else
      goto old_Gnew_addr_return;
#endif
}}}
 L3742: {
  y_514X = Slarge_img_start_addrS;
  if ((addr_112X < y_514X)) {
    goto L3768;}
  else {
    if (((Slarge_img_hp_addrS) < addr_112X)) {
      goto L3768;}
    else {
      SoffsetS = ((Sweaks_img_end_addrS) - (Sweaks_img_start_addrS));
      Sarea_startS = (Slarge_img_start_addrS);
      goto L3794;}}}
 L3794: {
  old_Gnew_addr0_return_value = ((Sheap_image_pointerS) + ((SoffsetS) + (addr_112X - (Sarea_startS))));
#ifdef USE_DIRECT_THREADING
  goto *old_Gnew_addr_return_address;
#else
  goto old_Gnew_addr_return;
#endif
}
 L3768: {
  y_515X = Sweaks_img_start_addrS;
  if ((addr_112X < y_515X)) {
    goto L3790;}
  else {
    if (((Sweaks_img_hp_addrS) < addr_112X)) {
      goto L3790;}
    else {
      SoffsetS = 0;
      Sarea_startS = (Sweaks_img_start_addrS);
      goto L3794;}}}
 L3790: {
  ps_error("Unknown address area!", 0);
  goto L3794;}
#ifndef USE_DIRECT_THREADING
 old_Gnew_addr_return:
  switch (old_Gnew_addr_return_tag) {
  case 0: goto old_Gnew_addr_return_0;
  case 1: goto old_Gnew_addr_return_1;
  case 2: goto old_Gnew_addr_return_2;
  case 3: goto old_Gnew_addr_return_3;
  case 4: goto old_Gnew_addr_return_4;
  case 5: goto old_Gnew_addr_return_5;
  case 6: goto old_Gnew_addr_return_6;
  case 7: goto old_Gnew_addr_return_7;
  case 8: goto old_Gnew_addr_return_8;
  default: goto old_Gnew_addr_return_9;
  }
#endif
}

 relocateD1: {
  address_111X = merged_arg0K0;{
  address_516X = ((char *) (-7 + address_111X));
  if ((0 == (((long) address_516X)))) {
    arg0K0 = -1;
    goto L12581;}
  else {
    arg0K0 = (((long) address_516X));
    goto L12581;}}
 L12581: {
  v_517X = arg0K0;
  image_location_518X = table_ref(stob_table_365X, v_517X);
  relocateD10_return_value = (3 + (((long) ((((char *) (image_location_518X->new_descriptor))) + 4))));
#ifdef USE_DIRECT_THREADING
  goto *relocateD1_return_address;
#else
  goto relocateD1_return;
#endif
}
#ifndef USE_DIRECT_THREADING
 relocateD1_return:
  switch (relocateD1_return_tag) {
  case 0: goto relocateD1_return_0;
  default: goto relocateD1_return_1;
  }
#endif
}

}
long s48_write_image(long resume_proc_519X, long undumpables_520X, FILE * port_521X)
{
  struct table *arg5K0;
  struct bibop_areas *arg7K0;
  long arg0K0;
  char * arg1K0;
  char * merged_arg1K0;
  long merged_arg0K1;
  long merged_arg0K0;

#ifdef USE_DIRECT_THREADING
  void *write_stob_return_address;
#else
  int write_stob_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *copy_image_data_return_address;
#else
  int copy_image_data_return_tag;
#endif
#ifdef USE_DIRECT_THREADING
  void *write_shared_table_return_address;
#else
  int write_shared_table_return_tag;
#endif
  char write_shared_table0_return_value;
  long stob_522X;
  char * start_523X;
  long size_524X;
  long table_525X;
  long value_689X;
  long v_688X;
  struct image_location *have_687X;
  long v_686X;
  long v_685X;
  struct image_location *have_684X;
  long thing_683X;
  struct image_location *have_682X;
  long thing_681X;
  long x_680X;
  char * addr_679X;
  long v_678X;
  long value_677X;
  struct image_location *have_676X;
  long thing_675X;
  char * addr_674X;
  char * start_673X;
  long header_672X;
  long next_671X;
  struct image_location *have_670X;
  long thing_669X;
  struct image_location *have_668X;
  char * addr_667X;
  long link_666X;
  struct image_location *v_665X;
  long next_664X;
  char * start_663X;
  long header_662X;
  long next_661X;
  long have_660X;
  long available_659X;
  long size_658X;
  char * start_657X;
  long header_656X;
  long shared_655X;
  long link_654X;
  struct image_location *v_653X;
  long next_652X;
  long link_651X;
  long symbol_650X;
  long v_649X;
  long link_648X;
  struct image_location *have_647X;
  long thing_646X;
  long id_645X;
  char * addr_644X;
  char * start_643X;
  long header_642X;
  struct image_location *have_641X;
  long thing_640X;
  struct image_location *v_639X;
  long thing_638X;
  long have_637X;
  long v_636X;
  long value_635X;
  struct image_location *have_634X;
  long thing_633X;
  long link_632X;
  struct image_location *v_631X;
  long next_630X;
  long shared_629X;
  long link_628X;
  char temp_627X;
  long i_626X;
  long v_625X;
  long value_624X;
  struct image_location *have_623X;
  long thing_622X;
  long i_621X;
  long link_620X;
  struct image_location *v_619X;
  long next_618X;
  struct image_location **values_617X;
  long *keys_616X;
  struct table *table_615X;
  long symbol_614X;
  long have_613X;
  char v_612X;
  struct image_location *location_611X;
  long stob_610X;
  long link_609X;
  long v_608X;
  long v_607X;
  long i_606X;
  long table_605X;
  long i_604X;
  long i_603X;
  long i_602X;
  long v_601X;
  long v_600X;
  long v_599X;
  long v_598X;
  long n_597X;
  struct image_location *have_596X;
  long thing_595X;
  long v_594X;
  long n_593X;
  struct image_location *have_592X;
  long thing_591X;
  long v_590X;
  long n_589X;
  struct image_location *have_588X;
  long thing_587X;
  long v_586X;
  long cells_585X;
  long v_584X;
  long cells_583X;
  long v_582X;
  long cells_581X;
  long v_580X;
  long cells_579X;
  long v_578X;
  long cells_577X;
  long v_576X;
  long cells_575X;
  long v_574X;
  long cells_573X;
  long v_572X;
  long v_571X;
  long v_570X;
  long v_569X;
  long i_568X;
  long v_567X;
  struct image_location *image_location_566X;
  long stob_565X;
  struct image_location **values_564X;
  long *keys_563X;
  struct table *table_562X;
  long v_561X;
  long resumer_records_560X;
  long i_559X;
  struct image_location *image_location_558X;
  long stob_557X;
  long i_556X;
  long n_555X;
  long v_554X;
  long n_553X;
  long v_552X;
  long n_551X;
  long v_550X;
  struct table *stob_table_549X;
  long v_548X;
  long v_547X;
  long v_546X;
  long v_545X;
  struct image_location *last_544X;
  char * addr_543X;
  long next_542X;
  struct image_location *image_location_541X;
  char * start_540X;
  long link_539X;
  long entry_538X;
  long header_537X;
  long stob_536X;
  long link_535X;
  long i_534X;
  long table_533X;
  long resume_proc_532X;
  struct bibop_areas *v_531X;
  struct bibop_areas *bibop_areas_530X;
  struct table *v_529X;
  struct table *table_528X;
  long i_527X;
  long *keys_526X;
 {  keys_526X = (long*)malloc(sizeof(long) * 4097);
  arg0K0 = 0;
  goto L7695;}
 L7695: {
  i_527X = arg0K0;
  if ((i_527X < 4097)) {
    *(keys_526X + i_527X) = 0;
    arg0K0 = (1 + i_527X);
    goto L7695;}
  else {
    table_528X = (struct table*)malloc(sizeof(struct table));
    if ((NULL == table_528X)) {
      arg5K0 = table_528X;
      goto L7668;}
    else {
      table_528X->keys = keys_526X;
      table_528X->values = ((struct image_location**)malloc(sizeof(struct image_location*) * 4096));
      table_528X->count = 0;
      table_528X->size = 4096;
      arg5K0 = table_528X;
      goto L7668;}}}
 L7668: {
  v_529X = arg5K0;
  Sstob_tableS = v_529X;
  Sfirst_stobS = 1;
  Slast_stobS = (NULL);
  Sundumpable_recordsS = undumpables_520X;
  Sundumpable_countS = 0;
  Sresumer_countS = 0;
  small_image_start_address = (((long) (Snew_small_start_addrS)));
  Ssmall_image_beginS = (((char *) (small_image_start_address)));
  Ssmall_image_hpS = (((char *) (small_image_start_address)));
  Ssmall_image_endS = NULL;
  Slarge_image_beginS = (((char *) 0));
  Slarge_image_hpS = (((char *) 0));
  Slarge_image_endS = NULL;
  Sweaks_image_beginS = (((char *) 0));
  Sweaks_image_hpS = (((char *) 0));
  Sweaks_image_endS = NULL;
  bibop_areas_530X = (struct bibop_areas*)malloc(sizeof(struct bibop_areas));
  if ((NULL == bibop_areas_530X)) {
    arg7K0 = bibop_areas_530X;
    goto L5188;}
  else {
    bibop_areas_530X->small = ((long*)malloc(sizeof(long) * 1048576));
    bibop_areas_530X->small_index = 0;
    bibop_areas_530X->large = ((long*)malloc(sizeof(long) * 1048576));
    bibop_areas_530X->large_index = 0;
    bibop_areas_530X->weaks = ((long*)malloc(sizeof(long) * 1048576));
    bibop_areas_530X->weaks_index = 0;
    arg7K0 = bibop_areas_530X;
    goto L5188;}}
 L5188: {
  v_531X = arg7K0;
  Sbibop_areasS = v_531X;
  Straced_last_stobPS = 0;
  Simage_portS = port_521X;
  Simage_bufferS = ((char *)malloc(4096));
  Simage_buffer_pointerS = (Simage_bufferS);
  SstatusS = NO_ERRORS;
  if (((Simage_bufferS) == NULL)) {
    return ENOMEM;}
  else {
    resume_proc_532X = trace_image_value(resume_proc_519X);
    table_533X = s48_exported_bindings();
    arg0K0 = 0;
    goto L11522;}}
 L11522: {
  i_534X = arg0K0;
  if ((1024 == i_534X)) {
    arg0K0 = (Sfirst_stobS);
    goto L15455;}
  else {
    link_535X = *((long *) ((((char *) (-3 + table_533X))) + (PS_SHIFT_LEFT_INLINE(i_534X, 2))));
    if ((0 == (3 & link_535X))) {
      arg0K0 = (3 + (-4 & link_535X));
      goto L11496;}
    else {
      arg0K0 = link_535X;
      goto L11496;}}}
 L15455: {
  stob_536X = arg0K0;
  header_537X = *((long *) (((char *) (-7 + stob_536X))));
  if ((2 == (3 & header_537X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(header_537X, 2))) < 16)) {
      goto L15319;}
    else {
      goto L15457;}}
  else {
    goto L15319;}}
 L11496: {
  entry_538X = arg0K0;
  if ((1 == entry_538X)) {
    arg0K0 = (1 + i_534X);
    goto L11522;}
  else {trace_image_value(entry_538X);
    link_539X = *((long *) ((((char *) (-3 + entry_538X))) + 12));
    if ((0 == (3 & link_539X))) {
      arg0K0 = (3 + (-4 & link_539X));
      goto L11496;}
    else {
      arg0K0 = link_539X;
      goto L11496;}}}
 L15319: {
  if ((1078 == header_537X)) {
    goto L15457;}
  else {
    start_540X = ((char *) (-3 + stob_536X));
    arg1K0 = start_540X;
    goto L15338;}}
 L15457: {
  if ((0 < ((Sstob_tableS)->size))) {
    image_location_541X = table_ref((Sstob_tableS), stob_536X);
    next_542X = image_location_541X->next;
    if ((3 == (3 & next_542X))) {
      arg0K0 = next_542X;
      goto L15455;}
    else {
      goto L15527;}}
  else {
    goto L15527;}}
 L15338: {
  addr_543X = arg1K0;
  if ((addr_543X == (start_540X + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_537X, 8))))))) {
    goto L15457;}
  else {trace_image_value((*((long *) addr_543X)));
    arg1K0 = (addr_543X + 4);
    goto L15338;}}
 L15527: {
  last_544X = Slast_stobS;
  Straced_last_stobPS = 1;
  v_545X = s48_symbol_table();trace_image_value(v_545X);
  v_546X = s48_imported_bindings();trace_image_value(v_546X);
  v_547X = s48_exported_bindings();trace_image_value(v_547X);
  last_544X->next = 1;
  v_548X = image_alloc(0, (PS_SHIFT_LEFT_INLINE((Sresumer_countS), 2)));
  Sresumer_recordsS = v_548X;
  stob_table_549X = Sstob_tableS;
  v_550X = (Ssmall_image_hpS) - (Ssmall_image_beginS);
  if ((0 == v_550X)) {
    arg0K0 = 1;
    goto L7565;}
  else {
    arg0K0 = ((Ssmall_image_hpS) - (Ssmall_image_beginS));
    goto L7565;}}
 L7565: {
  n_551X = arg0K0;
  Ssmall_image_endS = ((Ssmall_image_beginS) + (-4096 & (4095 + n_551X)));
  v_552X = (Slarge_image_hpS) - (Slarge_image_beginS);
  if ((0 == v_552X)) {
    arg0K0 = 1;
    goto L7576;}
  else {
    arg0K0 = ((Slarge_image_hpS) - (Slarge_image_beginS));
    goto L7576;}}
 L7576: {
  n_553X = arg0K0;
  Slarge_image_endS = ((Slarge_image_beginS) + (-4096 & (4095 + n_553X)));
  v_554X = (Sweaks_image_hpS) - (Sweaks_image_beginS);
  if ((0 == v_554X)) {
    arg0K0 = 1;
    goto L7587;}
  else {
    arg0K0 = ((Sweaks_image_hpS) - (Sweaks_image_beginS));
    goto L7587;}}
 L7587: {
  n_555X = arg0K0;
  Sweaks_image_endS = ((Sweaks_image_beginS) + (-4096 & (4095 + n_555X)));
  arg0K0 = 0;
  goto L4819;}
 L4819: {
  i_556X = arg0K0;
  if ((i_556X == ((Sbibop_areasS)->large_index))) {
    Slarge_image_beginS = ((Ssmall_image_endS) + (((long) (Slarge_image_beginS))));
    Slarge_image_hpS = ((Ssmall_image_endS) + (((long) (Slarge_image_hpS))));
    Slarge_image_endS = ((Ssmall_image_endS) + (((long) (Slarge_image_endS))));
    arg0K0 = 0;
    goto L4954;}
  else {
    stob_557X = *(((Sbibop_areasS)->large) + i_556X);
    if ((3 == (3 & stob_557X))) {
      if ((1 == stob_557X)) {
        goto L4861;}
      else {
        image_location_558X = table_ref(stob_table_549X, stob_557X);
        image_location_558X->new_descriptor = (3 + (((long) ((Ssmall_image_endS) + (((long) (((char *) (-3 + (image_location_558X->new_descriptor))))))))));
        goto L4861;}}
    else {
      goto L4861;}}}
 L4954: {
  i_559X = arg0K0;
  if ((i_559X == ((Sbibop_areasS)->weaks_index))) {
    Sweaks_image_beginS = ((Slarge_image_endS) + (((long) (Sweaks_image_beginS))));
    Sweaks_image_hpS = ((Slarge_image_endS) + (((long) (Sweaks_image_hpS))));
    Sweaks_image_endS = ((Slarge_image_endS) + (((long) (Sweaks_image_endS))));
    if ((0 < ((Sstob_tableS)->size))) {
      resumer_records_560X = Sresumer_recordsS;
      if (((SstatusS) == NO_ERRORS)) {
        PS_WRITE_CHAR(10, port_521X, v_561X)
        SstatusS = v_561X;
        goto L6881;}
      else {
        goto L6881;}}
    else {
      table_562X = Sstob_tableS;
      keys_563X = table_562X->keys;
      values_564X = table_562X->values;
      arg0K0 = 0;
      goto L15841;}}
  else {
    stob_565X = *(((Sbibop_areasS)->weaks) + i_559X);
    if ((3 == (3 & stob_565X))) {
      if ((1 == stob_565X)) {
        goto L4996;}
      else {
        image_location_566X = table_ref(stob_table_549X, stob_565X);
        image_location_566X->new_descriptor = (3 + (((long) ((Slarge_image_endS) + (((long) (((char *) (-3 + (image_location_566X->new_descriptor))))))))));
        goto L4996;}}
    else {
      goto L4996;}}}
 L4861: {
  arg0K0 = (1 + i_556X);
  goto L4819;}
 L6881: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(12, (Simage_portS), v_567X)
    SstatusS = v_567X;
    goto L6883;}
  else {
    goto L6883;}}
 L15841: {
  i_568X = arg0K0;
  if ((i_568X == (table_562X->size))) {
    free(keys_563X);
    free(values_564X);
    free(table_562X);
    free((Simage_bufferS));
    return ENOMEM;}
  else {
    if ((0 == (*(keys_563X + i_568X)))) {
      goto L15843;}
    else {
      free((*(values_564X + i_568X)));
      goto L15843;}}}
 L4996: {
  arg0K0 = (1 + i_559X);
  goto L4954;}
 L6883: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_521X, v_569X)
    SstatusS = v_569X;
    goto L6892;}
  else {
    goto L6892;}}
 L15843: {
  arg0K0 = (1 + i_568X);
  goto L15841;}
 L6892: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_string("Vanilla 40", port_521X));
    goto L6899;}
  else {
    goto L6899;}}
 L6899: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_521X, v_570X)
    SstatusS = v_570X;
    goto L6908;}
  else {
    goto L6908;}}
 L6908: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(1, port_521X));
    goto L6915;}
  else {
    goto L6915;}}
 L6915: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, port_521X, v_571X)
    SstatusS = v_571X;
    goto L6924;}
  else {
    goto L6924;}}
 L6924: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(4, (Simage_portS)));
    goto L6987;}
  else {
    goto L6987;}}
 L6987: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_572X)
    SstatusS = v_572X;
    goto L6926;}
  else {
    goto L6926;}}
 L6926: {
  cells_573X = ((long) (Ssmall_image_beginS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_573X, 2)), (Simage_portS)));
    goto L7004;}
  else {
    goto L7004;}}
 L7004: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_574X)
    SstatusS = v_574X;
    goto L6930;}
  else {
    goto L6930;}}
 L6930: {
  cells_575X = ((long) (Ssmall_image_hpS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_575X, 2)), (Simage_portS)));
    goto L7021;}
  else {
    goto L7021;}}
 L7021: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_576X)
    SstatusS = v_576X;
    goto L6934;}
  else {
    goto L6934;}}
 L6934: {
  cells_577X = ((long) (Slarge_image_beginS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_577X, 2)), (Simage_portS)));
    goto L7038;}
  else {
    goto L7038;}}
 L7038: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_578X)
    SstatusS = v_578X;
    goto L6938;}
  else {
    goto L6938;}}
 L6938: {
  cells_579X = ((long) (Slarge_image_hpS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_579X, 2)), (Simage_portS)));
    goto L7055;}
  else {
    goto L7055;}}
 L7055: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_580X)
    SstatusS = v_580X;
    goto L6942;}
  else {
    goto L6942;}}
 L6942: {
  cells_581X = ((long) (Sweaks_image_beginS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_581X, 2)), (Simage_portS)));
    goto L7072;}
  else {
    goto L7072;}}
 L7072: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_582X)
    SstatusS = v_582X;
    goto L6946;}
  else {
    goto L6946;}}
 L6946: {
  cells_583X = ((long) (Sweaks_image_hpS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_583X, 2)), (Simage_portS)));
    goto L7089;}
  else {
    goto L7089;}}
 L7089: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_584X)
    SstatusS = v_584X;
    goto L6950;}
  else {
    goto L6950;}}
 L6950: {
  cells_585X = ((long) (Sweaks_image_endS));
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer((PS_SHIFT_RIGHT_INLINE(cells_585X, 2)), (Simage_portS)));
    goto L7106;}
  else {
    goto L7106;}}
 L7106: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_586X)
    SstatusS = v_586X;
    goto L6954;}
  else {
    goto L6954;}}
 L6954: {
  thing_587X = s48_symbol_table();
  if ((3 == (3 & thing_587X))) {
    have_588X = table_ref((Sstob_tableS), thing_587X);
    if ((NULL == have_588X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L16084;}
    else {
      goto L16084;}}
  else {
    arg0K0 = thing_587X;
    goto L6958;}}
 L16084: {
  arg0K0 = (have_588X->new_descriptor);
  goto L6958;}
 L6958: {
  n_589X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_589X, (Simage_portS)));
    goto L7120;}
  else {
    goto L7120;}}
 L7120: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_590X)
    SstatusS = v_590X;
    goto L6960;}
  else {
    goto L6960;}}
 L6960: {
  thing_591X = s48_imported_bindings();
  if ((3 == (3 & thing_591X))) {
    have_592X = table_ref((Sstob_tableS), thing_591X);
    if ((NULL == have_592X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L16098;}
    else {
      goto L16098;}}
  else {
    arg0K0 = thing_591X;
    goto L6964;}}
 L16098: {
  arg0K0 = (have_592X->new_descriptor);
  goto L6964;}
 L6964: {
  n_593X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_593X, (Simage_portS)));
    goto L7134;}
  else {
    goto L7134;}}
 L7134: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_594X)
    SstatusS = v_594X;
    goto L6966;}
  else {
    goto L6966;}}
 L6966: {
  thing_595X = s48_exported_bindings();
  if ((3 == (3 & thing_595X))) {
    have_596X = table_ref((Sstob_tableS), thing_595X);
    if ((NULL == have_596X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L16112;}
    else {
      goto L16112;}}
  else {
    arg0K0 = thing_595X;
    goto L6970;}}
 L16112: {
  arg0K0 = (have_596X->new_descriptor);
  goto L6970;}
 L6970: {
  n_597X = arg0K0;
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(n_597X, (Simage_portS)));
    goto L7148;}
  else {
    goto L7148;}}
 L7148: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_598X)
    SstatusS = v_598X;
    goto L6972;}
  else {
    goto L6972;}}
 L6972: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(resumer_records_560X, (Simage_portS)));
    goto L7162;}
  else {
    goto L7162;}}
 L7162: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_599X)
    SstatusS = v_599X;
    goto L6974;}
  else {
    goto L6974;}}
 L6974: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_integer(resume_proc_532X, (Simage_portS)));
    goto L7176;}
  else {
    goto L7176;}}
 L7176: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(10, (Simage_portS), v_600X)
    SstatusS = v_600X;
    goto L6976;}
  else {
    goto L6976;}}
 L6976: {
  if (((SstatusS) == NO_ERRORS)) {
    PS_WRITE_CHAR(12, (Simage_portS), v_601X)
    SstatusS = v_601X;
    goto L15742;}
  else {
    goto L15742;}}
 L15742: {
write_descriptor(1);
  arg0K0 = 0;
  goto L4500;}
 L4500: {
  i_602X = arg0K0;
  if ((i_602X == ((Sbibop_areasS)->weaks_index))) {
    goto L4488;}
  else {
    if ((1 == (*(((Sbibop_areasS)->weaks) + i_602X)))) {
      goto L4488;}
    else {
      merged_arg0K0 = (*(((Sbibop_areasS)->weaks) + i_602X));
#ifdef USE_DIRECT_THREADING
      write_stob_return_address = &&write_stob_return_0;
#else
      write_stob_return_tag = 0;
#endif
      goto write_stob;
     write_stob_return_0:
      arg0K0 = (1 + i_602X);
      goto L4500;}}}
 L4488: {
  arg0K0 = 0;
  goto L4519;}
 L4519: {
  i_603X = arg0K0;
  if ((i_603X == ((Sbibop_areasS)->large_index))) {
    goto L4490;}
  else {
    if ((1 == (*(((Sbibop_areasS)->large) + i_603X)))) {
      goto L4490;}
    else {
      merged_arg0K0 = (*(((Sbibop_areasS)->large) + i_603X));
#ifdef USE_DIRECT_THREADING
      write_stob_return_address = &&write_stob_return_1;
#else
      write_stob_return_tag = 1;
#endif
      goto write_stob;
     write_stob_return_1:
      arg0K0 = (1 + i_603X);
      goto L4519;}}}
 L4490: {
  arg0K0 = 0;
  goto L4538;}
 L4538: {
  i_604X = arg0K0;
  if ((i_604X == ((Sbibop_areasS)->small_index))) {
    goto L14944;}
  else {
    if ((1 == (*(((Sbibop_areasS)->small) + i_604X)))) {
      goto L14944;}
    else {
      merged_arg0K0 = (*(((Sbibop_areasS)->small) + i_604X));
#ifdef USE_DIRECT_THREADING
      write_stob_return_address = &&write_stob_return_2;
#else
      write_stob_return_tag = 2;
#endif
      goto write_stob;
     write_stob_return_2:
      arg0K0 = (1 + i_604X);
      goto L4538;}}}
 L14944: {
  table_605X = s48_symbol_table();write_descriptor((*((long *) (((char *) (-7 + table_605X))))));
  arg0K0 = 0;
  goto L11908;}
 L11908: {
  i_606X = arg0K0;
  if ((i_606X == (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-7 + table_605X))))), 8))), 2)))) {
    v_607X = s48_imported_bindings();
    merged_arg0K0 = v_607X;
#ifdef USE_DIRECT_THREADING
    write_shared_table_return_address = &&write_shared_table_return_0;
#else
    write_shared_table_return_tag = 0;
#endif
    goto write_shared_table;
   write_shared_table_return_0:
    v_608X = s48_exported_bindings();
    merged_arg0K0 = v_608X;
#ifdef USE_DIRECT_THREADING
    write_shared_table_return_address = &&write_shared_table_return_1;
#else
    write_shared_table_return_tag = 1;
#endif
    goto write_shared_table;
   write_shared_table_return_1:write_descriptor((10 + (PS_SHIFT_LEFT_INLINE((Sresumer_countS), 10))));
    arg0K0 = (Sfirst_stobS);
    goto L14965;}
  else {
    link_609X = *((long *) ((((char *) (-3 + table_605X))) + (PS_SHIFT_LEFT_INLINE(i_606X, 2))));
    if ((0 == (3 & link_609X))) {
      arg0K0 = (3 + (-4 & link_609X));
      goto L11918;}
    else {
      arg0K0 = link_609X;
      goto L11918;}}}
 L14965: {
  stob_610X = arg0K0;
  if ((3 == (3 & stob_610X))) {
    location_611X = table_ref((Sstob_tableS), stob_610X);
    v_612X = resumer_recordP(stob_610X);
    if (v_612X) {write_descriptor((location_611X->new_descriptor));
      goto L14972;}
    else {
      goto L14972;}}
  else {
    have_613X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_613X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_613X));
        goto L15787;}
      else {
        goto L15787;}}
    else {
      goto L15748;}}}
 L11918: {
  symbol_614X = arg0K0;
  arg0K0 = symbol_614X;
  goto L11952;}
 L14972: {
  arg0K0 = (location_611X->next);
  goto L14965;}
 L15787: {
  Simage_buffer_pointerS = (Simage_bufferS);
  goto L15748;}
 L15748: {
  table_615X = Sstob_tableS;
  keys_616X = table_615X->keys;
  values_617X = table_615X->values;
  arg0K0 = 0;
  goto L15807;}
 L11952: {
  next_618X = arg0K0;
  if ((3 == (3 & next_618X))) {
    v_619X = table_ref((Sstob_tableS), next_618X);
    if ((NULL == v_619X)) {
      link_620X = *((long *) ((((char *) (-3 + next_618X))) + 4));
      if ((0 == (3 & link_620X))) {
        arg0K0 = (3 + (-4 & link_620X));
        goto L11952;}
      else {
        arg0K0 = link_620X;
        goto L11952;}}
    else {
      arg0K0 = next_618X;
      goto L11920;}}
  else {
    arg0K0 = next_618X;
    goto L11920;}}
 L15807: {
  i_621X = arg0K0;
  if ((i_621X == (table_615X->size))) {
    free(keys_616X);
    free(values_617X);
    free(table_615X);
    free((Simage_bufferS));
    return (SstatusS);}
  else {
    if ((0 == (*(keys_616X + i_621X)))) {
      goto L15809;}
    else {
      free((*(values_617X + i_621X)));
      goto L15809;}}}
 L11920: {
  thing_622X = arg0K0;
  if ((3 == (3 & thing_622X))) {
    have_623X = table_ref((Sstob_tableS), thing_622X);
    if ((NULL == have_623X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L11971;}
    else {
      goto L11971;}}
  else {
    arg0K0 = thing_622X;
    goto L11922;}}
 L15809: {
  arg0K0 = (1 + i_621X);
  goto L15807;}
 L11971: {
  arg0K0 = (have_623X->new_descriptor);
  goto L11922;}
 L11922: {
  value_624X = arg0K0;
  if ((3 == (3 & value_624X))) {
    arg0K0 = (-4 & value_624X);
    goto L11924;}
  else {
    arg0K0 = value_624X;
    goto L11924;}}
 L11924: {
  v_625X = arg0K0;write_descriptor(v_625X);
  arg0K0 = (1 + i_606X);
  goto L11908;}
 write_shared_table: {
  table_525X = merged_arg0K0;{write_descriptor((*((long *) (((char *) (-7 + table_525X))))));
  arg0K0 = 0;
  goto L11998;}
 L11998: {
  i_626X = arg0K0;
  temp_627X = i_626X == (PS_SHIFT_RIGHT_INLINE((3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE((*((long *) (((char *) (-7 + table_525X))))), 8))), 2));
  if (temp_627X) {
    write_shared_table0_return_value = temp_627X;
#ifdef USE_DIRECT_THREADING
    goto *write_shared_table_return_address;
#else
    goto write_shared_table_return;
#endif
}
  else {
    link_628X = *((long *) ((((char *) (-3 + table_525X))) + (PS_SHIFT_LEFT_INLINE(i_626X, 2))));
    if ((0 == (3 & link_628X))) {
      arg0K0 = (3 + (-4 & link_628X));
      goto L12008;}
    else {
      arg0K0 = link_628X;
      goto L12008;}}}
 L12008: {
  shared_629X = arg0K0;
  arg0K0 = shared_629X;
  goto L12042;}
 L12042: {
  next_630X = arg0K0;
  if ((3 == (3 & next_630X))) {
    v_631X = table_ref((Sstob_tableS), next_630X);
    if ((NULL == v_631X)) {
      link_632X = *((long *) ((((char *) (-3 + next_630X))) + 12));
      if ((0 == (3 & link_632X))) {
        arg0K0 = (3 + (-4 & link_632X));
        goto L12042;}
      else {
        arg0K0 = link_632X;
        goto L12042;}}
    else {
      arg0K0 = next_630X;
      goto L12010;}}
  else {
    arg0K0 = next_630X;
    goto L12010;}}
 L12010: {
  thing_633X = arg0K0;
  if ((3 == (3 & thing_633X))) {
    have_634X = table_ref((Sstob_tableS), thing_633X);
    if ((NULL == have_634X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L12061;}
    else {
      goto L12061;}}
  else {
    arg0K0 = thing_633X;
    goto L12012;}}
 L12061: {
  arg0K0 = (have_634X->new_descriptor);
  goto L12012;}
 L12012: {
  value_635X = arg0K0;
  if ((3 == (3 & value_635X))) {
    arg0K0 = (-4 & value_635X);
    goto L12014;}
  else {
    arg0K0 = value_635X;
    goto L12014;}}
 L12014: {
  v_636X = arg0K0;write_descriptor(v_636X);
  arg0K0 = (1 + i_626X);
  goto L11998;}
#ifndef USE_DIRECT_THREADING
 write_shared_table_return:
  switch (write_shared_table_return_tag) {
  case 0: goto write_shared_table_return_0;
  default: goto write_shared_table_return_1;
  }
#endif
}

 copy_image_data: {
  start_523X = merged_arg1K0;
  size_524X = merged_arg0K1;{
  memmove((void *)(Simage_buffer_pointerS), (void *)start_523X,size_524X);
  Simage_buffer_pointerS = ((Simage_buffer_pointerS) + size_524X);
  if ((4096 == ((Simage_buffer_pointerS) - (Simage_bufferS)))) {
    have_637X = (Simage_buffer_pointerS) - (Simage_bufferS);
    if ((0 < have_637X)) {
      if (((SstatusS) == NO_ERRORS)) {
        SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_637X));
        goto L7408;}
      else {
        goto L7408;}}
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
 L7408: {
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

 write_stob: {
  stob_522X = merged_arg0K0;{
  if ((3 == (3 & stob_522X))) {
    if ((13 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + stob_522X))))), 2))))) {write_descriptor(1078);
      thing_638X = *((long *) (((char *) (-3 + stob_522X))));
      if ((3 == (3 & thing_638X))) {
        v_639X = table_ref((Sstob_tableS), thing_638X);
        if ((NULL == v_639X)) {
          write_descriptor(1);
#ifdef USE_DIRECT_THREADING
          goto *write_stob_return_address;
#else
          goto write_stob_return;
#endif
}
        else {
          goto L14636;}}
      else {
        goto L14636;}}
    else {
      goto L14642;}}
  else {
    goto L14642;}}
 L14636: {
  thing_640X = *((long *) (((char *) (-3 + stob_522X))));
  if ((3 == (3 & thing_640X))) {
    have_641X = table_ref((Sstob_tableS), thing_640X);
    if ((NULL == have_641X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L14700;}
    else {
      goto L14700;}}
  else {
    write_descriptor(thing_640X);
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}}
 L14642: {
  if ((3 == (3 & stob_522X))) {
    if ((6 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + stob_522X))))), 2))))) {
      header_642X = *((long *) (((char *) (-7 + stob_522X))));write_descriptor(header_642X);write_descriptor(0);
      start_643X = (((char *) (-3 + stob_522X))) + 4;
      arg1K0 = start_643X;
      goto L10755;}
    else {
      goto L14646;}}
  else {
    goto L14646;}}
 L14700: {
  write_descriptor((have_641X->new_descriptor));
#ifdef USE_DIRECT_THREADING
  goto *write_stob_return_address;
#else
  goto write_stob_return;
#endif
}
 L10755: {
  addr_644X = arg1K0;
  if ((addr_644X == (start_643X + (-4 + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_642X, 8)))))))) {
    ps_write_string("Channel closed in dumped image: ", (stderr));
    id_645X = *((long *) ((((char *) (-3 + stob_522X))) + 4));
    if ((0 == (3 & id_645X))) {
      ps_write_integer((PS_SHIFT_RIGHT_INLINE(id_645X, 2)), (stderr));
      goto L10732;}
    else {
      ps_write_string((((char *)(((char *) (-3 + id_645X))))), (stderr));
      goto L10732;}}
  else {
    thing_646X = *((long *) addr_644X);
    if ((3 == (3 & thing_646X))) {
      have_647X = table_ref((Sstob_tableS), thing_646X);
      if ((NULL == have_647X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L10769;}
      else {
        goto L10769;}}
    else {
      arg0K0 = thing_646X;
      goto L10760;}}}
 L14646: {
  if ((3 == (3 & stob_522X))) {
    if ((1 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + stob_522X))))), 2))))) {
      link_648X = *((long *) ((((char *) (-3 + stob_522X))) + 4));
      if ((0 == (3 & link_648X))) {
        arg0K0 = (3 + (-4 & link_648X));
        goto L13024;}
      else {
        arg0K0 = link_648X;
        goto L13024;}}
    else {
      goto L14650;}}
  else {
    goto L14650;}}
 L10732: {
  { long ignoreXX;
  PS_WRITE_CHAR(10, (stderr), ignoreXX) }
#ifdef USE_DIRECT_THREADING
  goto *write_stob_return_address;
#else
  goto write_stob_return;
#endif
}
 L10769: {
  arg0K0 = (have_647X->new_descriptor);
  goto L10760;}
 L10760: {
  v_649X = arg0K0;write_descriptor(v_649X);
  arg1K0 = (addr_644X + 4);
  goto L10755;}
 L13024: {
  symbol_650X = arg0K0;
  arg0K0 = symbol_650X;
  goto L13057;}
 L14650: {
  if ((3 == (3 & stob_522X))) {
    if ((14 == (31 & (PS_SHIFT_RIGHT_INLINE((*((long *) (((char *) (-7 + stob_522X))))), 2))))) {
      link_651X = *((long *) ((((char *) (-3 + stob_522X))) + 12));
      if ((0 == (3 & link_651X))) {
        arg0K0 = (3 + (-4 & link_651X));
        goto L13139;}
      else {
        arg0K0 = link_651X;
        goto L13139;}}
    else {
      goto L14654;}}
  else {
    goto L14654;}}
 L13057: {
  next_652X = arg0K0;
  if ((3 == (3 & next_652X))) {
    v_653X = table_ref((Sstob_tableS), next_652X);
    if ((NULL == v_653X)) {
      link_654X = *((long *) ((((char *) (-3 + next_652X))) + 4));
      if ((0 == (3 & link_654X))) {
        arg0K0 = (3 + (-4 & link_654X));
        goto L13057;}
      else {
        arg0K0 = link_654X;
        goto L13057;}}
    else {
      arg0K0 = next_652X;
      goto L13026;}}
  else {
    arg0K0 = next_652X;
    goto L13026;}}
 L13139: {
  shared_655X = arg0K0;
  arg0K0 = shared_655X;
  goto L13188;}
 L14654: {
  header_656X = *((long *) (((char *) (-7 + stob_522X))));
  start_657X = ((char *) (-3 + stob_522X));write_descriptor(header_656X);
  if ((2 == (3 & header_656X))) {
    if (((31 & (PS_SHIFT_RIGHT_INLINE(header_656X, 2))) < 16)) {
      goto L14670;}
    else {
      size_658X = -4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_656X, 8)));
      available_659X = 4096 - ((Simage_buffer_pointerS) - (Simage_bufferS));
      if ((available_659X < size_658X)) {
        if ((4096 < size_658X)) {
          have_660X = (Simage_buffer_pointerS) - (Simage_bufferS);
          if ((0 < have_660X)) {
            if (((SstatusS) == NO_ERRORS)) {
              SstatusS = (ps_write_block((Simage_portS), ((char *) (Simage_bufferS)), have_660X));
              goto L9844;}
            else {
              goto L9844;}}
          else {
            goto L9818;}}
        else {
          merged_arg1K0 = start_657X;
          merged_arg0K1 = available_659X;
#ifdef USE_DIRECT_THREADING
          copy_image_data_return_address = &&copy_image_data_return_0;
#else
          copy_image_data_return_tag = 0;
#endif
          goto copy_image_data;
         copy_image_data_return_0:
          merged_arg1K0 = (start_657X + available_659X);
          merged_arg0K1 = (size_658X - available_659X);
#ifdef USE_DIRECT_THREADING
          copy_image_data_return_address = &&copy_image_data_return_1;
#else
          copy_image_data_return_tag = 1;
#endif
          goto copy_image_data;
         copy_image_data_return_1:
#ifdef USE_DIRECT_THREADING
          goto *write_stob_return_address;
#else
          goto write_stob_return;
#endif
}}
      else {
        merged_arg1K0 = start_657X;
        merged_arg0K1 = size_658X;
#ifdef USE_DIRECT_THREADING
        copy_image_data_return_address = &&copy_image_data_return_2;
#else
        copy_image_data_return_tag = 2;
#endif
        goto copy_image_data;
       copy_image_data_return_2:
#ifdef USE_DIRECT_THREADING
        goto *write_stob_return_address;
#else
        goto write_stob_return;
#endif
}}}
  else {
    goto L14670;}}
 L13026: {
  next_661X = arg0K0;
  header_662X = *((long *) (((char *) (-7 + stob_522X))));write_descriptor(header_662X);
  start_663X = ((char *) (-3 + stob_522X));
  arg1K0 = start_663X;
  goto L13086;}
 L13188: {
  next_664X = arg0K0;
  if ((3 == (3 & next_664X))) {
    v_665X = table_ref((Sstob_tableS), next_664X);
    if ((NULL == v_665X)) {
      link_666X = *((long *) ((((char *) (-3 + next_664X))) + 12));
      if ((0 == (3 & link_666X))) {
        arg0K0 = (3 + (-4 & link_666X));
        goto L13188;}
      else {
        arg0K0 = link_666X;
        goto L13188;}}
    else {
      arg0K0 = next_664X;
      goto L13141;}}
  else {
    arg0K0 = next_664X;
    goto L13141;}}
 L14670: {
  arg1K0 = start_657X;
  goto L14772;}
 L9844: {
  Simage_buffer_pointerS = (Simage_bufferS);
  goto L9818;}
 L9818: {
  if (((SstatusS) == NO_ERRORS)) {
    SstatusS = (ps_write_block((Simage_portS), ((char *) start_657X), size_658X));
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}
  else {
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}}
 L13086: {
  addr_667X = arg1K0;
  if ((addr_667X == (start_663X + (-4 + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_662X, 8)))))))) {
    if ((3 == (3 & next_661X))) {
      have_668X = table_ref((Sstob_tableS), next_661X);
      if ((NULL == have_668X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13116;}
      else {
        goto L13116;}}
    else {
      arg0K0 = next_661X;
      goto L13039;}}
  else {
    thing_669X = *((long *) addr_667X);
    if ((3 == (3 & thing_669X))) {
      have_670X = table_ref((Sstob_tableS), thing_669X);
      if ((NULL == have_670X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13100;}
      else {
        goto L13100;}}
    else {
      arg0K0 = thing_669X;
      goto L13091;}}}
 L13141: {
  next_671X = arg0K0;
  header_672X = *((long *) (((char *) (-7 + stob_522X))));write_descriptor(header_672X);
  start_673X = ((char *) (-3 + stob_522X));
  arg1K0 = start_673X;
  goto L13217;}
 L14772: {
  addr_674X = arg1K0;
  if ((addr_674X == (start_657X + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_656X, 8))))))) {
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}
  else {
    thing_675X = *((long *) addr_674X);
    if ((3 == (3 & thing_675X))) {
      have_676X = table_ref((Sstob_tableS), thing_675X);
      if ((NULL == have_676X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L14786;}
      else {
        goto L14786;}}
    else {
      arg0K0 = thing_675X;
      goto L14777;}}}
 L13116: {
  arg0K0 = (have_668X->new_descriptor);
  goto L13039;}
 L13039: {
  value_677X = arg0K0;
  if ((3 == (3 & value_677X))) {
    write_descriptor((-4 & value_677X));
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}
  else {
    write_descriptor(value_677X);
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}}
 L13100: {
  arg0K0 = (have_670X->new_descriptor);
  goto L13091;}
 L13091: {
  v_678X = arg0K0;write_descriptor(v_678X);
  arg1K0 = (addr_667X + 4);
  goto L13086;}
 L13217: {
  addr_679X = arg1K0;
  if ((addr_679X == (start_673X + (-8 + (-4 & (3 + (PS_SHIFT_RIGHT_LOGICAL_INLINE(header_672X, 8)))))))) {
    x_680X = *((long *) ((((char *) (-3 + stob_522X))) + 4));
    if ((5 == x_680X)) {
      arg0K0 = 529;
      goto L13166;}
    else {
      thing_681X = *((long *) ((((char *) (-3 + stob_522X))) + 8));
      if ((3 == (3 & thing_681X))) {
        have_682X = table_ref((Sstob_tableS), thing_681X);
        if ((NULL == have_682X)) {
          ps_error("traced object has no descriptor in image", 0);
          goto L13278;}
        else {
          goto L13278;}}
      else {
        arg0K0 = thing_681X;
        goto L13166;}}}
  else {
    thing_683X = *((long *) addr_679X);
    if ((3 == (3 & thing_683X))) {
      have_684X = table_ref((Sstob_tableS), thing_683X);
      if ((NULL == have_684X)) {
        ps_error("traced object has no descriptor in image", 0);
        goto L13231;}
      else {
        goto L13231;}}
    else {
      arg0K0 = thing_683X;
      goto L13222;}}}
 L14786: {
  arg0K0 = (have_676X->new_descriptor);
  goto L14777;}
 L14777: {
  v_685X = arg0K0;write_descriptor(v_685X);
  arg1K0 = (addr_674X + 4);
  goto L14772;}
 L13166: {
  v_686X = arg0K0;write_descriptor(v_686X);
  if ((3 == (3 & next_671X))) {
    have_687X = table_ref((Sstob_tableS), next_671X);
    if ((NULL == have_687X)) {
      ps_error("traced object has no descriptor in image", 0);
      goto L13254;}
    else {
      goto L13254;}}
  else {
    arg0K0 = next_671X;
    goto L13170;}}
 L13278: {
  arg0K0 = (have_682X->new_descriptor);
  goto L13166;}
 L13231: {
  arg0K0 = (have_684X->new_descriptor);
  goto L13222;}
 L13222: {
  v_688X = arg0K0;write_descriptor(v_688X);
  arg1K0 = (addr_679X + 4);
  goto L13217;}
 L13254: {
  arg0K0 = (have_687X->new_descriptor);
  goto L13170;}
 L13170: {
  value_689X = arg0K0;
  if ((3 == (3 & value_689X))) {
    write_descriptor((-4 & value_689X));
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}
  else {
    write_descriptor(value_689X);
#ifdef USE_DIRECT_THREADING
    goto *write_stob_return_address;
#else
    goto write_stob_return;
#endif
}}
#ifndef USE_DIRECT_THREADING
 write_stob_return:
  switch (write_stob_return_tag) {
  case 0: goto write_stob_return_0;
  case 1: goto write_stob_return_1;
  default: goto write_stob_return_2;
  }
#endif
}

}void
s48_heap_init(void)
{
Smax_heap_sizeS = 0;
Smin_heap_sizeS = 0;
Snew_small_start_addrS = NULL;
Snew_large_start_addrS = NULL;
Snew_weaks_start_addrS = NULL;
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
small_image_start_address = 0;
Straced_last_stobPS = 0;
Sstob_table_obj_nrS = 0;
}
