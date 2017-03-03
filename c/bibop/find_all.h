/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: David Frese
 */

#ifndef __S48_GC_FIND_ALL_H
#define __S48_GC_FIND_ALL_H

#include "scheme48.h"

extern s48_value s48_find_all(long type);
extern s48_value s48_find_all_records(s48_value record_type);
extern s48_value s48_gather_objects(char(*predicate)(s48_value),
				    char(*for_each_object)(char(*)(s48_value)));

#endif
