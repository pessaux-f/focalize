
#ifndef __C_BUILTINS_H
#define __C_BUILTINS_H

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define PTR_SIZE (sizeof(void*))

typedef struct foc_value 
{
  int (*equal)(struct foc_value*, struct foc_value*);
} foc_value;

/* typedef struct foc_variant */
/* { */
/*   int (*equal)(struct foc_variant*, struct foc_variant*); */
/*   int _type; */
/*   char _arity; */
/* } foc_variant; */

/* int variant_equal(foc_variant*, foc_variant*); */

foc_value* mk_tuple(int);
void set_tuple(foc_value*, int, foc_value*);
foc_value* get_tuple(foc_value*, int);

int structural_equality(foc_value*, foc_value*);

typedef foc_value foc_int;
/* struct foc_int; */
/* typedef struct foc_int foc_int; */
foc_int* mk_foc_int(int);

typedef foc_value foc_unit;
/* struct foc_unit; */
/* typedef struct foc_unit foc_unit; */
inline foc_unit* mk_foc_unit(void);

typedef foc_value foc_float;
/* struct foc_float; */
/* typedef struct foc_float foc_float; */
foc_float* mk_foc_float(float);

typedef foc_value foc_char;
/* struct foc_char; */
/* typedef struct foc_char foc_char; */
foc_char* mk_foc_char(char);

typedef foc_value foc_string;
/* struct foc_string; */
/* typedef struct foc_string foc_string; */
foc_string* mk_foc_string(char*);

typedef foc_value foc_bool;
/* struct foc_bool; */
/* typedef struct foc_bool foc_bool; */
/* typedef enum {TRUE, FALSE} foc_bool_type; */
/* inline foc_bool_type get_foc_bool_type(foc_bool*); */
inline foc_bool* mk_foc_true(void);
inline foc_bool* mk_foc_false(void);

typedef foc_value foc_list;
/* struct foc_list; */
/* typedef struct foc_list foc_list; */
/* typedef enum {NIL, CONS} foc_list_type; */
/* inline foc_list_type get_foc_list_type(foc_list*); */
/* inline foc_list* mk_foc_nil(void); */
/* foc_list* mk_foc_cons(foc_value*, foc_list*); */

foc_value* c_builtins_foc_error(foc_string*);

foc_string* c_builtins_string_of_int(foc_int*);

foc_int* c_builtins_int_of_string(foc_string*);

foc_unit* c_builtins_print_int(foc_int*);

foc_int* c_builtins_int_mod(foc_int*, foc_int*);

foc_bool* c_builtins_int_eq(foc_int*, foc_int*);

foc_int* c_builtins_int_div(foc_int*, foc_int*);

foc_bool* c_builtins_int_lt(foc_int*, foc_int*);

foc_bool* c_builtins_int_leq(foc_int*, foc_int*);

foc_bool* c_builtins_int_geq(foc_int*, foc_int*);

foc_bool* c_builtins_int_gt(foc_int*, foc_int*);

foc_bool* c_builtins_phys_eq(foc_value*, foc_value*);

foc_bool* c_builtins_base_eq(foc_value*, foc_value*);

foc_int* c_builtins_pred(foc_int*);

foc_int* c_builtins_int_opp(foc_int*);

foc_int* c_builtins_int_plus(foc_int*, foc_int*);

foc_int* c_builtins_int_mult(foc_int*, foc_int*);

foc_int* c_builtins_int_minus(foc_int*, foc_int*);

foc_int* c_builtins_int_max(foc_int*, foc_int*);

foc_int* c_builtins_int_min(foc_int*, foc_int*);

foc_string* c_builtins_sc(foc_string*, foc_string*);

foc_bool* c_builtins_str_lt(foc_string*, foc_string*);

foc_unit* c_builtins_print_string(foc_string*);

#endif
