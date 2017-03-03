/*
 * Part of Scheme 48 1.9.  See file COPYING for notices and license.
 *
 * Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike Sperber
 */

extern long s48_initialize(int *argc, char ***argv);
extern long s48_call_startup_procedure(char **, long);

int
main(int argc, char **argv)
{
  long return_value;

  return_value = s48_initialize(&argc, &argv);

  if (return_value != 0)
    return return_value;
  else
    return s48_call_startup_procedure(argv, argc);
}
