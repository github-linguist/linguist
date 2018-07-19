#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <yajl/yajl_tree.h>
#include <yajl/yajl_gen.h>

static void print_callback (void *ctx, const char *str, size_t len)
{
  FILE *f = (FILE *) ctx;
  fwrite (str, 1, len, f);
}

static void check_status (yajl_gen_status status)
{
  if (status != yajl_gen_status_ok)
    {
      fprintf (stderr, "yajl_gen_status was %d\n", (int) status);
      exit (EXIT_FAILURE);
    }
}

static void serialize_value (yajl_gen gen, yajl_val val, int parse_numbers)
{
  size_t i;

  switch (val->type)
    {
    case yajl_t_string:
      check_status (yajl_gen_string (gen,
                                     (const unsigned char *) val->u.string,
                                     strlen (val->u.string)));
      break;
    case yajl_t_number:
      if (parse_numbers  &&  YAJL_IS_INTEGER (val))
        check_status (yajl_gen_integer (gen, YAJL_GET_INTEGER (val)));
      else if (parse_numbers  &&  YAJL_IS_DOUBLE (val))
        check_status (yajl_gen_double (gen, YAJL_GET_DOUBLE (val)));
      else
        check_status (yajl_gen_number (gen, YAJL_GET_NUMBER (val),
                                       strlen (YAJL_GET_NUMBER (val))));
      break;
    case yajl_t_object:
      check_status (yajl_gen_map_open (gen));
      for (i = 0  ;  i < val->u.object.len  ;  i++)
        {
          check_status (yajl_gen_string (gen,
                                         (const unsigned char *) val->u.object.keys[i],
                                         strlen (val->u.object.keys[i])));
          serialize_value (gen, val->u.object.values[i], parse_numbers);
        }
      check_status (yajl_gen_map_close (gen));
      break;
    case yajl_t_array:
      check_status (yajl_gen_array_open (gen));
      for (i = 0  ;  i < val->u.array.len  ;  i++)
        serialize_value (gen, val->u.array.values[i], parse_numbers);
      check_status (yajl_gen_array_close (gen));
      break;
    case yajl_t_true:
      check_status (yajl_gen_bool (gen, 1));
      break;
    case yajl_t_false:
      check_status (yajl_gen_bool (gen, 0));
      break;
    case yajl_t_null:
      check_status (yajl_gen_null (gen));
      break;
    default:
      fprintf (stderr, "unexpectedly got type %d\n", (int) val->type);
      exit (EXIT_FAILURE);
    }
}

static void print_tree (FILE *f, yajl_val tree, int parse_numbers)
{
  yajl_gen gen;

  gen = yajl_gen_alloc (NULL);
  if (! gen)
    {
      fprintf (stderr, "yajl_gen_alloc failed\n");
      exit (EXIT_FAILURE);
    }

  if (0 == yajl_gen_config (gen, yajl_gen_beautify, 1)  ||
      0 == yajl_gen_config (gen, yajl_gen_validate_utf8, 1)  ||
      0 == yajl_gen_config (gen, yajl_gen_print_callback, print_callback, f))
    {
      fprintf (stderr, "yajl_gen_config failed\n");
      exit (EXIT_FAILURE);
    }

  serialize_value (gen, tree, parse_numbers);
  yajl_gen_free (gen);
}

int main (int argc, char **argv)
{
  char err_buf[200];
  const char *json =
    "{\"pi\": 3.14, \"large number\": 123456789123456789123456789, "
    "\"an array\": [-1, true, false, null, \"foo\"]}";
  yajl_val tree;

  tree = yajl_tree_parse (json, err_buf, sizeof (err_buf));
  if (! tree)
    {
      fprintf (stderr, "parsing failed because: %s\n", err_buf);
      return EXIT_FAILURE;
    }

  printf ("Treating numbers as strings...\n");
  print_tree (stdout, tree, 0);
  printf ("Parsing numbers to long long or double...\n");
  print_tree (stdout, tree, 1);

  yajl_tree_free (tree);

  return EXIT_SUCCESS;
}
