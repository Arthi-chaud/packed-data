
#include "benchmark.h"
#include <assert.h>
#include <math.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

// static long get_nanos(void) {
//   struct timespec ts;
//   timespec_get(&ts, TIME_UTC);
//   return (long)ts.tv_sec * 1000000000L + ts.tv_nsec;
// }

//// AST

long eval(struct AST *t) {
  long left;
  long right;
  switch (t->tag) {
  case VAL:
    return t->value;
  case ADD:
    left = eval(t->left);
    right = eval(t->right);
    return left + right;
  case SUB:
    left = eval(t->left);
    right = eval(t->right);
    return left - right;
  case MUL:
    left = eval(t->left);
    right = eval(t->right);
    return left * right;
  case DIV:
    left = eval(t->left);
    right = eval(t->right);
    return left / right;
  }
}

struct AST *build_ast(int size) {
  struct AST *t = malloc(sizeof(struct AST));

  if (size <= 0) {
    t->tag = VAL;
    t->value = 1;
  } else {
    t->tag = ADD;
    t->left = build_ast(size - 1);
    t->right = build_ast(size - 1);
  }

  return t;
}

void free_ast(struct AST *t) {

  if (t->tag != VAL) {
    free_ast(t->left);
    free_ast(t->right);
  }
  free(t);
}

/// Tree
long sum(struct Tree *t) {
  if (t->tag == LEAF) {
    return t->value;
  } else {
    return sum(t->left) + sum(t->right);
  }
}

long get_right_most(struct Tree *t) {
  if (t->tag == LEAF) {
    return t->value;
  } else {
    return get_right_most(t->right);
  }
}

struct Tree *increment(struct Tree *t) {
  struct Tree *res = malloc(sizeof(struct Tree));
  if (t->tag == LEAF) {
    res->left = NULL;
    res->right = NULL;
    res->value = t->value + 1;
  } else {
    res->left = increment(t->left);
    res->right = increment(t->right);
  }
  return res;
}

void increment_inplace(struct Tree *t) {
  if (t->tag == LEAF) {
    t->value++;
  } else {
    increment(t->left);
    increment(t->right);
  }
}

struct Tree *build_tree(int size) {
  struct Tree *t = malloc(sizeof(struct Tree));

  if (size <= 0) {
    t->tag = LEAF;
    t->value = 1;
  } else {
    t->tag = NODE;
    t->left = build_tree(size - 1);
    t->right = build_tree(size - 1);
  }

  return t;
}

void free_tree(struct Tree *t) {

  if (t->tag == NODE) {
    free_tree(t->left);
    free_tree(t->right);
  }
  free(t);
}

////////

// void time_sum(int size, struct Tree *tree) {
//   long begin = get_nanos();
//
//   long res = sum(tree);
//
//   long time_spent = (get_nanos() - begin);
//
//   assert(res == pow(2, size));
//   free_tree(tree);
//   // todo free tree
//   printf("Size: %d\n", size);
//   printf("Execution time %ldns\n", time_spent);
// }
//
// void time_traversal(int size, struct Tree *tree) {
//   long begin = get_nanos();
//
//   long res = get_right_most(tree);
//
//   long time_spent = (get_nanos() - begin);
//
//   free_tree(tree);
//   printf("Size: %d\n", size);
//   printf("Execution time %ldns\n", time_spent);
// }

// void time_eval(int size, struct AST *tree) {
//   long begin = get_nanos();
//
//   long res = eval(tree);
//
//   long time_spent = (get_nanos() - begin);
//
//   free_ast(tree);
//   printf("Size: %d\n", size);
//   printf("Execution time %ldns\n", time_spent);
// }
//
// int main(int argc, char **argv) {
//   // TODO Check "sum", "right-most"
//   if (argc < 2) {
//     printf("Expected at least one argument\n");
//     return 1;
//   }
//   if (strcmp(argv[1], "sum") == 0) {
//     printf("right-most\n");
//     if (argc == 3) {
//
//       int tree_size = atoi(argv[2]);
//       time_sum(tree_size, build_tree(tree_size));
//       return 0;
//     }
//     for (int tree_size = 0; tree_size <= 20; tree_size++) {
//
//       time_sum(tree_size, build_tree(tree_size));
//     }
//   } else if (strcmp(argv[1], "right-most") == 0) {
//     printf("right-most\n");
//     if (argc == 3) {
//       int tree_size = atoi(argv[2]);
//       time_traversal(tree_size, build_tree(tree_size));
//       return 0;
//     }
//     for (int tree_size = 0; tree_size <= 20; tree_size++) {
//
//       time_traversal(tree_size, build_tree(tree_size));
//     }
//
//   } else if (strcmp(argv[1], "ast") == 0) {
//     printf("ast\n");
//     if (argc == 3) {
//       int tree_size = atoi(argv[2]);
//       time_eval(tree_size, build_ast(tree_size));
//       return 0;
//     }
//     for (int tree_size = 0; tree_size <= 20; tree_size++) {
//
//       time_eval(tree_size, build_ast(tree_size));
//     }
//   }
// }
