#pragma once

/// AST
enum ASTTag { VAL, ADD, SUB, DIV, MUL };
struct AST {
  enum ASTTag tag;
  union {
    // Content of a leaf
    struct {
      long value;
    };
    // Content of a node
    struct {
      struct AST *left;
      struct AST *right;
    };
  };
};

long eval(struct AST *t);
struct AST *build_ast(int size);
void free_ast(struct AST *t);

/// Tree
enum Tag { LEAF, NODE };
struct Tree {
  enum Tag tag;
  union {
    // Content of a leaf
    struct {
      long value;
    };

    // Content of a node
    struct {
      struct Tree *left;
      struct Tree *right;
    };
  };
};

long sum(struct Tree *t);
long get_right_most(struct Tree *t);
struct Tree *build_tree(int size);
void increment_inplace(struct Tree *t);
struct Tree *increment(struct Tree *t);
void free_tree(struct Tree *t);
