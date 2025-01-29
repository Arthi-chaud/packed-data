package main

import "testing"

// We'll consider that a tree is a leaf if both pointers are null
// otherwise we won't use the Value field
type Tree struct {
	Left  *Tree
	Value int
	Right *Tree
}

func BuildTree(n int) *Tree {
	if n == 0 {
		return &Tree{Value: 1}
	}
	return &Tree{Left: BuildTree(n - 1), Right: BuildTree(n - 1)}
}

func Sum(t *Tree) int {
	if t.Left == nil && t.Right == nil {
		return t.Value
	}
	return Sum(t.Left) + Sum(t.Right)
}

func SumN(b *testing.B, n int) {
	t := BuildTree(n)
	b.ResetTimer()
	for range b.N {
		Sum(t)
	}
}

func BenchmarkSum_1(b *testing.B)  { SumN(b, 1) }
func BenchmarkSum_5(b *testing.B)  { SumN(b, 5) }
func BenchmarkSum_10(b *testing.B) { SumN(b, 10) }
func BenchmarkSum_15(b *testing.B) { SumN(b, 15) }
func BenchmarkSum_20(b *testing.B) { SumN(b, 20) }

func GetRightMost(t *Tree) int {
	if t.Left == nil && t.Right == nil {
		return t.Value
	}
	return GetRightMost(t.Right)
}

func GetRightMostN(b *testing.B, n int) {
	t := BuildTree(n)
	b.ResetTimer()
	for range b.N {
		GetRightMost(t)
	}
}

func BenchmarkGetRightMost_1(b *testing.B)  { GetRightMostN(b, 1) }
func BenchmarkGetRightMost_5(b *testing.B)  { GetRightMostN(b, 5) }
func BenchmarkGetRightMost_10(b *testing.B) { GetRightMostN(b, 10) }
func BenchmarkGetRightMost_15(b *testing.B) { GetRightMostN(b, 15) }
func BenchmarkGetRightMost_20(b *testing.B) { GetRightMostN(b, 20) }

func Increment(t *Tree) {
	if t.Left == nil && t.Right == nil {
		t.Value = t.Value + 1
	} else {
		Increment(t.Left)
		Increment(t.Right)
	}
}

func IncrementN(b *testing.B, n int) {

	t := BuildTree(n)
	b.ResetTimer()
	for range b.N {
		Increment(t)
	}
}

func BenchmarkIncrement_1(b *testing.B)  { IncrementN(b, 1) }
func BenchmarkIncrement_5(b *testing.B)  { IncrementN(b, 5) }
func BenchmarkIncrement_10(b *testing.B) { IncrementN(b, 10) }
func BenchmarkIncrement_15(b *testing.B) { IncrementN(b, 15) }
func BenchmarkIncrement_20(b *testing.B) { IncrementN(b, 20) }

type AST struct {
	Value int
	Op    *ASTBinOp
}

type ASTBinOp struct {
	Left  *AST
	Right *AST
}

func BuildAST(n int) *AST {
	if n == 0 {
		return &AST{Value: 1}
	}
	op := ASTBinOp{Left: BuildAST(n - 1), Right: BuildAST(n - 1)}
	return &AST{Op: &op}
}

func Eval(t *AST) int {
	if t.Op == nil {
		return t.Value
	}
	left := Eval(t.Op.Left)
	right := Eval(t.Op.Right)
	return left + right
}

func EvalN(b *testing.B, n int) {
	t := BuildAST(n)
	b.ResetTimer()
	for range b.N {
		Eval(t)
	}
}

func BenchmarkEval_1(b *testing.B)  { EvalN(b, 1) }
func BenchmarkEval_5(b *testing.B)  { EvalN(b, 5) }
func BenchmarkEval_10(b *testing.B) { EvalN(b, 10) }
func BenchmarkEval_15(b *testing.B) { EvalN(b, 15) }
func BenchmarkEval_20(b *testing.B) { EvalN(b, 20) }
