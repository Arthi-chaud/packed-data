# haskell-gibbon

## Benchmark

To run benchmarks, run the following command:

```
stack bench
# Saves the report as CSV
stack bench --ba --csv bench.csv
# Saves the report, and runs a specific test
stach bench --ba '--csv bench.csv sums'
```

Note: Graphs of the benchmark results will be generated in the `graph/` directory when saving the report as CSV.
