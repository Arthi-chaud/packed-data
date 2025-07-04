[0.2.0.0] — July 2025 

* __Breaking Changes__:
  * Complete rewrite of the 'NeedsBuilder'
    * Please refer to the examples to see how to build packed data
  * Dropping support for GHC < 9.10. 
    * 'NeedsBuilder' relies on linear types and type inference is broken before that version

* Features:
  * 'PackedReader' has been rewritten, however the API should be retro-compatible
  * The code generator defines pattern synonyms, allowing one to pattern-match on packed data using a native 'case' expression
    * Refer to the examples (e.g. Sum) to see how it works
    * The generated 'case' functions are still generated, but they use these pattern synonyms under the hood
