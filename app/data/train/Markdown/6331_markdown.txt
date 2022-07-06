dartExpressionProblem - untyped version
=====================

This code shows a solution to the expression problem using mixins. At the top level here are 5 files:

abstractExpressions.dart shows an initial type hierarchy for an AST for a simple expression language.
evaluator.dart defines an evaluator on this AST.
multiplication.dart adds a new type and provides the necessary extension to the evaluator, separately.
stringConverter.dart defines toString() on the extended hierarchy including multiplication.
expressions.dart ties everything together. 

The assumption is that we started without multiplication and without any functions. Then we added evaluation,
followed by adding the AST node for multiplication. Next we added string conversion. Each extension is separate.
The only file that needs to be modified at each stage is expressions.dart.

Stricter formulations of the expression problem require typechecking, and even modular typechecking. 
See the simplyTyped version for a statically typed version of this code, and the generic version for  a version
that can be typechecked in a modular fashion.

dart-expression-problem
