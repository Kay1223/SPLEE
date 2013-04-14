## Simple Propositional Logic Expression Evaluation
### - Written in Haskell

This program simplifies and evaluates propositional logic expression  

* the result may or may not be in its simplest form if it cannot be evaluated to a sinlge value  
* if can be evaluated to a single value, it has to be the simplest form  

It will print out:

1. How the program understands your input
2. The evaluated result

## Usage

	$ ghc --make logic.hs

	$ logic
	((((A => B) | A) & ~B) => f)
	
**Outer brackets can be omitted:**

	(((A => B) | A) & ~B) => f
	
**Spaces can be ommited:**

	(((A=>B)|A)&~B)=>f

**Output for the sample input above:**  
*output uses `V` for disjunction*

	((((A => B) V A) & ~B) => f)
	((A V B) & (~A V B))

proposition variable: `Single Capital Character`  
not: `~`  
conjunction: `&`  
disjunction: `|`  
implication: `=>`  
tautology: `t`  
contradiction: `f`  

*By Po Chen*  
*chenpaul914@gmail.com*  
*[pochen.me](http://pochen.me/)*