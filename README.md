## Simple Propositional Logic Expression Evaluation
### - Written in Haskell

Title says all.

## Usage

	$ ghc --make logic.hs

	$ logic
	((((A => B) | A) & ~B) => f)
	
**Outer brackets can be omitted:**

	(((A => B) | A) & ~B) => f
	
**Spaces can be ommited:**

	(((A=>B)|A)&~B)=>f

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