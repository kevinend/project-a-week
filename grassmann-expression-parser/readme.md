The files contained in this directory implement a simple parser for Grassmann (exterior algebra) expressions.
The Backus-Naur forms (BNF) are specified for the types of expressions that the parsing and evaluation routines handle
and then procedures are implemented to those specs.

Grassmann's products especially the interior product between different graded elements require recursive expansion
from interior products to interior products to eventually scalar products that are cumbersome to complete by hand.
Intention is to be able to expand and evaluate these expressions.
