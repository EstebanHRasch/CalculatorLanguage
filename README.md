# Project

Follow the [instructions](INSTRUCTIONS.md), use this space to document your project for yourself and the graders.

## Names
Esteban Hernandez;
Elyse Kaczmarek;
Paul Menexas;


## Summary
Our language is a basic integer and boolean based "calculator" language. It's capable of variable assignments, if statements, while loops, return statements, and all basic boolean and integer operations (or, and, not, multiplication, integer division, modulus, addition, subtraction), in addition to all of the basic comparison operators (less than, greater than, less than equal to, greater than equal to, equality, inequality). 

We set it up such that there are three "layers" -- the module layer (the package used to contain all statements), the statement layer (assignments, if statements, while statements, and return statements), and the expression layer (all boolean, arithmetic, and comparison operations).

The module contains a list of statements enclosed in curly brackets, called a sequence. A sequence is just a list of statements meant to be run in sequential order, dileniated by semicolons at the end of every statement. The return statement returns a value immediately after being called.

An example module would look as follows: 

myModuleName {
	stmt;
	stmt;
	if (expr) {
		stmt;
		stmt;
	};
	return expr;
} 

## Plan
We implemented strict evaluation -- everything is evaluated as soon as possible. For the most part, we all worked together on most of the sections, but generally speaking, Paul implemeneted the language, Elyse implemented the parser, and Esteban implemented the tests. However, there was a lot of crossover and we all contributed to all of the sections at one point or another.

We chose to implement Euler problems 1, 2, 3, 31, 6, and 9 in our language.
