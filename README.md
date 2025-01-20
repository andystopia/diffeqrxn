## DiffEqRxn


# DiffEqRxn : A Faster Way To Write Mass Action Kinetics!


## What is DiffEqRxn?

It\'s a way to represent a mass action model textually and then export
equivalent forms to typesetters and programming languages.

## Why create DiffEqRxn?

Mass action kinetics have a lot of repetition and tediously updated
equations. What\'s more is that all of this work needs to be repeated
multiple times across languages and typesetters and I have a couple of
notable *skill issues* when writing them.

1.  The amount of tedium means that often global variables sneak into
    the equations.
2.  Copying the model into a language from a typeset definition often
    leads to a simple typo which subtley bugs the model with no
    immediate way to spot it.
3.  It\'s hard to see the dynamics of the system immediately just from
    the equations; while possible, I get lost in the sea of symbols that
    define the models.
4.  Tedium leads to shortcuts
5.  Computers do tedious things. Humans do creative things.

## Use Cases.

This program was developed for use with systems biology where reactions
are often expressed as a set of differential equations, and our use
cases uses it for modeling circadian systems and light entrainment.

## Further Reading.

I have written documentation at https://andystopia.github.io/diffeqrxn-docs/.
