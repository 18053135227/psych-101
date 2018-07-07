## NOTE: Definitions / nomenclature
## Union of A and B: A U B
## Contains all elementary outcomes in A, B, or both.

## Intersection of A and B: AB
## Contains all elementary outcomes that are in both A and B.

## Compliment of A: compliment of A
## Contains all elementary events that are not in A


## NOTE:
## Definitions and nomenclature
## arrangements / permutations care about order:
## ABC != CBA

## combinations do not care about order
## ABC == CBA

## gtools provides these useful functions:
## The "repeats" argument is whether or not sampling is with replacement
combinations(3,2,letters[1:3])
combinations(3,2,letters[1:3],repeats=TRUE)

permutations(3,2,letters[1:3])
permutations(3,2,letters[1:3],repeats=TRUE)

## NOTE: Definitions and nomenclature
## The number of ways to arrange r objects selected from n distinct objects is:
## n * (n-1) * (n-2) * ... * (n-r+1)

## which is equal to the first r terms of n! and can be expressed:
## n! / (n-r)!


## NOTE: Definitions and nomenclature
## The number of combinations of r objects chosen from n objects is given by:
## "n choose r"

## TODO: write this on the board because the notation is bit tricky

## (the number of ways to arrange r objects selected from n objects) /
## (the number of ways to arrange r objects)

## (n!/(n-r)!) / r!
## R has this built in...
## choose(n, r)


## NOTE: Definition and nomenclature --- conditional probability
## The probability of A given that B has already occurred
## The probability of A conditioned on B
## P(A|B)
## P(A|B) = P(AB) / P(B)
## P(AB) = P(B) * P(A|B)


## NOTE: Definition and nomenclature --- Independence
## Two events A and B are independent if:
## P(A|B) = P(A)
## P(B|A) = P(B)
## P(AB) = P(A) * P(B)
