# wangsAlgorithmLisp


Wang's algorithm is a means of proving theorems in the Propositional
Calculus. 

   1. Write the premises down separated by commas, followed by an arrow and
      then the theorem to be proved. Eg:

         p, ~q ^ r     -->  p ^ ~q

      The aim is now to progressively remove connectives from this (using the
      rules 2, 3, 4 below) until condition 5 or condition 6 is met.

   2. If one of the formulae separated by commas is the negation of a
      formula, drop the negation sign and move it to the other side of the
      arrow. Eg:

         p, ~(q ^ r)   --> p ^ r

      is changed to:

         p --> p ^ r, q ^ r

   3. If the principal connective of a formula on the left is ^ (and), or on
      the right of the arrow is v (or), replace the connective by a comma.
      Eg:

         p, p ^ q  --> r, s

      is changed to:

         p, p , q  --> r, s

      and

         p, q --> r v p

      is changed to:

         p, q --> r, p

   4. If the principal connective of a formula on the left is v (or), or on
      the right of the arrow is ^ (and), then produce two new lines, each
      with one of the two sub-formulae replacing the formula. Both of these
      must be proved in order to prove the original theorem. Eg:

         p v q, r, s  --> q v p

      is changed to the two lines:

         p, r, s --> q v p
         q, r, s --> q v p

      Each of these must be independently successfully dealt with in order
      that the proof of the original formula is successful.

   5. If the same formula ever appears on both sides of an arrow, the line is
      proved.

   6. If no connectives (~, ^ or v) remain in a line, and no proposition
      appears on both sides of the arrow, the line cannot be proved.

References
----------

TEACH WANG  (reproduced here)                                Mellish, Chris  January 1983

Lemmon, E.J. "Beginning Logic", Nelson, 1972.

Raphael, B. "The Thinking Computer", Freeman, 1976 (Chapter 4).
