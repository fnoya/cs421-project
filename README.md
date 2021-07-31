# Improving the performance of the puzzle ``Escape from Zurg'' by applying pruning methods

Reducing the search space is critical for search programs that have to deal with spaces that tend to grow exponentially. Functional programming can produce elegant algorithms that can benefit from features of modern functional languages such as lazy data structures. Using the ``Escape from Zurg'' puzzle, I converted the original exhaustive search algorithm into a more efficient version by means of two pruning methods based on a lazy data structure called *improving sequence*. After applying these techniques the running time for finding the optimal solution was slashed by over 20 times and the amount of memory used was reduced by 95%.

Run with 
```
stack ghci app/Main.hs
```

Please read [Final report.pdf](./Final report.pdf) for further information.
