# Day 3: Spiral Memory

You come across an experimental new kind of memory stored on an
infinite two-dimensional grid.

Each square on the grid is allocated in a spiral pattern 
starting at a location marked 1 and then counting up while
spiraling outward. For example, the first few squares are
allocated like this:

    17  16  15  14  13
    18   5   4   3  12
    19   6   1   2  11
    20   7   8   9  10
    21  22  23---> ...
    
    
While this is very space-efficient (no squares are skipped), 
requested data must be carried back to square 1 (the location 
of the only access port for this memory system) by programs 
that can only move up, down, left, or right. They always take 
the shortest path: the Manhattan Distance between the location 
of the data and square 1.

For example:

- Data from square 1 is carried 0 steps, since it's at the access port.
- Data from square 12 is carried 3 steps, such as: down, left, left.
- Data from square 23 is carried only 2 steps: up twice.
- Data from square 1024 must be carried 31 steps.

How many steps are required to carry the data from the square 
identified in your puzzle input all the way to the access port?

## Part Two

As a stress test on the system, the programs here clear the 
grid and then store the value 1 in square 1. Then, in the 
same allocation order as shown above, they store the sum of 
the values in all adjacent squares, including diagonals.

So, the first few squares' values are chosen as follows:

- Square 1 starts with the value 1.

- Square 2 has only one adjacent filled square (with value 1),
so it also stores 1.

- Square 3 has both of the above squares as neighbors and 
stores the sum of their values, 2.

- Square 4 has all three of the aforementioned squares as 
neighbors and stores the sum of their values, 4.

- Square 5 only has the first and fourth squares as neighbors,
so it gets the value 5.

Once a square is written, its value does not change. 
Therefore, the first few squares would receive the following values:

    147  142  133  122   59
    304    5    4    2   57
    330   10    1    1   54
    351   11   23   25   26
    362  747  806--->   ...

What is the first value written that is larger than 
your puzzle input?


# Solutions
The algorithm used here is based on this observation:
If you look at how the steps keep increasing just in
one direction (say downwards) you get:

          0
        2 1 2
      4 3 2 3 4
    6 5 4 3 4 5 6
    
basically you can copy the last line and
increase the step by one but then you have
two more edges that need an additional step
(say move in the direction of the sides center).

So I just generate this as a sequence and wrap
it around a existing layer like this:

    bbbba
    c221a
    c301a
    c344a
    cdddd
    
you need 4 copies of the sides above where
you skip the first edge so that you don't
write it two times.

---

## Part 2

For the second part I got trapped by the *clever*
way I solved part 1 and I was really to lazy to
change my approach.

Luckyly the sequence is actually one of the *known*
[Oeis sequences](https://oeis.org/A141481) so I just
looked up the solution there.

### Haskell solution
The solution to part 1 is [here](./Day3.hs)

I came back and solved it just like you would on paper.
The algorithm produces the coords for the spiral numbers
with a similar approach to the one above.

After it `scanr` a map with the known values to
produce a sequence of values and new maps.

The sequence of those found numbers is then just
searched for the first value bigger then the
problems input.

As this runs in under a second even with `runhaskell`
I'm perfectly fine with it.

## C# solution
I redid the day using [C# here](./Day3.cs).

It uses direct formulas where I succeded in finding them in a few
minutes but for the coords I was to lazy and just enumerate and pick
the ring coords instead.