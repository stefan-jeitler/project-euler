# Problem 68

## Some Observations

The graph can be seen as a pentagon surrounded by another pentagon.

Properties of the inner pentagon are:

- Each number occurs twice
- The numbers form a cycle, similar to cycles in Problem 61
  - e.g. Triangluar set of three in the form of  (*outer, **inner**, **inner** ; outer, ...*)
  - 4, **2, 3** ; 5, **3, 1** ; 6, **1, 2**
  - (2, 3) -> (3, 1) -> (1, 2) -> the last number is equal to the first number

Properties of the outer pentagon are:

- Each number occurs only once

In order to form a 16-digits string the number 10 must occur only once, which means it belongs to the outer pentagon.

The problem description also states that we have to start with the smallest external node. The largest possible first digit is obtained by placing the largest numbers in the outer pentagon, 6-10, and start with the smallest of these. By simply sorting the remaining numbers in descending order we get the result ``[6, 10, 9, 8, 7]``.

## My Solution

With all that knowledge the problem can be solved in five steps.

1. Split the numbers into two sets, the outer set with the largest five numbers and the inner set with the smallest five numbers.  
2. The next step is to create pairs of the inner numbers that form a cycle.  
3. Sort the outer numbers in descending order expect the first number in the list.
4. Create combinations with the inner cycles and the outer numbers that form a ``List<List<GroupOfThree>>``.
5. Pick all inner lists that share the same total number, concatenate the groups of three and find the max value
