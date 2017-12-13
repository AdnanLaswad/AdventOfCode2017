# Day 13: Packet Scanners

Problem: see [Day 13 - AoC](https://adventofcode.com/2017/day/13)

---

You need to cross a vast firewall. The firewall consists of several layers,
each with a security scanner that moves back and forth across the layer. To
succeed, you must not be detected by a scanner.

By studying the firewall briefly, you are able to record (in your puzzle input)
the depth of each layer and the range of the scanning area for the scanner
within it, written as depth: range. Each layer has a thickness of exactly 1. A
layer at depth 0 begins immediately inside the firewall; a layer at depth 1
would start immediately after that.


---

## Part 2

Now, you need to pass through the firewall without being caught - easier said
than done.

You can't control the speed of the packet, but you can delay it any number of
picoseconds. For each picosecond you delay the packet before beginning your trip,
all security scanners move one step. You're not in the firewall during this time;
you don't enter layer 0 until you stop delaying the packet.


## Solution
in [Haskell](./Day13.hs)
