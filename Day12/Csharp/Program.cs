using System;
using System.Linq;

namespace Day12
{
    class Program
    {
        static void Main()
        {
            var inputs = System.IO.File
                .ReadAllLines(@"..\input.txt")
                .Select(Input.ParseLine);

            var graph = new IntGraph();
            foreach (var inp in inputs)
                inp.AddToGraph(graph);

            var part1 = graph.Closure(0).Count;
            Console.WriteLine("Part 1: {0}", part1);

            var part2 = graph.Groups().Count();
            Console.WriteLine("Part 2: {0}", part2);

            Console.ReadLine();
        }
    }
}
