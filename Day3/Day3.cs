using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2017.Day3
{
    class Program
    {
        static void Main()
        {
            var input = 312051;

            Console.WriteLine($"Part1 for {input}: {Part1(input)}");
            Console.WriteLine($"Part2 for {input}: {Part2(input)}");


        }

        static int Part1(int number)
        {
            if (number <= 1)
                return 0;

            var ring = RingNr(number);
            var sideLen = SideLength(ring - 1);
            var index = number - sideLen * sideLen;

            var offset = StepsToRingmid(SideLength(ring), index);
            return ring + offset;
        }

        static int Part2(int input)
        {
            var number = 2;
            int res;
            var field = InitField();

            do
            {
                var coord = Coord(number);
                res = SetValueAtCoord(field, coord);
                number++;
            } while (res < input);

            return res;
        }

        static Coord Coord(int number)
        {
            if (number <= 1)
                return new Coord(0,0);

            var ring = RingNr(number);
            var sideLen = SideLength(ring - 1);
            var index = number - sideLen * sideLen;

            var coords = RingCoords(ring).ToArray();
            return coords[index % coords.Length];
        }


        static Dictionary<Coord,int> InitField()
        {
            return new Dictionary<Coord, int> {[new Coord(0, 0)] = 1};
        }

        static int SetValueAtCoord(Dictionary<Coord, int> field, Coord coord)
        {
            var neighbours = new []
            {
                new Coord(coord.X+1, coord.Y+1),
                new Coord(coord.X+1, coord.Y+0),
                new Coord(coord.X+1, coord.Y-1),
                new Coord(coord.X+0, coord.Y-1),
                new Coord(coord.X-1, coord.Y-1),
                new Coord(coord.X-1, coord.Y+0),
                new Coord(coord.X-1, coord.Y+1),
                new Coord(coord.X+0, coord.Y+1)
            };
            var sum = neighbours.Select(c => GetValue(field, c)).Sum();
            field[coord] = sum;
            return sum;
        }

        static int GetValue(Dictionary<Coord, int> field, Coord coord)
        {
            return field.TryGetValue(coord, out int val) ? val : 0;
        }

        static int StepsToRingmid(int sideLen, int i)
        {
            var half = sideLen / 2;
            return (int)Math.Abs(Math.Floor((double)(i % (sideLen - 1) - half)));
        }

        static int RingNr(int number)
        {
            if (number <= 1)
                return 0;

            var radix = Math.Floor(0.5 * (Math.Sqrt(number) - 1));
            if ((int) ((2*radix+1) * (2*radix+1)) == number)
                return (int) radix;
            else
                return (int) radix + 1;
        }

        static int SideLength(int ring)
        {
            if (ring == 0)
                return 1;

            return ring * 2 + 1;
        }


        static IEnumerable<Coord> RingCoords(int ringNr)
        {
            var sideLen = SideLength(ringNr);
            var half = sideLen / 2;

            for (var y = half; y > -half; y--)
                yield return new Coord(half, y);

            for (var x = half; x > -half; x--)
                yield return new Coord(x, -half);

            for (var y = -half; y < half; y++)
                yield return new Coord(-half, y);

            for (var x = -half; x < half; x++)
                yield return new Coord(x, half);

        }

    }

    struct Coord
    {
        public Coord(int x, int y)
        {
            X = x;
            Y = y;
        }

        public int X;
        public int Y;
    }
}
