using System;
using System.Collections.Generic;
using System.Linq;

namespace Day13
{
    class Program
    {
        static void Main(string[] args)
        {
            var input = System.IO.File.ReadAllLines(@"../input.txt").ToList();
            var firewall = new Firewall(input);

            var totalSeverity = firewall.TotalSeverity(0);
            Console.WriteLine($"Part1: {totalSeverity}");


            var delay = firewall.FindSaveDelay();
            Console.WriteLine($"Part2: {delay}");
        }
    }

    class Firewall
    {

        private readonly Dictionary<int, Scanner> _scanners;

        public int TotalSeverity(int delay)
        {
            return Enumerable
                .Range(0, MaxLayer())
                .Where(l => Hit(l, delay + l))
                .Sum(Severity);

        }

        public int FindSaveDelay()
        {
            for (var delay = 0; ; delay++)
            {
                if (_scanners.Keys
                    .All(layer => (layer + delay) % ScannerCycleLength(layer) != 0))
                    return delay;
            }
        }


        public Firewall(IEnumerable<string> liste)
        {
            _scanners = new Dictionary<int, Scanner>();

            foreach (var l in liste)
            {
                var parts = l.Split(new[] { ':', ' ' }, StringSplitOptions.RemoveEmptyEntries);

                var scanner = new Scanner { Layer = Convert.ToInt32(parts[0]), Range = Convert.ToInt32(parts[1]) };
                _scanners[scanner.Layer] = scanner;
            }
        }

        private int MaxLayer()
        {
            return _scanners.Keys.Max();
        }


        private int? ScannerCycleLength(int layer)
        {
            Scanner scannerAtLayer;
            if (!_scanners.TryGetValue(layer, out scannerAtLayer))
                return null;

            // (0 ... range-1, range-2 ... 1)*
            //  ^.. range   ^    ^  range-2^
            // => range + range - 2 = 2 * range - 2
            return 2 * scannerAtLayer.Range - 2;
        }


        private bool Hit(int layer, int step)
        {
            var cycleLength = ScannerCycleLength(layer);
            if (cycleLength == null)
                return true;

            return step % cycleLength == 0;
        }

        private int Severity(int layer)
        {
            Scanner scannerAtLayer;
            if (!_scanners.TryGetValue(layer, out scannerAtLayer))
                return 0;

            return scannerAtLayer.Range * layer;
        }


        class Scanner
        {
            public int Layer;
            public int Range;
        }

    }
}
