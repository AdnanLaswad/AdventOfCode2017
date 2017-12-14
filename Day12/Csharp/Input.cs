using System;
using System.Linq;

namespace Day12
{
    class Input
    {
        public int FromNodeId;
        public int[] ConnectedIds;

        public static Input ParseLine(string line)
        {
            var lineParts = line.Split(new[] { " ", ",", "<->" }, StringSplitOptions.RemoveEmptyEntries);

            return new Input
            {
                FromNodeId = Convert.ToInt32(lineParts[0]),
                ConnectedIds = lineParts.Skip(1).Select(t => Convert.ToInt32(t)).ToArray()
            };
        }

        public void AddToGraph(IntGraph graph)
        {
            foreach (var id in ConnectedIds)
                graph.AddConnection(FromNodeId, id);
        }
    }
}