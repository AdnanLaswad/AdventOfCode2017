using System.Collections.Generic;
using System.Linq;

namespace Day12
{
    public class IntGraph
    {
        private readonly Dictionary<int, HashSet<int>> _connections;

        public IntGraph()
        {
            _connections = new Dictionary<int, HashSet<int>>();
        }


        public void AddConnection(int from, int to)
        {
            if (!_connections.TryGetValue(from, out HashSet<int> neighbours))
            {
                neighbours = new HashSet<int>();
                _connections.Add(from, neighbours);
            }
            neighbours.Add(to);
        }

        public IEnumerable<HashSet<int>> Groups()
        {
            var groups = new List<HashSet<int>>();
            var toVisit = GetAllNodeIds();

            while (toVisit.Any())
            {
                var node = toVisit.First();
                var group = Closure(node);
                groups.Add(group);

                toVisit.ExceptWith(group);
            }

            return groups;
        }

        public HashSet<int> GetAllNodeIds()
        {
            var keys = new HashSet<int>(_connections.Keys);
            var neighbours = _connections.Values.SelectMany(x => x);
            keys.UnionWith(neighbours);

            return keys;
        }

        public HashSet<int> Closure(int start)
        {
            return Closure(new HashSet<int>(new[] { start }));
        }

        private HashSet<int> Closure(ISet<int> toVisit)
        {
            var collected = new HashSet<int>();

            while (toVisit.Any())
            {
                var next = toVisit.First();
                toVisit.Remove(next);
                collected.Add(next);

                var neighbours = Neighbours(next);
                toVisit.UnionWith(neighbours.Except(collected));
            }
            return collected;
        }

        private IEnumerable<int> Neighbours(int id)
        {
            return _connections.TryGetValue(id, out HashSet<int> found) ? found : new HashSet<int>();
        }
    }
}