using System;
using System.Collections.Generic;


namespace CSharp
{
    enum State
    {
        Clean,
        Weakened,
        Infected,
        Flagged
    }
    class Program
    {
        static Dictionary<(int,int),State> _grid = new Dictionary<(int, int), State>();
        static (int,int) _position = (0,0);
        static (int,int) _direction = (0,-1);
        static int _infected = 0;

        static void Main(string[] args)
        {
            InitGrid();
            for (var i = 0; i < 10000000; i++)
                Burst();
            
            System.Console.WriteLine($"carrier infected {_infected} nodes");
        }

        static void Burst()
        {
            var currentState = GetCurrentState();
            ChooseDirection(currentState);
            ModifyCurrentNode(currentState);
            MoveForward();
        }

        private static void ChooseDirection(State current)
        {
            switch (current)
            {
                case State.Clean:
                    TurnLeft();
                    break;
                case State.Weakened:
                    break;
                case State.Infected:
                    TurnRight();
                    break;
                case State.Flagged:
                    TurnBack();
                    break;
            }
        }

        private static void ModifyCurrentNode(State current)
        {
            switch (current)
            {
                case State.Clean:
                    _grid[_position] = State.Weakened;
                    break;
                case State.Weakened:
                    _grid[_position] = State.Infected;
                    _infected++;
                    break;
                case State.Infected:
                    _grid[_position] = State.Flagged;
                    break;
                case State.Flagged:
                    _grid[_position] = State.Clean;
                    break;
            }
        }

        private static void MoveForward()
        {
            var (x,y) = _position;
            var (dX,dY) = _direction;
            _position = (x+dX,y+dY);
        }

        private static void TurnLeft()
        {
            var (dX,dY) = _direction;
            _direction = (dY, -dX);
        }

        private static void TurnRight()
        {
            var (dX,dY) = _direction;
            _direction = (-dY, dX);
        }

        private static void TurnBack()
        {
            var (dX,dY) = _direction;
            _direction = (-dX, -dY);
        }

        static State GetCurrentState()
        {
            State res = State.Clean;
            if (!_grid.TryGetValue(_position, out res))
                return State.Clean;
            return res;
        }

        static void InitGrid()
        {
            _grid.Clear();

            var inputLines = System.IO.File.ReadAllLines(@"../input.txt");
            var width = inputLines[0].Length;
            var height = inputLines.Length;
            var dX = width / 2;
            var dY = height / 2;            

            for (var y = 0; y < inputLines.Length; y++)
            {
                var line = inputLines[y];
                for (var x = 0; x < line.Length; x++)
                    _grid[(x-dX,y-dY)] = line[x] == '#' ? State.Infected : State.Clean;
            }
        }
    }
}
