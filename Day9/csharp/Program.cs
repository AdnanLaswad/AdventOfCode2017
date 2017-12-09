using System;
using System.Collections.Generic;
using System.Linq;

namespace Day9
{
    class Program
    {
        static void Main(string[] args)
        {
            var content = System.IO.File.ReadAllText(@"../input.txt");

            var parser = new Parser(content);
            var group = parser.ParseGroup();

            Console.WriteLine("Part 1: {0}", group.Score(0));
            Console.WriteLine("Part 2: {0}", group.GargabeCharCount);
        }
    }

    class Group : Content
    {
        private readonly Content[] _content;

        public Group(Content[] content)
        {
            _content = content;
        }

        public IEnumerable<Content> Contents => _content;

        public override int GargabeCharCount => _content.Sum(c => c.GargabeCharCount);

        public override int Score(int outer)
        {
            var thisLayer = outer + 1;
            var innerSum = _content.Sum(c => c.Score(thisLayer));
            return thisLayer + innerSum;
        }
    }

    class Garbage : Content
    {
        private readonly string _garbage;

        public Garbage (string garbage)
        {
            _garbage = garbage;
        }

        public string Value => _garbage;
        public override int GargabeCharCount => _garbage.Length;
        public override int Score(int outer) 
        {
            return 0;
        }
    }

    abstract class Content
    {
        public abstract int GargabeCharCount { get; }
        public abstract int Score (int outer);
    }

    class Parser 
    {
        private ParserPos _pos;

        public Parser (string input)
        {
            _pos = new ParserPos(input);
        }

        public Group ParseGroup()
        {
            if (NextUnignoredChar() != '{')
                throw new Exception ("'{' expected");

            var contents = ParseContents();
            
            if (NextUnignoredChar() != '}')
                throw new Exception ("'}' expected");

            return new Group(contents);
        }

        Content[] ParseContents()
        {
            var list = new List<Content>();
            while (CurrentChar != '}')
            {
                list.Add(ParseContent());

                if (CurrentChar == '}')
                    break;

                if (NextUnignoredChar() != ',')
                    throw new Exception("',' or '}' expected");
            }

          return list.ToArray();
            
        } 

        Content ParseContent()
        {
            if (CurrentChar == '{')
                return ParseGroup();
            if (CurrentChar == '<')
                return ParseGarbage();

            return null;
        }

        Garbage ParseGarbage()
        {
            if (NextUnignoredChar() != '<')
                throw new Exception ("'<' expected");

            var garbage = new List<char>();
            
            while (true)
            {
                var next = NextUnignoredChar();
                if (next == null)
                    throw new Exception("encountered end of stream while parsing garbage");
                if (next == '>')
                    return new Garbage(new String(garbage.ToArray()));
                garbage.Add(next.Value);

            }
        }

        char? NextUnignoredChar()
        {
            var c = NextChar();
            if (c == '!')
            {
                NextChar();
                return NextUnignoredChar();
            }
            return c;
        }

        char? CurrentChar => _pos.Current;

        char? NextChar()
        {
            var c = _pos.Current;
            _pos = _pos.MoveRight();
            return c;
        }
    }

    class ParserPos
    {
        private readonly string _input;
        private readonly int _index;

        public ParserPos(string input, int index = 0)
        {
            _input = input;   
            _index = index;
            if (_index < 0)
                _index = 0;
            if (_index >= input.Length)
                _index =  input.Length;
        }

        public ParserPos MoveRight()
        {
            return new ParserPos(_input, _index + 1);
        }

        public ParserPos MoveLeft()
        {
            return new ParserPos(_input, _index - 1);
        }

        public bool EndOfStream => _index >= _input.Length;

        public char? Current => !EndOfStream ? _input[_index] : (char?)null;
    }
}
