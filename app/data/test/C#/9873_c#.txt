using Patterns.AbstractFactory.Factories;
using Patterns.AbstractFactory.Interfaces;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Patterns.AbstractFactory.TestExamples
{
    class AbstractFactoryTest : IPatternTest
    {
        IMusicalInstrumentFactory _currentFactory;

        const int GUITAR_STRINGS_NUM = 6;

        string GetOperationResult(Func<string> operation)
        {
            try
            {
                return operation();
            }
            catch (NotSupportedException ex)
            {
                return ex.Message;
            }
        }

        public void Run()
        {
            var factories = new IMusicalInstrumentFactory[] { new GibsonFactory(), new KorgFactory(), new YamahaFactory() };
            foreach (var factory in factories)
            {
                _currentFactory = factory;

                Console.WriteLine(GetOperationResult(() => _currentFactory.CreateGuitar(GUITAR_STRINGS_NUM).Name));
                Console.WriteLine(GetOperationResult(() => _currentFactory.CreateSynthesizer().Name));
                Console.WriteLine(GetOperationResult(() => _currentFactory.CreateViolin().Name));

                Console.WriteLine();
            }

            //OUTPUT:
            //
            //Fucking Gibson Mega Guitar
            //Oberheim Matrix-1000
            //Gibson Violin

            //Korg doesn't produce guitars
            //Korg PS60
            //Korg doesn't produce violins

            //Yamaha F310
            //Yamaha DX-7
            //‎Yamaha V3SKA

            Console.ReadKey();
        }
    }
}
