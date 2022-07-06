using System;
using System.Diagnostics;

namespace DesignPatterns.ObserverPattern
{
    public class HexaObserver : Observer
    {
        public HexaObserver(Subject subject)
        {
            Subject = subject;
            Subject.Attach(this);
        }

        //Override
        public override void Update()
        {
            Trace.WriteLine("Hex String: " + Subject.State.ToString("X1") + Environment.NewLine);
        }
    }
}
