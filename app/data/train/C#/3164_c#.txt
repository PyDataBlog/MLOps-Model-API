using FingerMovingSimulation.Core.Hands.Colemak.Fingers.Right;
using MiscUtil;

namespace FingerMovingSimulation.Infrastructure.Hands.Colemak.Fingers.Right.IndexFinger
{
    internal class LKeyState : IRightIndexFingerKeyState
    {
        private readonly RightIndexFinger rightIndexFinger;

        public LKeyState(RightIndexFinger rightIndexFinger)
        {
            this.rightIndexFinger = rightIndexFinger;
        }

        public string CurrentKey
        {
            get { return "l"; }
        }

        public double PressD7()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.D7KeyState;
            return 1;
        }

        public double PressL()
        {
            return 0;
        }

        public double PressN()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.NKeyState;
            return 1;
        }

        public double PressM()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.MKeyState;
            return 2;
        }

        public double PressD6()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.D6KeyState;
            return 3.Sqrt();
        }

        public double PressJ()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.JKeyState;
            return 1;
        }

        public double PressH()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.HKeyState;
            return 1;
        }

        public double PressK()
        {
            rightIndexFinger.CurrentKeyState = rightIndexFinger.KKeyState;
            return 3.Sqrt();
        }
    }
}