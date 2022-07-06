using SoundDevices;

namespace SoundDeviceRemotes
{
    public class SpeakersRemote
    {
        public static ISoundDevice GetSoundDevice()
        {
            return new Speakers();
        }
    }
}
