# cpu
A simple but interactive design of CPU I hand-wired with logic gates back in college. It was fun :) Sample programs included!

# to view samples in interactive editor
1. Download and install Digital Works (Demo version is enough), i.e. from following url
http://electronics-lab.com/downloads/schematic/002/index.html
   * unfortunately it is windows-only and license forbids me to redistribute. Should work on WINE as well.
2. Open DigitalWorks, type CTRL-O and navigate to CPU_final.dwm
3. You should see view like this 
![Control panel](/img/outside.PNG?raw=true)
4. Now to run simulation
   * double click RAM in the middle. Memory contents dialog should pop-up. CPU is already loaded with program that counts squares of natural numbers from 0 to 7, feel free to load other program from /program directory (*.map file, txt source is only for reference). Feel free to write own programs :)
   * from program menu select Circuit -> Run. You should notice clock in bottom left corner started to toggle its state
   * you can change frequency of clock from Circuit -> Clock speed menu
   * select "Object interaction" tool from pallete (hand with pointing finger icon)
   * left click "start" input in bottom-left corner
   * after a while (around 100 clock ticks) you should see numbers changing on display register
   * you can double click most of the parts to view what's inside, even when simulation is running!
5. Have fun!
6. Trouble? Send me an e-mail to ![my email](/img/e-mail.PNG?raw=true)

# to view schematics
1. Just go to img directory, it is not so fun though if not interactive...