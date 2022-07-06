using System;

class Program
{
    static void Main()
    {       
        int b = int.Parse(Console.ReadLine()); //wywevdase 0 ili 1
        uint n = uint.Parse(Console.ReadLine());
        uint currentNumberBitCheck;
        int bitCounterFor1 = 0;
        int bitCounterFor0 = 0;       
        string[] numbersToCheck = new string[n]; // inicializirane na masiw w string fomrat
        for (int i = 0; i < numbersToCheck.Length; i++)
        {   
            numbersToCheck[i] = Console.ReadLine();//wywejdane na masiwa
        }    
        for (int i = 0; i < numbersToCheck.Length; i++)// obhojdane na stringa [] el. po element
        {  
            currentNumberBitCheck = uint.Parse(numbersToCheck[i]); //priswoqwame pyrwiq element
            string bits = Convert.ToString((uint)currentNumberBitCheck, 2);// pyrviq element go convertirame w string s dwoi4en kod
            
            bitCounterFor0 = 0; // zanulqwame za wsqko otdelno 4islo w protiwen slu4ei 
            bitCounterFor1 = 0; // 4e sybere otgovora za wsi4ki 4isla
            if (b == 1)                         // nehsto kato ~(mask |1) ako tyrsime 0
            {                                      //zashtoto ima osobenost tyrsqt se samo bitovete koito sa izpolzwani 20 ->10100 = 3 
                bits = (bits.PadLeft(32, '0')); //a ne 29 ako raglevdame 32bitowiq uint 000 000 000 000 ... 10100 
            }                      
            if (b == 0)
            {
                bits = (bits.PadLeft(32, '1'));// markirat se neizpolzwanite bitowe = na 0 po pirncip kato sega sa kato 1111 
            }      //Console.WriteLine(bits); // izvejdame bitowete na konzolata za wizualna proverka
            for (int j = 0; j < bits.Length; j++)// obhojdame stringa sydyrjasht 010101
            {  
                if (bits[j] == '1')//tuk tyrsime bit 1 kato char zashtoto obhojdame string
                {
                    bitCounterFor1++;
                }                
                if (bits[j] == '0')//tuk tyrsime bit 0 kato char zashtoto obhojdame string
                {                   // neizpolzwanite bitowe sa markirani kato 11111111111111111111 ina4e shte prebroi 30 nuli
                    bitCounterFor0++;
                }
            }
            if (b == 1)
            {
                Console.WriteLine(bitCounterFor1);
            }
            if (b == 0)
            {
                Console.WriteLine(bitCounterFor0);
            }
        }
    }
}

//using System;

//class Program
//{
//    static void Main()
//    {
//        //wywevdase 0 ili 1
//        int b = int.Parse(Console.ReadLine());
//        long n = int.Parse(Console.ReadLine());
//        long currentNumberBitCheck;
//        int bitCounterFor1=0;
//        int bitCounterFor0 = 0;

//        // inicializirane na masiw w string fomrat

//        string[] numbersToCheck = new string[n];

//        for (int i = 0; i < numbersToCheck.Length ; i++)
//        {   //wywejdane na masiwa

//            numbersToCheck [i] = Console.ReadLine();
//        }
//        // obhojdane na stringa [] el. po element

//        for (int i = 0; i < numbersToCheck.Length; i++)
//        {
//            //priswoqwame pyrwiq element

//            currentNumberBitCheck =  long.Parse(numbersToCheck[i]);

//            // pyrviq element go convertirame w string s dwoi4en kod

//            string bits = Convert.ToString((long)currentNumberBitCheck, 2);

//            // izwevdame bitowete

//            bits += (bits.PadLeft(32,'0'));
//            for (int j = 0; j < 64/*bits.Length*/; j++)
//            {
//                //tuk tyrsime bit 1 kato char zashtoto obhojdame string
//                if (bits[j]=='1')
//                {
//                     bitCounterFor1++;
//                }
//                //tuk tyrsime bit 1 kato char zashtoto obhojdame string

//                if (bits[j]=='0')
//                {
//                    bitCounterFor0++;
//                }
//            }
//        }
//        if (b==1)
//        {
//            Console.WriteLine(bitCounterFor1);
//        }
//        if (b==0)
//        {
//            Console.WriteLine(bitCounterFor0);
//        }

//    }
//}