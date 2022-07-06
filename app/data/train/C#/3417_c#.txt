using System;


namespace e10
{
    class Liste<T>
    {
        private T[] liste = new T[5];
        private int sayi = 0;
        public void Ekle(T t)
        {
            if (sayi < 5)
                liste[sayi++] = t;
        }
        public void Cikar()
        {
            sayi--;
        }
        public void Yazdir()
        {
            for (int i = 0; i < sayi; i++)
                Console.Write($"{liste[i]}, ");
        }
    }

    class Program
    {
        static void Main(string[] args)
        {
            Liste<string> isimListesi = new Liste<string>();
            isimListesi.Ekle("Ali");
            isimListesi.Ekle("Ayse");
            isimListesi.Yazdir();
            Console.WriteLine();

            Liste<int> sayilar = new Liste<int>();
            sayilar.Ekle(1);
            sayilar.Ekle(5);
            // DERLEME hatasi. Derleyici alttaki satira izin vermez
            // sayilar.Ekle("Fatma");
            sayilar.Yazdir();
            Console.WriteLine();
        }
    }
}
