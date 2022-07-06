package ua.job4j.loop;

/**
 * Class Класс для вычисления факториала заданного числа.
 * @author vfrundin
 * @since 05.11.2017
 * @version 1.0
 */
public class Factorial {

    /**
     * Метод должен вычислять факториал поданного на вход числа.
     * @param n Число для которого нужно определить факториал.
     * @return result - найденный факториал числа n.
     */
    public int calc(int n) {
        int result = 1;

        if (n != 0) {
            for (int i = 1; i <= n; i++) {
                result *= i;
            }
        }

        return result;
    }
}
