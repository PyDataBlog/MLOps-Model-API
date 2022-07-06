package com.kashukov.convert;

/**
 * Convert short to any primitive data type
 */
public class Short {
    /**
     * Convert short to boolean
     *
     * @param input short
     * @return boolean
     */
    public static boolean shortToBoolean(short input) {
        return input != 0;
    }

    /**
     * Convert short to byte
     *
     * @param input short
     * @return byte
     */
    public static byte shortToByte(short input) {
        return (byte) input;
    }

    /**
     * Convert short to byte[]
     *
     * @param input short
     * @return byte[]
     */
    public static byte[] shortToByteArray(short input) {
        return new byte[]{
                (byte) (input >>> 8),
                (byte) input};
    }

    /**
     * Convert short to char
     *
     * @param input short
     * @return char
     */
    public static char shortToChar(short input) {
        return (char) input;
    }

    /**
     * Convert short to double
     *
     * @param input short
     * @return double
     */
    public static double shortToDouble(short input) {
        return (double) input;
    }

    /**
     * Convert short to float
     *
     * @param input short
     * @return float
     */
    public static float shortToFloat(short input) {
        return (float) input;
    }

    /**
     * Convert short to int
     *
     * @param input short
     * @return int
     */
    public static int shortToInt(short input) {
        return (int) input;
    }

    /**
     * Convert short to long
     *
     * @param input short
     * @return long
     */
    public static long shortToLong(short input) {
        return (long) input;
    }

    /**
     * Convert short to String
     *
     * @param input short
     * @return String
     */
    public static java.lang.String shortToString(short input) {
        return java.lang.Short.toString(input);
    }
}