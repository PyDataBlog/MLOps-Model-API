/*
 * Copyright (c) 2014. Licensed under GPL-2. See license.txt in project folder.
 */

package de._0x29.xkpasswd;

import java.io.IOException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created by christian on 1/26/14.
 */
public class PasswordFactory {
    private int mAmount = 4;
    private int mMaxWordLength = 8;
    private int mMinWordLength = 5;
    private String mDelimiter = " ";
    private int mDigitsBefore = 0;
    private int mDigitsAfter = 0;

    public PasswordFactory setNumberOfWords(int amount) {
        mAmount = amount;

        return this;
    }

    public PasswordFactory setMinimumWordLength(int min) {
        mMinWordLength = min;

        return this;
    }

    public PasswordFactory setMaximumWordLength(int max) {
        mMaxWordLength = max;

        return this;
    }

    public PasswordFactory setWordDelimiter(String delimiter) {
        mDelimiter = delimiter;

        return this;
    }

    public PasswordFactory setDigitsBeforePassphrase(int digitsBefore) {
        mDigitsBefore = digitsBefore;

        return this;
    }

    public PasswordFactory setDigitsAfterPassphrase(int digitsAfter) {
        mDigitsAfter = digitsAfter;

        return this;
    }

    public String create() {
        ArrayList<String> availableWords = new ArrayList<String>();

        SecureRandom secureRandom = new SecureRandom();
        StringBuffer buffer = new StringBuffer();

        try {
            Dictionary dictionary = new Dictionary();
            HashMap<Integer, ArrayList<String>> words = dictionary.getWords();
            for (Integer wordLength : words.keySet()) {
                if (wordLength <= mMaxWordLength && wordLength >= mMinWordLength) {
                    availableWords.addAll(words.get(wordLength));
                }
            }

            for (int i = 0; i < mAmount; i++) {
                int randomIndex = secureRandom.nextInt(availableWords.size());

                String s = availableWords.get(randomIndex);
                buffer.append(s);

                availableWords.remove(randomIndex);
                if ((i + 1) < mAmount) {
                    buffer.append(mDelimiter);
                }
            }


        } catch (IOException e) {
            e.printStackTrace();
        }


        return buffer.toString();
    }
}
