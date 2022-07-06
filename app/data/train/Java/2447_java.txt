package ru.job4j.polymorphism;

/**
 * Created on 01.09.2017.
 *
 * @author Aleks Sidorenko (alek.sidorenko1979@gmail.com).
 * @version $Id$.
 * @since 0.1.
 */
public class StubInput implements Input {

    /**
     * @param answers - array's param.
     */
    private String[] answers;

    /**
     * @param position - param count position.
     */
    private int position = 0;

    /**
     * Constructor.
     * @param answers - array's param.
     */
    public StubInput(String[] answers) {
        this.answers = answers;
    }

    /**
     * Method from interface.
     * @param question - param of method interface.
     * @return - string.
     */
    public String ask(String question) {
        return answers[position++];
    }
}
