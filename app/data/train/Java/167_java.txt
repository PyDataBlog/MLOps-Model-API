// THIS CODE IS MACHINE-GENERATED, DO NOT EDIT!

package fallk.jfunktion;

/**
 * Represents a predicate (boolean-valued function) of a {@code float}-valued and a generic argument.
 * This is the primitive type specialization of
 * {@link java.util.function.BiPredicate} for {@code float}/{@code char}.
 *
 * @see java.util.function.BiPredicate
 */
@FunctionalInterface
public interface FloatObjectPredicate<E> {

    /**
     * Evaluates this predicate on the given arguments.
     *
     * @param v1 the {@code float} argument
     * @param v2 the generic argument
     * @return {@code true} if the input arguments match the predicate,
     * otherwise {@code false}
     */
    boolean apply(float v1, E v2);
}
