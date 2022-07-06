/**
 * World interface that represents the entire play area.
 */
public interface World {
    public boolean contains(Bot bot);

    public int getHeight();

    public int getWidth();
}
