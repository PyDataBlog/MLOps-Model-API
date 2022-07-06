package visitors;

/**
 * Created by stratosphr on 20/11/15.
 */
public interface IVisitedSort extends IVisited {

    Object accept(ISortVisitor visitor);

}
