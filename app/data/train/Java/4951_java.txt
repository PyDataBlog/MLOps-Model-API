package demo.designpatterns.simplefactory;

/**
 * Created by Roger Xu on 2017/6/24.
 */
public class MobileFactory {
    public Mobile getMobile(String title) throws Exception {
        if (title.equalsIgnoreCase("nokia")) {
            return new Nokia();
        } else if (title.equalsIgnoreCase("motorola")) {
            return new Motorola();
        } else {
            throw new Exception("no such " + title + " mobile found");
        }
    }
}
