package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:15 AM
 */
public class cEdge {

    cFace adjface[];

    cVertex endpts[];

    cFace newface;

    boolean delete;

    cEdge next, prev;

    cEdge() {
        adjface = new cFace[2];
        adjface[0] = adjface[1] = null;
        endpts = new cVertex[2];
        endpts[0] = endpts[1] = null;
        newface = null;
        delete = false;
        next = prev = null;
    }
}
