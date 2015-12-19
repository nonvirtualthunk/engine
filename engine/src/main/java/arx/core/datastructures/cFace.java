package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:15 AM
 */
public class cFace {

    cEdge edge[];

    cVertex vertex[];

    boolean visible;
    boolean lower;

    cFace next, prev;

    cFace() {
        edge = new cEdge[3];
        edge[0] = edge[1] = edge[2] = null;
        vertex = new cVertex[3];
        vertex[0] = vertex[1] = vertex[2] = null;
        visible =false;
        lower = true;
        next = prev = null;
    }
}
