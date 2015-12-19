package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:43 AM
 */
public class GraphEdge {
    public VornoiEdge e;
    public float x1, y1, x2, y2;

    GraphEdge next;

    public GraphEdge(VornoiEdge e) {
        this.e = e;
    }
}