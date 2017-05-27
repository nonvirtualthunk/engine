package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:43 AM
 */
public class GraphEdge {
    public VornoiEdge voronoiEdge;
    public float x1, y1, x2, y2;
    public int p0index;
    public int p1index;

    public GraphEdge(VornoiEdge e) {
        this.voronoiEdge = e;
    }
}