package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 7:23 AM
 */
class VornoiEdge {
    public float a = 0, b = 0, c = 0;

    public Site[] ep = new Site[2];
    public Site[] reg = new Site[2];

    public int edgenbr;
}
