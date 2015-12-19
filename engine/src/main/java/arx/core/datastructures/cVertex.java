package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:14 AM
 */
public class cVertex {

    cVertex prev, next;
    cPointi v;
    boolean ear = false;
    int vnum;
    cEdge duplicate;
    boolean  onhull;
    boolean  mark;

    public cVertex() {
        prev = next = null;
        v = new cPointi();
        vnum = 0;
        duplicate = null;
        onhull = false;
        mark = false;
    }

    public cVertex(float i, float j) {
        v = new cPointi();
        v.x = i;
        v.y = j;
        v.z = i * i + j*  j;
        prev = next = null;
    }

    public cVertex(float x, float y, float z) {
        v = new cPointi();
        v.x = x;
        v.y = y;
        v.z = z;
        prev = next = null;
    }

    public void ResetVertex3D()
    {
        v.z = v.x * v.x + v.y * v.y;
    }

}









