package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:15 AM
 */
public class cPointi {
    float x;

    float y;

    float z;

    cPointi() {
        x = y = z = 0;
    }

    cPointi(float x, float y) {
        this.x = x;
        this.y = y;
        this.z = 0;
    }

    cPointi(float x, float y, float z) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    public float Area2(cPointi a, cPointi b, cPointi c) {
        float area = ((c.x - b.x) * (a.y - b.y)) - ((a.x - b.x) * (c.y - b.y));
        return area;
    }

    public float AreaSign(cPointi a, cPointi b, cPointi c) {
        double area2;

        area2 = (b.x - a.x) * (double) (c.y - a.y) - (c.x - a.x)
                * (double) (b.y - a.y);

        if (area2 > 0.5)
            return 1;
        else if (area2 < -0.5)
            return -1;
        else
            return 0;
    }

    public boolean Left(cPointi a, cPointi b, cPointi c) {
        return AreaSign(a, b, c) > 0;
    }

    public boolean LeftOn(cPointi a, cPointi b, cPointi c) {
        return AreaSign(a, b, c) >= 0;
    }

    /*
     * returns the distance of two points
     */
    public double Dist(cPointi p, cPointi p1) {
        double l = Math.sqrt(Math.pow((p.x - p1.x), 2)
                + Math.pow((p.y - p1.y), 2));
        return l;
    }

    public boolean Collinear(cPointi a, cPointi b, cPointi c) {
        return AreaSign(a, b, c) == 0;
    }

    public boolean Between(cPointi a, cPointi b, cPointi c) {
        if (!Collinear(a, b, c))
            return false;

        if (a.x != b.x)
            return ((a.x <= c.x) && (c.x <= b.x))
                    || ((a.x >= c.x) && (c.x >= b.x));
        else
            return ((a.y <= c.y) && (c.y <= b.y))
                    || ((a.y >= c.y) && (c.y >= b.y));
    }

    /*
     * Returns the distance of the input point from its perp. proj. to the e1
     * edge. Uses method detailed in comp.graphics.algorithms FAQ
     */
    double DistEdgePoint(cPointi a, cPointi b, cPointi c) {
        double r, s;
        double length;
        double dproj = 0.0;
        length = Math.sqrt(Math.pow((b.x - a.x), 2) + Math.pow((b.y - a.y), 2));

        if (length == 0.0) {
            System.out.println("DistEdgePoint: Length = 0");
        }
        r = (((a.y - c.y) * (a.y - b.y)) - ((a.x - c.x) * (b.x - a.x)))
                / (length * length);
        s = (((a.y - c.y) * (b.x - a.x)) - ((a.x - c.x) * (b.y - a.y)))
                / (length * length);

        dproj = Math.abs(s * length);

        if ((s != 0.0) && ((0.0 <= r) && (r <= 1.0)))
            return dproj;
        if ((s == 0.0) && Between(a, b, c))
            return 0.0;
        else {
            double ca = Dist(a, c);
            double cb = Dist(b, c);
            return Math.min(ca, cb);
        }
    }
}
