package arx.core.datastructures;

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 3/18/15
 * Time: 6:14 AM
 */
public class cVertexList {
    int n; // 0 means empty; 1 means one vertex; etc.

    int j;

    cVertex head;

    public cVertexList() {
        head = null;
        n = 0;
    }

    public cVertex GetElement(int index) {

        cVertex v = new cVertex();
        if (index <= n) {
            v = head;
            for (int i = 0; i < index; i++)
                v = v.next;

        } else
            v = new cVertex(10000, 10000);

        return v;
    }

    public cVertex MakeNullVertex() {
        cVertex v = new cVertex();
        InsertBeforeHead(v);
        return v;
    }

    public void InitHead(cVertex h) {
        head = new cVertex();
        head = h;
        head.next = head.prev = head;
        n = 1;
    }

    public void ClearVertexList() {
        if (head != null)
            head = null;
        n = 0;
    }

    public void InsertBeforeHead(cVertex ver) {
        if (head == null)
            InitHead(ver);
        else {
            InsertBefore(ver, head);
        }
    }

    public void InsertBefore(cVertex newV, cVertex oldV) {
        if (head == null)
            InitHead(newV);
        else {
            oldV.prev.next = newV;
            newV.prev = oldV.prev;
            newV.next = oldV;
            oldV.prev = newV;
            n++;
        }
    }

    public void SetVertex(float x, float y) {
        cVertex v = new cVertex(x, y);
        InsertBeforeHead(v);
    }

    public void AddVertex(float x, float y) {
        cVertex v = new cVertex(x, y);
        // gets vertex of 1st vertex of the closest edge to the point
        cVertex vNear = GetEdge(x, y);
        if (vNear != null)
            InsertBefore(v, vNear.next);
    }

    public void ResetVertex(cVertex resV, float x, float y) {
        resV.v.x = x;
        resV.v.y = y;
    }

    public void ResetVertex(cVertex resV, float x, float y, int vnum, boolean mark) {
        resV.v.x = x;
        resV.v.y = y;
        resV.vnum = vnum;
        resV.mark = mark;
    }

    public void Delete(cVertex ver) {
        if (head == head.next)
            head = null;
        else if (ver == head)
            head = head.next;

        ver.prev.next = ver.next;
        ver.next.prev = ver.prev;
        n--;
    }

    public void ListPart(cVertexList list, int j) {
        int i = j;
        cVertex temp1 = head, temp2;
        do {
            i--;
            temp2 = new cVertex(); // Create a new vertex cell
            temp2.v = temp1.v; // Fill it with the same cPointi as in list
            temp2.mark = temp1.mark;
            temp2.ear = temp1.ear;
            temp2.duplicate = temp1.duplicate;
            temp2.onhull = temp1.onhull;
            temp2.vnum = temp1.vnum;
            list.InsertBeforeHead(temp2);
            temp1 = temp1.next;
        } while (i >= 0);
    }

    public void ListCopy(cVertexList list) {
        cVertex temp1 = head, temp2;
        do {
            temp2 = new cVertex(); // Create a new vertex cell
            temp2.v = temp1.v; // Fill it with the same cPointi as in list
            temp2.mark = temp1.mark;
            temp2.ear = temp1.ear;
            temp2.duplicate = temp1.duplicate;
            temp2.onhull = temp1.onhull;
            temp2.vnum = temp1.vnum;
            list.InsertBeforeHead(temp2);
            temp1 = temp1.next;
        } while (temp1 != head);
    }

    public cVertex GetNearVertex(float x, float y) {
        cVertex vnear = null, vtemp = head;
        double dist = -1.0, dx, dy, mindist = 0.0;

        if (vtemp == null)
            return vnear;

        do {
            dx = vtemp.v.x - x;
            dy = vtemp.v.y - y;
            dist = dx * dx + dy * dy;

            // Initialize on first pass (when vnear==null);
            // otherwise update if new winner
            if (vnear == null || dist < mindist) {
                mindist = dist;
                vnear = vtemp;
            }
            vtemp = vtemp.next;
        } while (vtemp != head);

        return vnear;
    }

    public cVertex FindVertex(float x, float y, float w, float h) {
        cVertex notfound = null;
        cVertex temp = head;

        if (n > 0) {
            do {
                temp = temp.next;
                if ((temp.v.x <= x + (w / 2)) && (temp.v.x >= x - (w / 2))
                        && (temp.v.y <= y + (h / 2))
                        && (temp.v.y >= y - (h / 2)))
                    return temp;
            } while (temp != head);
        }
        return notfound;
    }

    public cVertex GetEdge(float x, float y) {
        cVertex vnear = null, vtemp = head;
        double mindist = 0.0, dist = -1.0;
        cPointi p = new cPointi();

        // input query point
        p.x = x;
        p.y = y;

        if (vtemp == null)
            return vnear;

        do {
            dist = p.DistEdgePoint(vtemp.v, vtemp.next.v, p);
            if (vnear == null || dist < mindist) {
                mindist = dist;
                vnear = vtemp;
            }
            vtemp = vtemp.next;
        } while (vtemp != head);

        return vnear;
    }

}
