package arx.core.vec;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec3i implements Externalizable {
	protected int xi;
	protected int yi;
	protected int zi;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec3i(){}
	public InternVec3i(int xa, int ya, int za) {
		xi = xa;
		yi = ya;
		zi = za;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeInt(xi);

		out.writeInt(yi);

		out.writeInt(zi);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		xi = in.readInt();
		yi = in.readInt();
		zi = in.readInt();
	}
}
