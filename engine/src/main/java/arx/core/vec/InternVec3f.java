package arx.core.vec;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec3f implements Externalizable {
	protected float xi;
	protected float yi;
	protected float zi;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec3f(){}
	public InternVec3f(float xa, float ya, float za) {
		xi = xa;
		yi = ya;
		zi = za;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeFloat(xi);

		out.writeFloat(yi);

		out.writeFloat(zi);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		xi = in.readFloat();
		yi = in.readFloat();
		zi = in.readFloat();
	}

}
