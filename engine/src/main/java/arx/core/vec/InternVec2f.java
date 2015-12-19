package arx.core.vec;
import java.io.Externalizable;
import java.io.IOException;
import java.io.ObjectInput;
import java.io.ObjectOutput;

public class InternVec2f implements Externalizable {
	protected float xi;
	protected float yi;
	private static final long serialVersionUID = 9223372036854770000L;
	public InternVec2f(){}
	public InternVec2f(float xa, float ya) {
		xi = xa;
		yi = ya;
	}
	@Override
	public void writeExternal(ObjectOutput out) throws IOException {
		out.writeFloat(xi);

		out.writeFloat(yi);

	}

	@Override
	public void readExternal(ObjectInput in) throws IOException, ClassNotFoundException {
		xi = in.readFloat();
		yi = in.readFloat();
	}
}
