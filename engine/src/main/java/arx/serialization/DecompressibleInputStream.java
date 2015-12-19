package arx.serialization;

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 5/13/12
 * Time: 7:15 PM
 * Created by nonvirtualthunk
 */


import java.io.*;


public class DecompressibleInputStream extends ObjectInputStream {

    public DecompressibleInputStream(InputStream in) throws IOException {
        super(in);
    }

    @Override
    protected ObjectStreamClass readClassDescriptor() throws IOException, ClassNotFoundException {
        ObjectStreamClass resultClassDescriptor = super.readClassDescriptor(); // initially streams descriptor
        Class localClass; // the class in the local JVM that this descriptor represents.
        try {
//            try {
                localClass = Class.forName(resultClassDescriptor.getName());

//            } catch ( ClassNotFoundException e ) {
//                String fallback = ReflectionAssistant.classEndingIn(resultClassDescriptor.getName().substring(resultClassDescriptor.getName().lastIndexOf(".")));
//                if ( fallback != "" ) {
//                    localClass = Class.forName(fallback);
//                } else {
//                    throw new ClassNotFoundException("No Fallback found",e);
//                }
//            }
        } catch (ClassNotFoundException e) {
            System.err.println("No local class for " + resultClassDescriptor.getName());
            e.printStackTrace();
            return resultClassDescriptor;
        } catch ( Exception e ) {
            System.out.println("Exception during class lookup on object read ");
            e.printStackTrace();
            return resultClassDescriptor;
        }
        ObjectStreamClass localClassDescriptor = ObjectStreamClass.lookup(localClass);
        if (localClassDescriptor != null) { // only if class implements serializable
            final long localSUID = localClassDescriptor.getSerialVersionUID();
            final long streamSUID = resultClassDescriptor.getSerialVersionUID();
            if (streamSUID != localSUID) { // check for serialVersionUID mismatch.
                final StringBuffer s = new StringBuffer("Overriding serialized class version mismatch: ");
                s.append("local serialVersionUID = ").append(localSUID);
                s.append(" stream serialVersionUID = ").append(streamSUID);
                Exception e = new InvalidClassException(s.toString());
                System.err.println("Potentially Fatal Deserialization Operation in class : " + localClassDescriptor.getName() + "| Exception : " + e);
                resultClassDescriptor = localClassDescriptor; // Use local class descriptor for deserialization
            }
        }
        return resultClassDescriptor;
    }

    @Override
    protected Object readObjectOverride() throws IOException, ClassNotFoundException {
        return super.readObjectOverride();
    }
}