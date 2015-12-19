package org.cliffc.high_scale_lib;

import sun.misc.Unsafe;

/**
 * Created by IntelliJ IDEA.
 * User: nvt
 * Date: 10/9/12
 * Time: 4:23 PM
 * Created by nonvirtualthunk
 */
public class ArxUnsafeUtil {
    public static final Unsafe unsafe = UtilUnsafe.getUnsafe();
    public static Unsafe getUnsafe () { return UtilUnsafe.getUnsafe(); }


}
