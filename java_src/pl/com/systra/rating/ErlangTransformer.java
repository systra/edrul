package pl.com.systra.rating;

import java.util.ArrayList;
import java.util.Map;
import java.util.TreeMap;
import java.util.List;
import java.util.HashMap;

import com.ericsson.otp.erlang.*;

/**
 * Transform from OtpErlangObject to Java object and vice versa.
 *
 * Modified version of ingo implementation (https://github.com/nerlo/nerlo.git)
 * Most likely better solution: https://svn.apache.org/repos/asf/tuscany/sandbox/wjaniszewski/binding-erlang-runtime/src/main/java/org/apache/tuscany/sca/binding/erlang/impl/types/
 *
 */
public class ErlangTransformer {
    public static OtpErlangObject fromJava(Object o) throws IllegalArgumentException {

        if (o instanceof String) {
            return new OtpErlangAtom((String) o);
        } else if (o instanceof Boolean) {
            return (o == Boolean.TRUE) ? new OtpErlangAtom("true") : new OtpErlangAtom("false");
        } else if (o instanceof byte[]) {
            return new OtpErlangBinary((byte[]) o);
        } else if (o instanceof Integer) {
            return new OtpErlangInt(((Integer) o).intValue());
        } else if (o instanceof Long) {
            return new OtpErlangInt(((Long) o).intValue());
        } else if (o instanceof Double) {
            return new OtpErlangDouble(((Double) o).doubleValue());
        } else if (o instanceof List) {
            Object[] list = ((List) o).toArray();
            OtpErlangObject[] erlObjects = new OtpErlangObject[list.length];
            for (int i = 0; i < list.length; i++) {
                erlObjects[i] = (OtpErlangObject) list[i];
            }
            return new OtpErlangList(erlObjects);
        }

        throw new IllegalArgumentException("cannot transform input class: " + o.getClass().getName());
    }

    public static Object toJava(OtpErlangObject o) throws IllegalArgumentException {
        if (o instanceof OtpErlangAtom) {
            String value = ((OtpErlangAtom) o).atomValue();
            if (value.equals("true") || value.equals("false")) {
                return new Boolean(value);
            } else {
                return value;
            }
        } else if (o instanceof OtpErlangBinary) {
            return new String(((OtpErlangBinary) o).binaryValue());
        }  else if (o instanceof OtpErlangInt) {
            return ((OtpErlangInt) o).longValue();
        } else if (o instanceof OtpErlangLong) {
            return ((OtpErlangLong) o).longValue();
        } else if (o instanceof OtpErlangDouble) {
            return ((OtpErlangDouble) o).doubleValue();
        } else if (o instanceof OtpErlangString) {
            return ((OtpErlangString) o).stringValue();
        } else if (o instanceof OtpErlangList) {
            Map<String, Object> map = new HashMap<String, Object>(((OtpErlangList) o).arity());
            for (OtpErlangObject x : ((OtpErlangList) o).elements()) {
                if (false == x instanceof OtpErlangTuple)
                    throw new IllegalArgumentException("Only tuple lists are supported");
                OtpErlangTuple t = (OtpErlangTuple) x;
                if (t.arity() != 2) {
                    throw new IllegalArgumentException("Only lists containing tuples {string() | binary(), term()} are supported");
                }
                OtpErlangObject e1 = t.elementAt(0);
                String key;
                if (e1 instanceof OtpErlangString) {
                    key = ((OtpErlangString) e1).stringValue();
                } else if (e1 instanceof OtpErlangBinary) {
                    key = new String(((OtpErlangBinary) e1).binaryValue());
                } else {
                    throw new IllegalArgumentException("Only lists containing tuples {string() | binary(), term()} are supported");
                }
                map.put(key, toJava(t.elementAt(1)));
            }
            return map;
        }
        throw new IllegalArgumentException("Cannot transform input class: " + o.getClass().getName());
    }
}
