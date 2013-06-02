/*
 * COPYRIGHT
 * Copyright (c) 2013, Andrzej Trawinski <at@systra.com.pl>
 *
 * LICENSE
 * This source file is subject to the new BSD license bundled with this package
 * in the file, LICENSE. This license is also available through the web at:
 * {@link http://www.opensource.org/licenses/bsd-license.php}. If you did not
 * receive a copy of the license, and are unable to obtain it through the web,
 * please send an email to at@systra.com.pl, and I will send you a copy.
 */

package pl.com.systra.rating;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;


public class ChargingParameter {

    //~ Instance fields --------------------------------------------------------

    private Boolean boolValue;
    private Date dateValue;
    private Integer intValue;
    private Long longValue;
    private String name;
    private String strValue;
    private Type type = Type.STRING;

    //~ Constructors -----------------------------------------------------------

    public ChargingParameter(String name) {
        this.name = name;
    }

    public ChargingParameter(ChargingParameter copy) throws Exception {
        this(copy.getName());
        setValue(copy.getValue());
    }

    public ChargingParameter(String name, Object obj) throws Exception {
        this(name);
        setValue(obj);
    }

    //~ Methods ----------------------------------------------------------------

    public Calendar getCalendarValue() {
        Calendar cal = Calendar.getInstance();
        cal.setTime(dateValue);

        return cal;
    }

    public String getName() {
        return name;
    }

    public Object getValue() {
        if (type == Type.STRING) {
            return strValue;
        } else if (type == Type.LONG) {
            return longValue;
        } else if (type == Type.DATE) {
            return dateValue;
        } else if (type == Type.BOOLEAN) {
            return boolValue;
        } else if (type == Type.INTEGER) {
            return intValue;
        } else {
            return null;
        }
    }

    public void setValue(Object value) throws Exception {
        if (value instanceof String) {
            this.strValue = (String) value;
            this.type = Type.STRING;
        } else if (value instanceof Integer) {
            this.intValue = (Integer) value;
            this.type = Type.LONG;
        } else if (value instanceof Long) {
            this.longValue = (Long) value;
            this.type = Type.LONG;
        } else if (value instanceof Date) {
            this.dateValue = (Date) value;
            this.type = Type.DATE;
        } else if (value instanceof Boolean) {
            this.boolValue = (Boolean) value;
            this.type = Type.BOOLEAN;
        } else {
            throw new Exception("Unsupported value type");
        }
    }

    public void setValue(int value) throws Exception {
        setValue(new Integer(value));
    }

    public void setValue(long value) throws Exception {
        setValue(new Long(value));
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        if (type == Type.DATE) {
            sb.append(name).append(":").append(dateValue == null ? "" : new SimpleDateFormat("yyyy-MM-dd hh:mm:ss").format(dateValue));
        } else {
            sb.append(name).append(":").append(getValue());
        }
        return sb.toString();
    }

    //~ Enumerations -----------------------------------------------------------

    public enum Type {
        BOOLEAN, DATE, INTEGER, LONG, STRING;
    }
}
