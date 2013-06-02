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

import java.util.ArrayList;
import java.util.List;
import com.ericsson.otp.erlang.*;


public class RatingResult {
    //~ Instance fields --------------------------------------------------------

    private ArrayList<RatingStep> steps = new ArrayList<RatingStep>();
    private String ratingGroup = "UNKNOWN";

    //~ Methods ----------------------------------------------------------------

    public void addStep(RatingStep step) {
        steps.add(step);
    }

    public ArrayList<RatingStep> getResults() {
        return steps;
    }

    public void setRatingGroup(String group) {
        ratingGroup = group;
    }

    public String getRatingGroup() {
        return ratingGroup;
    }

    public boolean is_rated() {
        return steps.size() > 0;
    }

    @Override
    public String toString() {
        StringBuilder buf = new StringBuilder();

        for (RatingStep step : steps) {
            if (buf.length() > 0) {
                buf.append(",");
            }

            buf.append(step.toString());
        }

        buf.append(",ratingGroup=");
        buf.append(ratingGroup);
        buf.append("]");
        buf.insert(0, "RatingResult=[");

        return buf.toString();
    }

    public OtpErlangObject toErlang() {
        ArrayList<OtpErlangObject> cascade = new ArrayList<OtpErlangObject>();
        for (RatingStep step : steps) {
            cascade.add(step.toErlang());
        }
        OtpErlangObject[] tuple = {ErlangTransformer.fromJava("rating_result"),
                                  ErlangTransformer.fromJava((List) cascade),
                                  ErlangTransformer.fromJava(this.ratingGroup.getBytes())};
        return new OtpErlangTuple(tuple);
    }
}
