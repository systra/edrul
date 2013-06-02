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

import com.ericsson.otp.erlang.*;

public class RatingStep {
    //~ Static fields/initializers ---------------------------------------------

    //~ Instance fields --------------------------------------------------------
    private String balanceType = ""; // The balance type
    private int pri = 0;
    private long cost = -1;

    //~ Constructors -----------------------------------------------------------
    public RatingStep() {
        //
    }

    public RatingStep(int pri, long cost) {
        this(pri, cost, "General Cash");
    }

    public RatingStep(int pri, long cost, String balanceType) {
        this.pri = pri;
        this.cost = cost;
        this.balanceType = balanceType;
    }

    //~ Methods ----------------------------------------------------------------
    /**
     * @return the cost
     */
    public long getCost() {
        return cost;
    }

    /**
     * @param cost the cost to set
     */
    public void setCost(long cost) {
        this.cost = cost;
    }

    /**
     * @return the balanceType
     */
    public String getBalanceType() {
        return balanceType;
    }

    /**
     * @param balanceType the balanceType to set
     */
    public void setBalanceType(String balanceType) {
        this.balanceType = balanceType;
    }

    @Override
    public String toString() {
        return String.format("RatingStep=[%d,%d,%s]", pri, cost, balanceType);
    }

    public OtpErlangObject toErlang() {
        OtpErlangObject[] tuple = {ErlangTransformer.fromJava("rating_step"),
            ErlangTransformer.fromJava(this.pri),
            ErlangTransformer.fromJava(this.balanceType.getBytes()),
            ErlangTransformer.fromJava(this.cost)};
        return new OtpErlangTuple(tuple);
    }
}
