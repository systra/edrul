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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Task implements Runnable {

    private static final Logger log = LoggerFactory.getLogger(Task.class);
    private Node node;
    private RatingEngine engine;
    private String action;
    private OtpErlangPid from;
    private OtpErlangRef ref;
    private Object params;

    public Task(OtpErlangObject msg, Node node, RatingEngine engine) {
        this.node = node;
        this.engine = engine;
        if (msg instanceof OtpErlangTuple) {
            OtpErlangTuple t = (OtpErlangTuple) msg;
            this.action = ((OtpErlangAtom) t.elementAt(0)).atomValue();
            this.from = (OtpErlangPid) t.elementAt(1);
            this.ref = (OtpErlangRef) t.elementAt(2);
            this.params = ErlangTransformer.toJava(t.elementAt(3));
        } else {
            throw new IllegalArgumentException("Tuple expected");
        }
    }

    public String getAction() {
        return this.action;
    }

    public OtpErlangPid getFrom() {
        return this.from;
    }

    @Override
    public void run() {
        try {
            if (action.equals("rate")) {
                doRate();
            } else if (action.equals("reload")) {
                doReload();
            } else if (action.equals("compile")) {
                doCompile();
            } else {
                log.warn("unknown action: {}", action);
                node.replyWithError(from, "unknown_action"); //throw UnsupportedOperationException;
            }
        } catch (Exception e) {
            log.error("unhandled exception: " + e.toString());
            node.replyWithError(from, "general_error");
        }
    }

    private void doRate() {
        try {
            if (false == params instanceof HashMap) {
                throw new Exception("Invalid parameter value: " + params.getClass().getName());
            }
            // remap charging parameters
            @SuppressWarnings("unchecked")
            HashMap<String, Object> map = ((HashMap<String, Object>) params);
            log.debug("Rating params: {}", map.toString());
            List<ChargingParameter> list = new ArrayList<ChargingParameter>(map.size());
            //ArrayList<ChargingParameter> list = new ArrayList<ChargingParameter>(map.size());
            for (Map.Entry<String, Object> entry : map.entrySet()) {
                list.add(new ChargingParameter(entry.getKey(), entry.getValue()));
            }
            RatingResult result = engine.rate(list);
            node.replyWithOKResult(from, result.toErlang());
        } catch (Exception e) {
            log.error("Rating failed: {}", e);
            node.replyWithError(from, "rating_failed");
        }
    }

    private void doCompile() throws Exception {
        if (false == params instanceof String) {
            throw new Exception("Invalid parameter value: " + params.getClass().getName());
        }
        engine.compile((String) params);
        node.replyWithOK(from);
    }

    private void doReload() throws Exception {
        engine.reload();
        node.replyWithOK(from);
    }
}
