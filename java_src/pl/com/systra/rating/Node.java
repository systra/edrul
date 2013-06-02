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
import java.io.IOException;
import java.util.concurrent.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Node {

    private static final Logger log = LoggerFactory.getLogger(Node.class);
    private final String nodeName;
    private final String peerNode;
    private final String cookie;
    private RatingEngine engine;
    private ExecutorService exec;
    private OtpNode node;
    private OtpMbox mbox;
    private OtpErlangPid self;
    private OtpErlangPid peerPid = null;

    public Node(String peerNode, String nodeName, String cookie, String rulesConfig) throws Exception {
        this.peerNode = peerNode;
        this.nodeName = nodeName;
        this.cookie = cookie;
        engine = new RatingEngine(rulesConfig);
        exec = Executors.newFixedThreadPool(20);
        createNode();
    }

    private void createNode() throws IOException {
        try {
            this.node = new OtpNode(this.nodeName, this.cookie);
            log.info("node running: " + this.nodeName + "@" + java.net.InetAddress.getLocalHost().getHostName());
            log.info("peer: " + this.peerNode);
            if (OtpEpmd.publishPort(node)) {
                log.info("node successfully registered");
            } else {
                log.warn("node was already registered");
            }
            String[] names = OtpEpmd.lookupNames();
            for (String name : names) {
                log.debug(name);
            }
            this.mbox = node.createMbox(nodeName);
            this.self = this.mbox.self();
            log.debug("self: " + this.self.toString());
        } catch (IOException e) {
            log.error("node cration failed: " + e.toString());
            throw e;
        }
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 4) {
            System.out.println("wrong number of arguments");
            System.out.println("expected: peerNode nodeName cookie rulesConfigFile");
            return;
        }
        Node main = new Node(args[0], args[1], args[2], args[3]);
        main.process();
    }

    private void process() {

        if (node.ping(peerNode, 2000)) {
            log.info(peerNode + ": pong.");
        } else {
            log.warn(peerNode + ": pang!");
        }

        while (true) {
            try {
                OtpErlangObject msg = mbox.receive();
                log.debug("Incoming message: {}", msg.toString());
                Task task = new Task(msg, this, engine);
                if (task.getAction().equals("handshake")) {
                    replyWithOKResult(task.getFrom(), mbox.self());
                } else if (task.getAction().equals("stop")) {
                    log.info("Stop requested by peer. Exiting.");
                    replyWithOK(task.getFrom());
                    OtpEpmd.unPublishPort(node);
                    System.exit(0);
                } else {
                    exec.submit(task);
                }
            } catch (IllegalArgumentException e) {
                log.error("Invalid message format: " + e);
            } catch (OtpErlangDecodeException e) {
                log.error("Received message could not be decoded: " + e);
            } catch (OtpErlangExit e) {
                log.error("Remote pid " + e.pid() + " has terminated");
                System.exit(1);
            } catch (Exception e) {
                String trace = "";
                for (StackTraceElement st : e.getStackTrace()) {
                    trace += " -- " + st.toString();
                }
                log.error(e.toString() + trace);
                System.exit(1);
            }
        }
    }

    public void replyWithError(OtpErlangPid to, String message) {
        OtpErlangAtom errorAtom = new OtpErlangAtom("error");
        OtpErlangTuple error = new OtpErlangTuple(new OtpErlangObject[]{errorAtom, new OtpErlangString(message)});
        sendReply(to, error);
    }

    public void replyWithOKResult(OtpErlangPid to, OtpErlangObject object) {
        OtpErlangAtom okAtom = new OtpErlangAtom("ok");
        OtpErlangTuple result = new OtpErlangTuple(new OtpErlangObject[]{okAtom, object});
        sendReply(to, result);
    }

    public void replyWithOK(OtpErlangPid to) {
        sendReply(to, new OtpErlangAtom("ok"));
    }

    public void sendReply(OtpErlangPid to, OtpErlangObject result) {
        OtpErlangAtom replyAtom = new OtpErlangAtom("reply");
        OtpErlangTuple reply = new OtpErlangTuple(new OtpErlangObject[]{replyAtom, result});
        log.info("Sending reply: " + reply.toString());
        mbox.send(to, reply);
    }
}
