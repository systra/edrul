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

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Properties;

import org.drools.FactHandle;
import org.drools.KnowledgeBase;
import org.drools.RuleBase;
import org.drools.StatefulSession;
import org.drools.agent.AgentEventListener;
import org.drools.agent.KnowledgeAgent;
import org.drools.agent.KnowledgeAgentConfiguration;
import org.drools.agent.KnowledgeAgentFactory;
import org.drools.agent.RuleAgent;
import org.drools.builder.KnowledgeBuilder;
import org.drools.builder.KnowledgeBuilderFactory;
import org.drools.builder.ResourceType;
import org.drools.io.ResourceChangeScannerConfiguration;
import org.drools.io.ResourceFactory;
import org.drools.runtime.StatelessKnowledgeSession;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class RatingEngine implements AgentEventListener {

    //~ Static fields/initializers ---------------------------------------------
    private static final Logger log = LoggerFactory.getLogger(RatingEngine.class);
    private static HashMap<String, RuleBase> rulesCache = new HashMap<String, RuleBase>();
    static final String CACHE_RULES = "CacheRules";
    private static boolean caching = true;
    //make the lock object final so that no one can change/reset it
    private final Object o = new Object();
    //~ Instance fields --------------------------------------------------------
    protected HashMap<Object, FactHandle> handlers = null;
    private String rulesConfiguration;
    private KnowledgeBase kbase;
    private KnowledgeAgent agent;

    //~ Methods ----------------------------------------------------------------
    public RatingEngine(String configFile) throws Exception {
        rulesConfiguration = configFile;
        if (caching) {
            try {
                loadRules();
            } catch (Exception e) {
                throw new Exception("Error while loading RuleBase", e);
            }
        }
    }

    public void loadRules() throws Exception {
        log.info("Loading file: {}", this.rulesConfiguration);
        ResourceFactory.getResourceChangeNotifierService().start();
        ResourceFactory.getResourceChangeScannerService().start();

        ResourceChangeScannerConfiguration sconf = ResourceFactory.getResourceChangeScannerService().newResourceChangeScannerConfiguration();
        sconf.setProperty("drools.resource.scanner.interval", "120"); // scan every 120s
        ResourceFactory.getResourceChangeScannerService().configure(sconf);

        KnowledgeAgentConfiguration aconf = KnowledgeAgentFactory.newKnowledgeAgentConfiguration();
        aconf.setProperty("drools.agent.scanDirectories", "true");
        //aconf.setProperty("drools.agent.scanResources", "true");
        //IMPORTANT!!! set new instance false will prevent agent from creating a new knowledge base!
        aconf.setProperty("drools.agent.newInstance", "false");
        KnowledgeAgent knowledgeAgent = KnowledgeAgentFactory.newKnowledgeAgent("RA", aconf);
        //knowledgeAgent.setSystemEventListener(new PrintStreamSystemEventListener());
        knowledgeAgent.setSystemEventListener(this);
        //knowledgeAgent.applyChangeSet(ResourceFactory.newFileResource(this.rulesConfiguration)); //"/root/com/me/drools/change-set.xml"));
        knowledgeAgent.applyChangeSet(ResourceFactory.newClassPathResource(this.rulesConfiguration)); //"/root/com/me/drools/change-set.xml"));
        this.agent = knowledgeAgent;
        this.kbase = knowledgeAgent.getKnowledgeBase();
        log.info("Building knowledge base completed");
    }

    protected RuleBase getRuleBase() throws IOException, Exception {
        synchronized (o) {
            return rulesCache.get(CACHE_RULES);
        }
    }

    protected synchronized void loadRuleBase() throws Exception {

        log.info("Rules initialising using configuration <{}>", this.rulesConfiguration);

        // create a temporary properties
        // properties from the file will be loaded for passing to the rule Agent
        Properties tempProps = new Properties();
        InputStream is = Thread.currentThread().getContextClassLoader().getResourceAsStream(this.rulesConfiguration);

        if (is != null) {
            try {
                // create an input stream for reading the properties from
                // the file and load it into the temporary properties object
                is.available();
                tempProps.load(is);
            } catch (IOException ioe) {
                throw new Exception("File <" + this.rulesConfiguration + "> not found in classpath. Message <" + ioe.getMessage() + ">");
            }
        } else {
            throw new Exception("File <" + this.rulesConfiguration + "> not found in classpath");
        }

        RuleAgent tempAgent = RuleAgent.newRuleAgent(tempProps);//, this);
        RuleBase retValue = tempAgent.getRuleBase();

        // update the cache
        synchronized (o) {
            rulesCache.remove(CACHE_RULES);
            rulesCache.put(CACHE_RULES, retValue);
        }
    }

    public static void invalidateRuleCache(String symbolicName) {
        rulesCache.remove(symbolicName);
    }

    public void reloadRuleCache() {
        try {
            loadRuleBase();
        } catch (Throwable t) {
            log.error("Error while reloading rule cache: " + t);
        }
    }

    public RatingResult rate(java.util.Collection<ChargingParameter> params) throws Exception {
        StatelessKnowledgeSession session = this.kbase.newStatelessKnowledgeSession();
        //session.addEventListener( new DebugAgendaEventListener() );
        //session.addEventListener( new DebugWorkingMemoryEventListener() );
        ArrayList<Object> rulesMemoryContext = new ArrayList<Object>();
        if ((params != null) && !params.isEmpty()) {
            rulesMemoryContext.addAll(params);
        }
        rulesMemoryContext.add(new ChargingParameter("EVENT_DATE", new Date()));
        RatingResult ratingResult = new RatingResult();
        session.setGlobal("rating", ratingResult);
        //for (Object o : rulesMemoryContext) {
        //log.debug("In memory context = " + o);
        //}
        log.info(" ==== Calling Rule Engine ====");
        session.execute((Collection<Object>) rulesMemoryContext);
        //session.execute(CommandFactory.newInsertElements(rulesMemoryContext));
        log.info(" ==== Rules Complete =====");
        log.info("Rating result: " + ratingResult);
        return ratingResult;
    }

    public void compile(String file) {
        final KnowledgeBuilder kbuilder = KnowledgeBuilderFactory.newKnowledgeBuilder();
        // this will parse and compile in one step
        kbuilder.add(ResourceFactory.newClassPathResource(file, RatingEngine.class), ResourceType.DRL);
        // Check the builder for errors
        if (kbuilder.hasErrors()) {
            log.debug("errors: {}", kbuilder.getErrors().toString());
            throw new RuntimeException("Unable to compile: " + file);
        }
        log.debug("Rule file compilation successfull");
    }

    public void reload() {
        log.info("Reloading rules!");
        reloadRuleCache();
    }

    //
    // AgentEventListener interface implementation
    //
    @Override
    public void debug(String message) {
        log.debug(message);
    }

    @Override
    public void debug(String message, Object object) {
        log.debug(message + ": " + object.toString());
    }

    @Override
    public void exception(Throwable e) {
        log.error("Exception: " + e);
    }

    @Override
    public void exception(String message, Throwable e) {
        log.error(message + ": " + e);
    }

    @Override
    public void info(String message) {
        log.info(message);
    }

    @Override
    public void info(String message, Object object) {
        log.info(message + ":" + object.toString());
    }

    @Override
    public void setAgentName(String name) {
        log.info(name);
    }

    @Override
    public void warning(String message) {
        log.warn(message);
    }

    @Override
    public void warning(String message, Object object) {
        log.warn(message + ":" + object.toString());
    }
}
