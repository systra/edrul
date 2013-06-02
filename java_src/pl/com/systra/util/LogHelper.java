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

package pl.com.systra.util;

import org.drools.spi.KnowledgeHelper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogHelper {
    public static void log(final KnowledgeHelper drools, final String message){
        LoggerFactory.getLogger(LogHelper.class).debug("Rule <{}> triggered: {}", drools.getRule().getName(), message);
        //log.debug("FH: {}", drools.getActivation().getFactHandle());
    }

    /**
     * Log a debug message from a rule, using package and name of the rule as the Log4J
     * category.
     */
    public static void log(final KnowledgeHelper drools, final String message, final Object... parameters) {
        //final String category = drools.getRule().getPackageName() + "." + drools.getRule().getName();
        final String formattedMessage = String.format(message, parameters);
        //Logger.getLogger(category).debug(formattedMessage);
        LogHelper.log(drools, formattedMessage);
    }
}
