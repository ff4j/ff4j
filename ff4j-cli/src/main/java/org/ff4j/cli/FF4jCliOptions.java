package org.ff4j.cli;

/*
 * #%L
 * ff4j-cli
 * %%
 * Copyright (C) 2013 - 2016 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;

/**
 * Command line options.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jCliOptions {

    /** String constants */
    private static final String FEATURE = "feature";
    private static final String FEATURENAME = "featurename";
    private static final String TARGET_FEATURE_TO_UPDATE = "target feature to update";

    /**
	 * Remove public constructor for utilities.
	 */
	private FF4jCliOptions() {
	}
	
	/**
	 * Options for command Line. The connect method will select an Environnement.
	 *
	 * @return
	 * 		elements
	 */
    public static Options connectOptions() {
        Options options = new Options();
        options.addOption(Option.builder("u").longOpt("user")
                .hasArg().argName("userName")
                .required(false)
                .desc("username to connect to env").build());
        options.addOption(Option.builder("p").longOpt("passwd")
                .hasArg().argName("password")
                .required(false)
                .desc("username to connect to env").build());
        return options;
    }
    
    /**
     * Enable a feature.
     *
     * @return
     * 		target option
     */
    public static Options enableFeatureOptions() {
    	 Options options = new Options();
         options.addOption(Option.builder("f").longOpt(FEATURE)
                 .hasArg().argName(FEATURENAME)
                 .required(true)
                 .desc(TARGET_FEATURE_TO_UPDATE).build());
         return options;
    }
    
    /**
     * Enable a feature.
     *
     * @return
     * 		target option
     */
    public static Options enableGroupOptions() {
    	 Options options = new Options();
         options.addOption(Option.builder("g").longOpt("group")
                 .hasArg().argName("groupName")
                 .required(true)
                 .desc("target group to update").build());
         return options;
    }
    
    /**
     * Enable a feature.
     *
     * @return
     * 		target option
     */
    public static Options grantOptions() {
    	 Options options = new Options();
    	 
    	 options.addOption(Option.builder("f").longOpt(FEATURE)
                 .hasArg().argName(FEATURENAME)
                 .required(true)
                 .desc(TARGET_FEATURE_TO_UPDATE).build());
    	 
    	 options.addOption(Option.builder("r").longOpt("role")
                 .hasArg().argName("roleName")
                 .required(true)
                 .desc("target role to grant/revoke").build());
         return options;
    }
    
    /**
     * Enable a feature.
     *
     * @return
     * 		target option
     */
    public static Options addGroupOptions() {
    	 Options options = new Options();
    	 options.addOption(Option.builder("f").longOpt(FEATURE)
                 .hasArg().argName(FEATURENAME)
                 .required(true)
                 .desc(TARGET_FEATURE_TO_UPDATE).build());
         options.addOption(Option.builder("g").longOpt("group")
                 .hasArg().argName("groupName")
                 .required(true)
                 .desc("target group to update").build());
    	 return options;
    }
    
    /**
     * Enable a feature.
     *
     * @return
     * 		target option
     */
    public static Options propertyOptions() {
    	 Options options = new Options();
    	 options.addOption(Option.builder("p").longOpt("property")
                 .hasArg().argName("property")
                 .required(true)
                 .desc("target property to update").build());
         options.addOption(Option.builder("v").longOpt("value")
                 .hasArg().argName("value")
                 .required(true)
                 .desc("new value for property").build());
    	 return options;
    }

	
}
