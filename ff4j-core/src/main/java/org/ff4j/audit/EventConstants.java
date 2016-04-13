package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
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

/**
 * Constant for Events.
 * 
 * @author clunven
 */
public class EventConstants {
	
	/** ACTIONS. */
	public static final String ACTION_CONNECT 		= "connect";
	public static final String ACTION_DISCONNECT 	= "disconnect";
	public static final String ACTION_TOGGLE_ON 	= "toggle-on";
	public static final String ACTION_TOGGLE_OFF 	= "toggle-off";
	public static final String ACTION_CREATE 		= "create";
	public static final String ACTION_DELETE 		= "delete";
	public static final String ACTION_UPDATE 		= "update";
	public static final String ACTION_CLEAR 		= "clear";
	public static final String ACTION_CHECK_OK      = "checkOn";
	public static final String ACTION_CHECK_OFF     = "checkOff";
    
	/** TARGETS. */
	public static final String TARGET_FEATURE 	= "feature";
	public static final String TARGET_GROUP 	= "group";
	public static final String TARGET_PROPERTY 	= "property";
	public static final String TARGET_USER 		= "user";
	public static final String TARGET_FSTORE 	= "featureStore";
	public static final String TARGET_PSTORE 	= "propertyStore";
	
	/** SOURCES. */
	public static final String SOURCE_JAVA     = "JAVA_API";
	public static final String SOURCE_WEB      = "EMBEDDED_SERVLET";
	public static final String SOURCE_WEBAPI   = "WEB_API";
	public static final String SOURCE_SSH      = "SSH";

	/** total hit count. */
	public static final String TITLE_PIE_HITCOUNT = "Total Hit Counts";

	/** distribution. */
	public static final String TITLE_BARCHAR_HIT = "HitCounts Distribution";


	private EventConstants() {}
}
