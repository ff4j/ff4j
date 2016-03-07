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
public interface EventConstants {
	
	/** ACTIONS. */
	String ACTION_CONNECT 		= "connect";
	String ACTION_DISCONNECT 	= "disconnect";
	String ACTION_TOGGLE_ON 	= "toggle-on";
	String ACTION_TOGGLE_OFF 	= "toggle-off";
	String ACTION_CREATE 		= "create";
	String ACTION_DELETE 		= "delete";
	String ACTION_UPDATE 		= "update";
	String ACTION_CLEAR 		= "clear";
	String ACTION_CHECK_OK      = "checkOn";
	String ACTION_CHECK_OFF     = "checkOff";
    
	/** TARGETS. */
	String TARGET_FEATURE 	= "feature";
	String TARGET_GROUP 	= "group";
	String TARGET_PROPERTY 	= "property";
	String TARGET_USER 		= "user";
	String TARGET_FSTORE 	= "featureStore";
	String TARGET_PSTORE 	= "propertyStore";
	
	/** SOURCES. */
	String SOURCE_JAVA     = "JAVA_API";
	String SOURCE_WEB      = "EMBEDDED_SERVLET";
	String SOURCE_WEBAPI   = "WEB_API";
	String SOURCE_SSH      = "SSH";
    
    
}
