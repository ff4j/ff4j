package org.ff4j.audit;

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
