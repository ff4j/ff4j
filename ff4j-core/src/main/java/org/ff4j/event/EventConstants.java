package org.ff4j.event;

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
	
	/** ATTRIBUTES. */
	public static final String ATTRIBUTE_ID       = "id";
	public static final String ATTRIBUTE_TIME     = "timestamp";
    public static final String ATTRIBUTE_HOST     = "hostName";
    public static final String ATTRIBUTE_SOURCE   = "source";
    public static final String ATTRIBUTE_NAME     = "name";
    public static final String ATTRIBUTE_TYPE     = "type"; 
    public static final String ATTRIBUTE_ACTION   = "action";
    public static final String ATTRIBUTE_DURATION = "duration";
    public static final String ATTRIBUTE_USER     = "user";
    public static final String ATTRIBUTE_KEYS     = "customKeys";

    /** CUSTOM KEY. */
    public static final String KEY_ACL = "accessControlList";

	/** total hit count. */
	public static final String TITLE_PIE_HITCOUNT = "Total Hit Counts";

	/** distribution. */
	public static final String TITLE_BARCHAR_HIT = "HitCounts Distribution";

	private EventConstants() {}
}
