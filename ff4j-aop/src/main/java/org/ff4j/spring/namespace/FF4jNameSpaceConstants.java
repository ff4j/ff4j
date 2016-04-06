package org.ff4j.spring.namespace;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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
 * XML Tags for dedicated Spring namespace
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jNameSpaceConstants {

    /** Namespace Prefix for tgas. **/
   public static final String PREFIX = "ff4j:";

    /** XML Tag. **/
   public static final String TAG_PLACEHOLDER = "property-placeholder";

    /** XML Tag. **/
   public static final String TAG_FF4J = "ff4j";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_ID = "id";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_AUTOCREATE = "autocreate";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_FILENAME = "fileName";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_AUTH_MANAGER = "authManager";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_STORE_PROPERTY = "storeProperty";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_STORE_FEATURE = "storeFeature";
   
   /** XML Attribute. **/
   public static final String ATT_FF4J_AUDIT_REPOSITORY = "auditRepository";
   
   /** Bean id. */
   public static final String BEANID_PLACEHOLDER_CONF = "ff4j.placeholderconfigurer";
   
   /** Bean id. */
   public static final String BEANID_PLACEHOLDER = "ff4j.placeholder";
   
   private FF4jNameSpaceConstants() {}
   
}
