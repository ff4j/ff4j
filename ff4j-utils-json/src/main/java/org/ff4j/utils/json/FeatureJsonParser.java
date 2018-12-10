package org.ff4j.utils.json;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.io.IOException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.property.Property;
import org.ff4j.security.FF4jAcl;
import org.ff4j.security.FF4jGrantees;
import org.ff4j.security.FF4jPermission;

import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Unmarshalling data from JSON with Jackson.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureJsonParser {

    /** Jackson mapper. */
    private static ObjectMapper objectMapper = FF4jCustomObjectMapper.createDefaultMapper();
    
    /** Constants. */
    public static String UID                        = "uid";
    public static String ENABLED                    = "enabled";
    public static String GROUP                      = "group";
    public static String CLAZZ                      = "clazz";
    public static String CREATION_DATE              = "creationDate";
    public static String DESCRIPTION                = "description";
    public static String PROPERTIES                 = "properties";
    public static String MODIF_DATE                 = "lastModifiedDate";
    public static String ACCESSCONTROL              = "accessControlList";
    public static String TOGGLE_STRATEGIES          = "toggleStrategies";
    public static String TOGGLE_PREDICATE_CLASSNAME = "className";
    public static String EMPTY                      = "empty";
    public static String PERMISSIONS                = "permissions";
    public static String PERM_USERS                 = "users";
    public static String PERM_ROLES                 = "roles";
    
    /** Parsing Dates. */
    public static final String            DATEPATTERN = "yyyy-MM-dd'T'HH:mm:ss.SSS";
    public static final SimpleDateFormat  SDF         = new SimpleDateFormat(DATEPATTERN);
    public static final DateTimeFormatter DF          = DateTimeFormatter.ofPattern(DATEPATTERN);
    
    /** Hide constructor. */
    private FeatureJsonParser() {}
    
    // -----------------------------------
    // ---------- Features ---------------
    // -----------------------------------
    
    /**
     * Parse the json expression as array of {@link Feature}.
     *
     * @param json
     *      json expression
     * @return
     *      array of feature
     */
    @SuppressWarnings("unchecked")
    public static Feature[] parseJsonFeatureArray(String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            List<LinkedHashMap<String, Object>> flipMap = objectMapper.readValue(json, List.class);
            Feature[] fArray = new Feature[flipMap.size()];
            int idx = 0;
            for (LinkedHashMap<String, Object> ll : flipMap) {
                fArray[idx++] = parseFeature(ll);
            }
            return fArray;
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }
    
    /**
     * Unmarshall {@link Feature} from json string.
     *
     * @param json
     *            json representation of feature.
     * @return feature object
     */
    @SuppressWarnings("unchecked")
    public static Feature parseJsonFeature(String json) {
        try {
            return parseFeature(objectMapper.readValue(json, HashMap.class));
        } catch (IOException e) {
            throw new IllegalArgumentException("Cannot parse json as Feature " + json, e);
        }
    }
    
    @SuppressWarnings("unchecked")
    private static Feature parseFeature(Map<String, Object> fMap) {
        Feature f = new Feature((String) fMap.get(UID));
        if (fMap.containsKey(ENABLED)) {
            f.setEnable((Boolean) fMap.get(ENABLED));
        }
        if (fMap.containsKey(DESCRIPTION)) {
            f.setDescription((String) fMap.get(DESCRIPTION));
        }
        if (fMap.containsKey(GROUP)) {
            f.setGroup((String) fMap.get(GROUP));
        }
        if (fMap.containsKey(CREATION_DATE)) {
            f.setCreationDate(parseLocalDateTime((String) fMap.get(CREATION_DATE)));
        }
        if (fMap.containsKey(MODIF_DATE)) {
            f.setLastModified(parseLocalDateTime((String) fMap.get(MODIF_DATE)));
        }
        // AccessControl List
        f.setAccessControlList(
                parseAccessControlList((LinkedHashMap<String, Object>) fMap.get(ACCESSCONTROL)));
        // Properties
        f.setProperties(PropertyJsonParser.
                parseProperties((LinkedHashMap<String, Object>) fMap.get(PROPERTIES)));
        // ToggleStrategies
        f.setToggleStrategies(
                parseTogglePredicates(f.getUid(), (List<Object>) fMap.get(TOGGLE_STRATEGIES)));
        return f;
    }
    
    private static LocalDateTime parseLocalDateTime(String value) {
        while (value.length() != 23) {
            value = value + "0";
        }
        return LocalDateTime.parse(value, DF);
    }
    
    // -----------------------------------
    // ------ AccessControlList ----------
    // -----------------------------------
    
    @SuppressWarnings("unchecked")
    public static FF4jAcl parseAccessControlList(Map<String, Object> fMap) {
        FF4jAcl acl = new FF4jAcl();
        boolean isEmpty = (Boolean) fMap.get(EMPTY);
        if (!isEmpty) {
            Map<String, Object> permsMap = (LinkedHashMap<String, Object>) fMap.get(PERMISSIONS);
            for (String currentPermission : permsMap.keySet()) {
                Map < String, List < String> > mapOfGrantees = 
                        (Map<String, List<String>>) permsMap.get(currentPermission);
                FF4jGrantees grantees = new FF4jGrantees();
                grantees.getUsers().addAll(mapOfGrantees.get(PERM_USERS));
                grantees.getRoles().addAll(mapOfGrantees.get(PERM_ROLES));
                acl.getPermissions().put(FF4jPermission.valueOf(currentPermission), grantees);
            }
        }
        return acl;
    }

    @SuppressWarnings("unchecked")
    public static Set<FF4jPermission> parseJsonPermissions(String json) {
        if (json == null) return null;
        try {
            Set<String>  perms = objectMapper.readValue(json, Set.class);
            return perms.stream().map(FF4jPermission::valueOf).collect(Collectors.toSet());
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse json list");
        }
    }
    
    
   
    
    /**
     * Convert feature array to json.
     *
     * @param features
     *            target features
     * @return json string
     */
    public static String featureArrayToJson(Feature[] features) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        if (features != null) {
            boolean first = true;
            for (Feature feature : features) {
                sb.append(first ? "" : ",");
                sb.append(feature.toJson());
                first = false;
            }
        }
        sb.append("]");
        return sb.toString();
    }
    
    // -----------------------------------
    // ------- TogglePredicate -----------
    // -----------------------------------
    
    @SuppressWarnings("unchecked")
    private static List < TogglePredicate > parseTogglePredicates(String uid, List <Object> toggleMap) {
        List < TogglePredicate > listOfPredicates = new ArrayList<TogglePredicate>();
        if (toggleMap != null) {
            for (Object togglePredicate : toggleMap) {
                listOfPredicates.add(parseTogglePredicate(uid, (Map<String, Object>) togglePredicate));
            }
        }
        return listOfPredicates;
    }
    
    /**
     * Parse json string to get {@link FlippingStrategy}.
     * 
     * @param uid
     *            identifier
     * @param json
     *            json expression
     * @return flip strategy
     */
    @SuppressWarnings("unchecked")
    public static TogglePredicate parseJsonTogglePredicate(String uid, String json) {
        if (null == json || "".equals(json)) {
            return null;
        }
        try {
            return parseTogglePredicate(uid, (HashMap<String, Object>) objectMapper.readValue(json, HashMap.class));
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot parse JSON " + json, e);
        }
    }
    
    /**
     * Parse json string to get {@link FlippingStrategy}.
     * 
     * @param uid
     *            identifier
     * @param json
     *            json expression
     * @return flip strategy
     */
    @SuppressWarnings("unchecked")
    private static TogglePredicate parseTogglePredicate(String uid, Map<String, Object> flipMap) {
        if (null == flipMap || flipMap.isEmpty()) {
            return null;
        }
        String toggleClassType  = (String) flipMap.get(TOGGLE_PREDICATE_CLASSNAME);
        List<HashMap<String, Object>> toggleProperties = (List<HashMap<String, Object>>) flipMap.get(PROPERTIES);
        Set <Property<?>> setOfProperties = new HashSet<>();
        if (toggleProperties != null) {
            toggleProperties.stream()
                            .map(PropertyJsonParser::parsePropertyTag)
                            .forEach(setOfProperties::add);
        }
        return TogglePredicate.of(uid, toggleClassType, setOfProperties);
    }
}
