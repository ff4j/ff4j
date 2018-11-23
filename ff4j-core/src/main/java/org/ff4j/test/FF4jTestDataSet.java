package org.ff4j.test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.PonderationToggleStrategy;
import org.ff4j.feature.togglestrategy.expression.ExpressionToggleStrategy;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.PropertyClass;
import org.ff4j.property.PropertyDouble;
import org.ff4j.property.PropertyFloat;
import org.ff4j.property.PropertyInstant;
import org.ff4j.property.PropertyInt;
import org.ff4j.property.PropertyListBigDecimal;
import org.ff4j.property.PropertyListBigInteger;
import org.ff4j.property.PropertyListBoolean;
import org.ff4j.property.PropertyListByte;
import org.ff4j.property.PropertyListCalendar;
import org.ff4j.property.PropertyListClass;
import org.ff4j.property.PropertyListDouble;
import org.ff4j.property.PropertyListFloat;
import org.ff4j.property.PropertyListInstant;
import org.ff4j.property.PropertyListInt;
import org.ff4j.property.PropertyListLocalDateTime;
import org.ff4j.property.PropertyListLogLevel;
import org.ff4j.property.PropertyListLong;
import org.ff4j.property.PropertyListShort;
import org.ff4j.property.PropertyListString;
import org.ff4j.property.PropertyLocalDateTime;
import org.ff4j.property.PropertyLogLevel;
import org.ff4j.property.PropertyLogLevel.LogLevel;
import org.ff4j.property.PropertyLong;
import org.ff4j.property.PropertyShort;
import org.ff4j.property.PropertyString;
import org.ff4j.security.FF4jGrantees;
import org.ff4j.security.FF4jPermission;
import org.ff4j.user.FF4jRole;
import org.ff4j.user.FF4jUser;
import org.ff4j.utils.Util;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

/**
 * For coherence, each store implementation will be tested with same dataset. Here are the constants contains in this DATASET.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface FF4jTestDataSet {
    
    // -- Features Names -- 
    String F1 = "f1";
    String F2 = "f2";
    String F3 = "f3";
    String F4 = "f4";
    String F5 = "f5";
    String FEATURE_FOR_TEST = "newFeatureForTest";
    
    String GRP0 = "GRP0";
    String GRP1 = "GRP1";
    String GRPX = "GRPX";
    
    // -- Properties --
    String PBigDecimal  = "pBigDecimal";
    String PBigInteger  = "pBigInteger";
    
    String P_F2_PPINT       = "ppint";
    String P_F2_PDOUBLE     = "ppdouble";
    String P_F2_DIGITVALUE  = "digitValue";
    String PROP_FLAG        = "flag";
    
    String USER_JOHN        = "john";
    String USER_SARAH       = "sarah";
    
    String ROLE_ADMIN       = "ADMINISTRATOR";
    String ROLE_SUPERADMIN  = "SUPERUSER";
    String ROLE_USER        = "USER";
    String ROLE_EVERYONE    = "EVERYONE";
    
    /**
     * DataSet for configuration.
     *
     * @return
     *      expected configuration
     */
    default FF4jConfigFile expectConfig() {
        FF4jConfigFile confFile = new FF4jConfigFile();
        confFile.setFeatures(expectedFeatures());
        confFile.setProperties(expectedProperties());
        confFile.setAudit(true);
        confFile.setUsers(expectedUsers());
        confFile.setRoles(expectedRoles());
        return confFile;
    }
    
    /**
     * Users.
     */
    default Map < String, FF4jUser> expectedUsers() {
        Map < String, FF4jUser> users = new HashMap<>();
        users.put(USER_JOHN, new FF4jUser(USER_JOHN).fisrtName("John").lastName("Connor")
                            .description("sample description if OK")
                            .addPermission(FF4jPermission.ADMIN_FEATURES)
                            .addRole(ROLE_ADMIN));
        users.put(USER_SARAH, new FF4jUser(USER_SARAH).fisrtName("Sarah").lastName("Connor")
                .description("sample description if OK")
                .addPermission(FF4jPermission.FEATURE_VIEW)
                .addRole(ROLE_USER));
        return users;
    }
    
    default Map < String, FF4jRole> expectedRoles() {
        Map < String, FF4jRole> roles = new HashMap<>();
        roles.put(ROLE_EVERYONE, new FF4jRole(ROLE_EVERYONE).grant(
                        FF4jPermission.VIEW_FEATURES,FF4jPermission.VIEW_PROPERTIES));
        roles.put(ROLE_ADMIN, new FF4jRole(ROLE_ADMIN).grant(
                        FF4jPermission.ADMIN_FEATURES,FF4jPermission.ADMIN_PROPERTIES, 
                        FF4jPermission.TOGGLE_FEATURES, FF4jPermission.VIEW_FEATURES, 
                        FF4jPermission.VIEW_PROPERTIES, FF4jPermission.VIEW_AUDITTRAIL, 
                        FF4jPermission.VIEW_FEATUREUSAGE));
        roles.put(ROLE_SUPERADMIN, new FF4jRole(ROLE_SUPERADMIN).grant(
                        FF4jPermission.TOGGLE_FEATURES, FF4jPermission.VIEW_FEATURES,
                        FF4jPermission.VIEW_PROPERTIES,  FF4jPermission.VIEW_AUDITTRAIL, 
                        FF4jPermission.VIEW_FEATUREUSAGE));
        roles.put(ROLE_USER, new FF4jRole(ROLE_USER).grant(
                        FF4jPermission.VIEW_FEATURES, FF4jPermission.VIEW_PROPERTIES,
                        FF4jPermission.VIEW_AUDITTRAIL,  FF4jPermission.VIEW_FEATUREUSAGE));
        return roles;
    }
    
    default Map < String, Feature> expectedFeatures() {
        Map < String, Feature> results = new HashMap<>();
        results.put(F1, new Feature(F1)
                        .enable(false)
                        .description("some desc"));
        
        Feature f2 = new Feature(F2)
            .enable(true).group(GRP1).description("description")
            .addProperty(new PropertyInt(P_F2_PPINT, 12))
            .addProperty(new PropertyDouble(P_F2_PDOUBLE, 12.5))
            .addProperty(new PropertyBoolean("ppboolean", true))
            .addProperty(new PropertyString("ppstring", "hello"))
            .addProperty(new PropertyListInt("ppListInt", 12,13,14))
            .addProperty(new PropertyLogLevel("myLogLevel",LogLevel.DEBUG))
            .addProperty(new PropertyInt("digitValue", 1).addFixedValues(0,1,2,3))
            .addProperty(new PropertyString("regionIdentifier", "NA").addFixedValues("NA", "APAC", "EMEA"))
            .addToggleStrategy(new PonderationToggleStrategy(1));
        Map < FF4jPermission, FF4jGrantees > permissions = f2.getAccessControlList().getPermissions();
        permissions.put(FF4jPermission.FEATURE_TOGGLE, new FF4jGrantees(Util.setOf(USER_JOHN), new HashSet<>()));
        permissions.put(FF4jPermission.FEATURE_VIEW, new FF4jGrantees(new HashSet<>(), Util.setOf(ROLE_EVERYONE)));
        results.put(F2, f2);
        
        Feature f3 = new Feature(F3).enable(false).group("GRP0").description("description");
        f3.getAccessControlList().getPermissions()
          .put(FF4jPermission.FEATURE_VIEW, 
                  new FF4jGrantees(new HashSet<>(), Util.setOf("USER")));
        results.put(F3, f3);
        
        results.put(F4, new Feature(F4)
                        .enable(true).group("GRP1").description("some desc")
                        .addToggleStrategy(new ExpressionToggleStrategy(F4, "f2 | f3")));
        
        return results;
    }

    default Map < String, Property<?> > expectedProperties() {
        
        Map < String, Property<?>> r = new HashMap<>();
        r.put(PBigDecimal, new PropertyBigDecimal(PBigDecimal, new BigDecimal(1.5)));
        r.put(PBigInteger, new PropertyBigInteger(PBigInteger, new BigInteger("123456")));
        PropertyBoolean    pBoolean     = new PropertyBoolean("pBoolean", true);
        PropertyByte       pByte        = new PropertyByte("pByte", (byte) 'p');
        PropertyCalendar   pCalendar    = new PropertyCalendar("pCalendar", Calendar.getInstance());
        PropertyClass      pClass       = new PropertyClass("pClass", String.class);
        PropertyDouble     pDouble      = new PropertyDouble("pDouble", new Double(20.0));
        PropertyFloat      pFloat       = new PropertyFloat("pFloat", new Float(20.0));
        PropertyInstant    pInstant     = new PropertyInstant("pInstant", Instant.now());
        PropertyInt        pInt         = new PropertyInt("pInt", 10);
        PropertyLocalDateTime pLocal    = new PropertyLocalDateTime("pLocal", LocalDateTime.now());
        PropertyLogLevel   pLogLevel    = new PropertyLogLevel("pLogLevel", LogLevel.INFO); 
        PropertyLong       pLong        = new PropertyLong("pLong", 123L);
        PropertyShort      pShort       = new PropertyShort("pShort", (short) 5);
        PropertyString     pString      = new PropertyString("pString", "pString");
        r.put(pBoolean.getUid(), pBoolean);
        r.put(pByte.getUid(), pByte);
        r.put(pCalendar.getUid(), pCalendar);
        r.put(pClass.getUid(), pClass);
        r.put(pDouble.getUid(), pDouble);
        r.put(pFloat.getUid(), pFloat);
        r.put(pInstant.getUid(), pInstant);
        r.put(pInt.getUid(), pInt);
        r.put(pLocal.getUid(), pLocal);
        r.put(pLogLevel.getUid(), pLogLevel);
        r.put(pLong.getUid(), pLong);
        r.put(pShort.getUid(), pShort);
        r.put(pString.getUid(), pString);
        
        PropertyListBigDecimal listBigDecimal  = new PropertyListBigDecimal("listBigDecimal", new BigDecimal(1.5),  new BigDecimal(2.5));
        PropertyListBigInteger listBigInteger  = new PropertyListBigInteger("listBigInteger", new BigInteger("123456"),  new BigInteger("789012"));
        PropertyListBoolean    listBoolean     = new PropertyListBoolean("listBoolean", true, false);
        PropertyListByte       listByte        = new PropertyListByte("listByte", (byte) 'p', (byte) 'l');
        PropertyListCalendar   listCalendar    = new PropertyListCalendar("listCalendar", Calendar.getInstance(), Calendar.getInstance());
        PropertyListClass      listClass       = new PropertyListClass("listClass", String.class, Integer.class);
        PropertyListDouble     listDouble      = new PropertyListDouble("listDouble", new Double(20.0), new Double(30.0) );
        PropertyListFloat      listFloat       = new PropertyListFloat("listFloat", new Float(20.0), new Float(30.0));
        PropertyListInstant    listInstant     = new PropertyListInstant("listInstant", Instant.now(), Instant.now());
        PropertyListInt        listInt         = new PropertyListInt("listInt", 10, 20);
        PropertyListLocalDateTime listLocal    = new PropertyListLocalDateTime("listLocal", LocalDateTime.now(), LocalDateTime.now());
        PropertyListLogLevel   listLogLevel    = new PropertyListLogLevel("listLogLevel", LogLevel.INFO, LogLevel.DEBUG); 
        PropertyListLong       listLong        = new PropertyListLong("listLong", 123L, 456L);
        PropertyListShort      listShort       = new PropertyListShort("listShort", (short) 5, (short) 6);
        PropertyListString     listString      = new PropertyListString("listString", "pString", "listString");
        r.put(listBigDecimal.getUid(), listBigDecimal);
        r.put(listBigInteger.getUid(), listBigInteger);
        r.put(listBoolean.getUid(), listBoolean);
        r.put(listByte.getUid(), listByte);
        r.put(listCalendar.getUid(), listCalendar);
        r.put(listClass.getUid(), listClass);
        r.put(listDouble.getUid(), listDouble);
        r.put(listFloat.getUid(), listFloat);
        r.put(listInstant.getUid(), listInstant);
        r.put(listInt.getUid(), listInt);
        r.put(listLocal.getUid(), listLocal);
        r.put(listLogLevel.getUid(), listLogLevel);
        r.put(listLong.getUid(), listLong);
        r.put(listShort.getUid(), listShort);
        r.put(listString.getUid(), listString);
        
        return r;
    }
    
}
