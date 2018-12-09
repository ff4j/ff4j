package org.ff4j.test;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.ParseException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.PonderationToggleStrategy;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.feature.togglestrategy.expression.ExpressionToggleStrategy;
import org.ff4j.parser.FF4jConfigFile;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyBigDecimal;
import org.ff4j.property.PropertyBigInteger;
import org.ff4j.property.PropertyBoolean;
import org.ff4j.property.PropertyByte;
import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.PropertyClass;
import org.ff4j.property.PropertyDate;
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
import org.ff4j.property.PropertyListDate;
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
    String F1               = "f1";
    String F2               = "f2";
    String F3               = "f3";
    String F4               = "f4";
    String F5               = "f5";
    String FEATURE_FOR_TEST = "newFeatureForTest";
    
    // -- Groups --
    String GRP0             = "GRP0";
    String GRP1             = "GRP1";
    String GRPX             = "GRPX";
    
    // -- Properties --
    String PBigDecimal      = "pBigDecimal";
    String PBigInteger      = "pBigInteger";
    String PBoolean         = "pBoolean";
    String PByte            = "pByte";
    String PCalendar        = "pCalendar";
    String PDate            = "pDate";
    String PClass           = "pClass";
    String PDouble          = "pDouble";
    String PFloat           = "pFloat";
    String PInstant         = "pInstant";
    String PInt             = "pInt";
    String PLocal           = "pLocal";
    String PLogLevel        = "pLogLevel";
    String PLong            = "pLong";
    String PShort           = "pShort";
    String PString          = "pString";
    String PListBigDecimal  = "listBigDecimal";
    String PListBigInteger  = "listBigInteger";
    String PListBoolean     = "listBoolean";
    String PListByte        = "listByte";
    String PListCalendar    = "listCalendar";
    String PListClass       = "listClass";
    String PListDouble      = "listDouble";
    String PListDate        = "listDate";
    String PListFloat       = "listFloat";
    String PListInstant     = "listInstant";
    String PListInt         = "listInt";
    String PListLocal       = "listLocal";
    String PListLogLevel    = "listLogLevel";
    String PListLong        = "listLong";
    String PListShort       = "listShort";
    String PListString      = "listString";
    
    // -- Property List --
    String P_F2_PPINT        = "ppint";
    String P_F2_PDOUBLE      = "ppdouble";
    String P_F2_DIGITVALUE   = "digitValue";
    String PROP_FLAG         = "flag";
    String PROPERTY_FOR_TEST = "newPropertyForTest";
    
    // -- User --
    String USER_JOHN        = "john";
    String USER_SARAH       = "sarah";
    
    // -- Roles --
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
            .addProperty(new PropertyString("regionIdentifier", "NA").addFixedValues("NA", "APAC", "EMEA"));            
        
        Set < Property<?> > ponderationProperties = new HashSet<>();
        ponderationProperties.add(new PropertyDouble(PonderationToggleStrategy.PARAM_WEIGHT, 1.0));
        f2.addToggleStrategy(TogglePredicate.of(F2, PonderationToggleStrategy.class.getName(),ponderationProperties));
        
        Map < FF4jPermission, FF4jGrantees > permissions = f2.getAccessControlList().getPermissions();
        permissions.put(FF4jPermission.FEATURE_TOGGLE, new FF4jGrantees(Util.setOf(USER_JOHN), new HashSet<>()));
        permissions.put(FF4jPermission.FEATURE_VIEW, new FF4jGrantees(new HashSet<>(), Util.setOf(ROLE_EVERYONE)));
        results.put(F2, f2);
        
        Feature f3 = new Feature(F3).enable(false).group("GRP0").description("description");
        f3.getAccessControlList().getPermissions()
          .put(FF4jPermission.FEATURE_VIEW, new FF4jGrantees(new HashSet<>(), Util.setOf("USER")));
        results.put(F3, f3);
        
        Feature f4 = new Feature(F4).enable(true).group("GRP1").description("some desc");
        Set < Property<?> > expressionProperties = new HashSet<>();
        expressionProperties.add(new PropertyString(ExpressionToggleStrategy.PARAM_EXPRESSION, "f2 | f3"));
        f4.addToggleStrategy(TogglePredicate.of(F4, ExpressionToggleStrategy.class.getName(),expressionProperties));
        results.put(F4, f4);
        
        return results;
    }

    default Map < String, Property<?> > expectedProperties() {
        Map < String, Property<?>> r = new HashMap<>();
        try {
            r.put(PBigDecimal,      new PropertyBigDecimal(PBigDecimal, new BigDecimal(1.5)));
            r.put(PBigInteger,      new PropertyBigInteger(PBigInteger, new BigInteger("123456")));
            r.put(PBoolean,         new PropertyBoolean(PBoolean, true));
            r.put(PByte,            new PropertyByte(PByte, (byte) 'p'));
            
            // Date1
            Date          myDate          = Property.SDF.parse("2018-12-24 23:00:00");
            LocalDateTime myLocalDateTime = LocalDateTime.parse("2018-12-24 23:00:00", Property.FORMATTER);
            Instant       myInstant       = myLocalDateTime.toInstant(Property.ZONE);
            Calendar      myCal           = new GregorianCalendar();
            myCal.setTime(myDate);
            // Date2
            Date          myDate2          = Property.SDF.parse("2019-01-01 23:00:00");
            LocalDateTime myLocalDateTime2 = LocalDateTime.parse("2019-01-01 23:00:00", Property.FORMATTER);
            Instant       myInstant2       = myLocalDateTime.toInstant(Property.ZONE);
            Calendar      myCal2           = new GregorianCalendar();
            myCal.setTime(myDate2);
            
            r.put(PCalendar,        new PropertyCalendar(PCalendar, myCal));
            r.put(PDate,            new PropertyDate(PDate, myDate));
            r.put(PClass,           new PropertyClass(PClass, String.class));
            r.put(PDouble,          new PropertyDouble(PDouble, new Double(20.0)));
            r.put(PFloat,           new PropertyFloat(PFloat, new Float(20.0)));
            r.put(PInstant,         new PropertyInstant(PInstant, myInstant));
            r.put(PInt,             new PropertyInt(PInt, 10));
            r.put(PLocal,           new PropertyLocalDateTime(PLocal, myLocalDateTime));
            r.put(PLogLevel,        new PropertyLogLevel(PLogLevel, LogLevel.INFO));
            r.put(PLong,            new PropertyLong(PLong, 123L));
            r.put(PShort,           new PropertyShort(PShort, (short) 5));
            r.put(PString,          new PropertyString(PString, "pString"));
            
            r.put(PListBigDecimal,  new PropertyListBigDecimal(PListBigDecimal, new BigDecimal(1.5),  new BigDecimal(2.5)));
            r.put(PListBigInteger,  new PropertyListBigInteger(PListBigInteger, new BigInteger("123456"),  new BigInteger("789012")));
            r.put(PListBoolean,     new PropertyListBoolean(PListBoolean, true, false));
            r.put(PListByte,        new PropertyListByte(PListByte, (byte) 'p', (byte) 'l'));
            r.put(PListCalendar,    new PropertyListCalendar(PListCalendar, myCal, myCal2));
            r.put(PListDate,        new PropertyListDate(PListDate, myDate, myDate2));
            r.put(PListClass,       new PropertyListClass(PListClass, String.class, Integer.class));
            r.put(PListDouble,      new PropertyListDouble(PListDouble, new Double(20.0), new Double(30.0) ));
            r.put(PListFloat,       new PropertyListFloat(PListFloat, new Float(20.0), new Float(30.0)));
            r.put(PListInstant,     new PropertyListInstant(PListInstant, myInstant, myInstant2));
            r.put(PListInt,         new PropertyListInt(PListInt, 10, 20));
            r.put(PListLocal,       new PropertyListLocalDateTime(PListLocal, myLocalDateTime, myLocalDateTime2));
            r.put(PListLogLevel,    new PropertyListLogLevel(PListLogLevel, LogLevel.INFO, LogLevel.DEBUG));
            r.put(PListLong,        new PropertyListLong(PListLong, 123L, 456L));
            r.put(PListShort,       new PropertyListShort(PListShort, (short) 5, (short) 6));
            r.put(PListString,      new PropertyListString(PListString, "pString", "listString"));
            
        } catch (ParseException e) {}
        return r;
    }
    
}
