package org.ff4j.backend;

import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureFlagNotFoundException;
import org.ff4j.property.*;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.exception.InvalidPropertyTypeException;
import org.ff4j.property.exception.PropertyNotFoundException;
import org.ff4j.property.list.*;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Duration;
import java.time.Instant;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Stream;

/**
 * Public methods for FF4j without accessing the BackendAdminOperation.
 */
public interface BackendRepositoryReadOnly {

    /**
     * Default workspace.
     */
    String DEFAULT_WORKSPACE = "default";

    /**
     * Default Workspace.
     *
     * @return
     *      selected workspace
     */
    default String getCurrentWorkspace() {
        return DEFAULT_WORKSPACE;
    }

    /**
     * List available workspace.
     *
     * @return workspace identifiers
     */
    Stream<String> getWorkspaces();

    /**
     * List of feature status for a workspace with a context.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @return status for features
     */
    Map<String, Boolean> getFeatureFlags(String workspace, FF4jEvaluationContext context);

    /**
     * List of feature status for a workspace with a context.
     *
     * @return status for features
     */
    default Map<String, Boolean> getFeatureFlags() {
        return getFeatureFlags((FF4jEvaluationContext) null);
    }

    /**
     * List of feature status for a workspace with a context.
     *
     * @param workspace
     *         current workspace
     * @return status for features
     */
    default Map<String, Boolean> getFeatureFlags(String workspace) {
        return getFeatureFlags(workspace, null);
    }

    /**
     * List of feature status for a workspace with a context.
     *
     * @return status for features
     */
    default Map<String, Boolean> getFeatureFlags(FF4jEvaluationContext context) {
        return getFeatureFlags(getCurrentWorkspace(), context);
    }

    /**
     * Find a feature from its id.
     *
     * @param workspace
     *         workspace identifier
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    Optional<Boolean> findFeatureFlag(String workspace, String uid, FF4jEvaluationContext context);

    /**
     * Find a feature from its id.
     *
     * @param uid
     *         feature identifier
     * @return feature object if exist
     */
    default Optional<Boolean> findFeatureFlag(String uid, FF4jEvaluationContext context) {
        return findFeatureFlag(getCurrentWorkspace(), uid, context);
    }

    // -------------------------------------
    // ---- Work with Properties -----------
    // -------------------------------------

    /**
     * List all properties in a workspace.
     *
     * @param workspace
     *         workspace name
     * @return all properties
     */
    Stream<Property<?>> getProperties(String workspace);

    /**
     * List all properties in a workspace.
     *
     * @return all properties
     */
    default Stream<Property<?>> getProperties() {
        return getProperties(getCurrentWorkspace());
    }

    /**
     * Get a property.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property has not been found
     */
    Optional<Property<?>> findProperty(String workspace, String uid);

    /**
     * Get a property.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property has not been found
     */
    default Optional<Property<?>> findProperty(String uid) {
        return findProperty(getCurrentWorkspace(), uid);
    }

    /**
     * Find a property from its id.
     *
     * @param workspace workspace identifier
     * @param uid       feature identifier
     * @return feature object if exist
     */
    default Property<?> getProperty(String workspace, String uid)
    throws PropertyNotFoundException {
        return findProperty(workspace, uid).orElseThrow(() -> new PropertyNotFoundException(workspace, uid));
    }

    /**
     * Find a property from its id.
     *
     * @param uid       feature identifier
     * @return feature object if exist
     */
    default Property<?> getProperty(String uid)
    throws PropertyNotFoundException {
        return findProperty(uid).orElseThrow(() -> new PropertyNotFoundException(getCurrentWorkspace(), uid));
    }

    // -------------------------------------
    // ----------- BIG DECIMAL -------------
    // -------------------------------------

    /**
     * Access a BigDecimal property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigDecimal getBigDecimal(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimal(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a BigDecimal property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigDecimal getBigDecimal(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimal(workspace, uid, null);
    }

    /**
     * Access a BigDecimal property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigDecimal getBigDecimal(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimal(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a BigDecimal property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigDecimal getBigDecimal(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyBigDecimal p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ------- BIG DECIMAL LIST-------------
    // -------------------------------------

    /**
     * Access a BigDecimal property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigDecimal> getBigDecimalList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimalList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a BigDecimal property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigDecimal> getBigDecimalList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimalList(workspace, uid, null);
    }

    /**
     * Access a BigDecimal property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigDecimal> getBigDecimalList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigDecimalList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a BigDecimal property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigDecimal> getBigDecimalList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListBigDecimal p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ----------- BIG INTEGER -------------
    // -------------------------------------

    /**
     * Access a BigInteger property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigInteger getBigInteger(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigInteger(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a BigInteger property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigInteger getBigInteger(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigInteger(workspace, uid, null);
    }

    /**
     * Access a BigInteger property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigInteger getBigInteger(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigInteger(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a BigDecimal property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default BigInteger getBigInteger(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyBigInteger p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ------- BIG INTEGER LIST-------------
    // -------------------------------------

    /**
     * Access a BigInteger property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigInteger> getBigIntegerList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigIntegerList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a BigInteger property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigInteger> getBigIntegerList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigIntegerList(workspace, uid, null);
    }

    /**
     * Access a BigInteger property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigInteger> getBigIntegerList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBigIntegerList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a BigInteger property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<BigInteger> getBigIntegerList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListBigInteger p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ----- BOOLEAN/BOOLEAN LIST ----------
    // -------------------------------------

    /**
     * Access a Boolean property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Boolean getBoolean(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBoolean(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Boolean property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Boolean getBoolean(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBoolean(workspace, uid, null);
    }

    /**
     * Access a Boolean property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Boolean getBoolean(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBoolean(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Boolean property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Boolean getBoolean(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyBoolean p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Boolean property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Boolean> getBooleanList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBooleanList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Boolean property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Boolean> getBooleanList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBooleanList(workspace, uid, null);
    }

    /**
     * Access a Boolean property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Boolean> getBooleanList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBooleanList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Boolean property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Boolean> getBooleanList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListBoolean p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --------- BYTE /  BYTE LIST ---------
    // -------------------------------------

    /**
     * Access a Byte property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Boolean getByte(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getBoolean(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Byte property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Byte getByte(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getByte(workspace, uid, null);
    }

    /**
     * Access a Byte property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Byte getByte(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getByte(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Byte property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Byte getByte(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyByte p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Byte property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Byte> getByteList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getByteList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Byte property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Byte> getByteList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getByteList(workspace, uid, null);
    }

    /**
     * Access a Byte property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Byte> getByteList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getByteList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Byte property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Byte> getByteList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListByte p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --- CALENDAR, CALENDAR LIST ---------
    // -------------------------------------

    /**
     * Access a Calendar property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Calendar getCalendar(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendar(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Calendar property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Calendar getCalendar(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendar(workspace, uid, null);
    }

    /**
     * Access a Calendar property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Calendar getCalendar(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendar(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Calendar property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Calendar getCalendar(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyCalendar p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Calendar property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Calendar> getCalendarList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendarList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Calendar property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Calendar> getCalendarList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendarList(workspace, uid, null);
    }

    /**
     * Access a Calendar property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Calendar> getCalendarList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getCalendarList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Calendar property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Calendar> getCalendarList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListCalendar p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // -------- CLASS, CLASS LIST ----------
    // -------------------------------------

    /**
     * Access a Class<?> property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Class<?> getClass(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClass(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Class<?> property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Class<?> getClass(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClass(workspace, uid, null);
    }

    /**
     * Access a Class<?> property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Class<?> getClass(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClass(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Class<?> property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Class<?> getClass(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyClass p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Class<?> property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Class<?>> getClassList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClassList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Class<?> property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Class<?>> getClassList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClassList(workspace, uid, null);
    }

    /**
     * Access a Class<?>  property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Class<?>> getClassList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getClassList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Class<?> property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Class<?>> getClassList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListClass p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --------- DATE, DATE LIST -----------
    // -------------------------------------

    /**
     * Access a Date property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Date getDate(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDate(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Date property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Date getDate(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDate(workspace, uid, null);
    }

    /**
     * Access a Date property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Date getDate(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDate(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Date property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Date getDate(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyDate p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Date property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Date> getDateList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDateList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Date property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Date> getDateList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDateList(workspace, uid, null);
    }

    /**
     * Access a Date property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Date> getDateList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDateList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Date property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Date> getDateList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListDate p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ----- STRING, STRING LIST -----------
    // -------------------------------------

    /**
     * Access a String property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default String getString(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getString(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a String property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default String getString(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getString(workspace, uid, null);
    }

    /**
     * Access a String property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default String getString(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getString(DEFAULT_WORKSPACE, uid, context);
    }

    /**
     * Access a String property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default String getString(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyString p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a String property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<String> getStringList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getStringList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a String property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<String> getStringList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getStringList(workspace, uid, null);
    }

    /**
     * Access a String property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<String> getStringList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getStringList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a String property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<String> getStringList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListString p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ----- DOUBLE, DOUBLE LIST -----------
    // -------------------------------------

    /**
     * Access a Double property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Double getDouble(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDouble(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Double property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Double getDouble(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDouble(workspace, uid, null);
    }

    /**
     * Access a Double property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Double getDouble(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDouble(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Double property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Double getDouble(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyDouble p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Double property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Double> getDoubleList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDoubleList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Double property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Double> getDoubleList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDoubleList(workspace, uid, null);
    }

    /**
     * Access a Double property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Double> getDoubleList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDoubleList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Double property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Double> getDoubleList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListDouble p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --- DURATION, DURATION LIST ---------
    // -------------------------------------

    /**
     * Access a Duration property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Duration getDuration(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDuration(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Double property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Duration getDuration(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDuration(workspace, uid, null);
    }

    /**
     * Access a Duration property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Duration getDuration(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDuration(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Duration property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Duration getDuration(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyDuration p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Duration property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Duration> getDurationList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDurationList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Duration property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Duration> getDurationList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDurationList(workspace, uid, null);
    }

    /**
     * Access a Duration property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Duration> getDurationList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getDurationList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Duration property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Duration> getDurationList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListDuration p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --------- FLOAT, FLOAT LIST ---------
    // -------------------------------------

    /**
     * Access a Float property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Float getFloat(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloat(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Float property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Float getFloat(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloat(workspace, uid, null);
    }

    /**
     * Access a Float property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Float getFloat(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloat(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Float property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Float getFloat(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyFloat p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Float property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Float> getFloatList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloatList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Float property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Float> getFloatList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloatList(workspace, uid, null);
    }

    /**
     * Access a Float property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Float> getFloatList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getFloatList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Float property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Float> getFloatList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListFloat p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ------ INSTANT, INSTANT LIST --------
    // -------------------------------------

    /**
     * Access an Instant property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Instant getInstant(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstant(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access an Instant property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Instant getInstant(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstant(workspace, uid, null);
    }

    /**
     * Access an Instant property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Instant getInstant(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstant(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access an Instant property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Instant getInstant(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyInstant p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Float property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Instant> getInstantList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstantList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Float property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Instant> getInstantList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstantList(workspace, uid, null);
    }

    /**
     * Access an Instant property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Instant> getInstantList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getInstantList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access an Instant property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Instant> getInstantList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListInstant p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --- LOCAL_DATE, LOCAL_DATE LIST -----
    // -------------------------------------

    /**
     * Access a LocalDate property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDate getLocalDate(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDate(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LocalDate property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDate getLocalDate(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDate(workspace, uid, null);
    }

    /**
     * Access a LocalDate property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDate getLocalDate(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDate(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LocalDate property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDate getLocalDate(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyLocalDate p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a LocalDate property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDate> getLocalDateList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LocalDate property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDate> getLocalDateList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateList(workspace, uid, null);
    }

    /**
     * Access a LocalDate property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDate> getLocalDateList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LocalDate property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDate> getLocalDateList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException{
        if (getProperty(workspace, uid) instanceof PropertyListLocalDate p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // - LocalDateTime, LocalDateTime LIST -
    // -------------------------------------

    /**
     * Access a LocalDateTime property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDateTime getLocalDateTime(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTime(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LocalDateTime property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDateTime getLocalDateTime(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTime(workspace, uid, null);
    }

    /**
     * Access a LocalDateTime property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDateTime getLocalDateTime(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTime(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LocalDateTime property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default LocalDateTime getLocalDateTime(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException{
        if (getProperty(workspace, uid) instanceof PropertyLocalDateTime p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a LocalDateTime property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDateTime> getLocalDateTimeList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTimeList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LocalDateTime property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDateTime> getLocalDateTimeList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTimeList(workspace, uid, null);
    }

    /**
     * Access a LocalDateTime property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDateTime> getLocalDateTimeList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLocalDateTimeList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LocalDateTime property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<LocalDateTime> getLocalDateTimeList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListLocalDateTime p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ------ LogLevel, LogLevel LIST -------
    // -------------------------------------

    /**
     * Access a Float property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default PropertyLogLevel.LogLevel getLogLevel(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevel(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LogLevel property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default PropertyLogLevel.LogLevel getLogLevel(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevel(workspace, uid, null);
    }

    /**
     * Access a LogLevel property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default PropertyLogLevel.LogLevel getLogLevel(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevel(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LogLevel property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default PropertyLogLevel.LogLevel getLogLevel(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyLogLevel p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a LogLevel property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<PropertyLogLevel.LogLevel> getLogLevelList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevelList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a LogLevel property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<PropertyLogLevel.LogLevel> getLogLevelList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevelList(workspace, uid, null);
    }

    /**
     * Access a LogLevel property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<PropertyLogLevel.LogLevel> getLogLevelList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLogLevelList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a LogLevel property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<PropertyLogLevel.LogLevel> getLogLevelList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListLogLevel p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // --------- Long, Long LIST -----------
    // -------------------------------------

    /**
     * Access a Long property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Long getLong(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLong(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Long property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Long getLong(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLong(workspace, uid, null);
    }

    /**
     * Access a Long property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Long getLong(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLong(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Long property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Long getLong(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyLong p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Float property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Long> getLongList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLongList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Long property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Long> getLongList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLongList(workspace, uid, null);
    }

    /**
     * Access a Long property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Long> getLongList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getLongList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Long property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Long> getLongList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListLong p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    // -------------------------------------
    // ------- Short, Short List -----------
    // -------------------------------------

    /**
     * Access a Short property in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Short getShort(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShort(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Short property in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Short getShort(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShort(workspace, uid, null);
    }

    /**
     * Access a Short property in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Short getShort(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShort(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Short property in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default Short getShort(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyShort p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

    /**
     * Access a Short property list in the store.
     *
     * @param uid
     *         property identifier
     * @return property value
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Short> getShortList(String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShortList(uid, (FF4jEvaluationContext) null);
    }

    /**
     * Access a Short property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Short> getShortList(String workspace, String uid)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShortList(workspace, uid, null);
    }

    /**
     * Access a Short property list in the store.
     *
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Short> getShortList(String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        return getShortList(getCurrentWorkspace(), uid, context);
    }

    /**
     * Access a Short property list in the store.
     *
     * @param workspace
     *         current workspace
     * @param context
     *         current context
     * @param uid
     *         current property
     * @return value for the property
     * @throws PropertyNotFoundException
     *         property did not exist
     * @throws InvalidPropertyTypeException
     *         property was not  big decimal
     */
    default List<Short> getShortList(String workspace, String uid, FF4jEvaluationContext context)
    throws PropertyNotFoundException, InvalidPropertyTypeException {
        if (getProperty(workspace, uid) instanceof PropertyListShort p) return p.getValue();
        throw new InvalidPropertyTypeException(uid, workspace, String.class, getProperty(workspace, uid).getValue().getClass());
    }

}
