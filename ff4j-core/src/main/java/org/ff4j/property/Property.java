package org.ff4j.property;

import org.ff4j.backend.Backend;
import org.ff4j.property.evaluate.AbstractEvaluationPolicy;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.evaluate.FF4jEvaluationPolicy;
import org.ff4j.property.list.*;
import org.ff4j.property.serialize.Serializer;
import org.ff4j.utils.Assert;

import java.time.Instant;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import static org.ff4j.utils.Assert.assertHasLength;

/**
 * Abstraction of Property.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class Property<T> implements Comparable<Property<T>>, Cloneable {

    /** Every entity will have a namespace, if not using default. */
    public static final String DEFAULT_NAMESPACE = "default";

    /** Entity creation date. */
    protected Instant creationDate = Instant.now();

    /** Last Modified value. */
    protected Instant lastModified;

    /** User who created the object. */
    protected String createdBy;

    /** User who edited the object. */
    protected String lastModifiedBy;

    /** unique identifier. */
    protected String uid;

    /** Short text to describe the entity. */
    protected String description;

    /** Namespace associating a feature to an application. */
    protected String namespace = DEFAULT_NAMESPACE;

    /** Set of tags. */
    protected Set <String> tags = new HashSet<>();

    /** Current Value. */
    protected T value;

    /** If value have a limited set of values. */
    protected Set<T> fixedValues = null;

    /** Custom evaluation, introducing code compute the property value. */
    protected FF4jEvaluationPolicy<T> evaluationStrategy;
    
    /**
     * Constructor by property name.
     *
     * @param uid
     *            unique property identifier
     */
    protected Property(String uid) {
        Assert.assertHasLength(uid);
        this.uid = uid;
        this.lastModified = creationDate;
        Backend.getContext().getUser().ifPresent(user -> {
            this.createdBy = user.getName();
            this.lastModifiedBy = user.getName();
        });
    }
    
    /**
     * Constructor with name and value as String.
     *
     * @param name
     *            current name
     * @param value
     *            current value
     */
    public Property(String name, String value) {
        this(name);
        this.value = getSerializer().deserialize(value);
    }

    /**
     * Constructor with name and value as String.
     *
     * @param name
     *            current name
     * @param value
     *            current value
     */
    public Property(String name, T value) {
        this(name);
        this.value = value;
    }

    /**
     * Serializer to work with Strings
     *
     * @return serializer
     */
    public abstract Serializer<T> getSerializer();

    /**
     * Cloning the property.
     *
     * @return
     *      clone
     */
    public Property<T> clone() throws CloneNotSupportedException {
        super.clone();
        return clone(this.getUid());
    }

    /**
     * Clone with a new name
     *
     * @return
     *      clone
     */
    public Property<T> clone(String newName) {
        Property<T> to = create(this.getClass().getName(), newName, this.getValueAsString());
        to.creationDate   = this.creationDate;
        to.createdBy      = this.createdBy;
        to.description    = this.description;
        to.lastModified   = this.lastModified;
        to.lastModifiedBy = this.lastModifiedBy;
        to.namespace      = this.getNamespace();
        to.tags           = this.tags;
        if (getFixedValues() != null) getFixedValuesAsString().forEach(to::addFixedValueString);
        if (getEvaluationStrategy() != null) {
            AbstractEvaluationPolicy<?,?> strategy = (AbstractEvaluationPolicy<?,?> ) this.evaluationStrategy;
            to.evaluationStrategy = FF4jEvaluationPolicy.getEvaluationPolicy(
                    strategy.getClass().getName(),
                    strategy.getBackend(), this,
                    strategy.getConfig());
        }
        return to;
    }

    /**
     * Factory method to create property.
     *
     * @param className
     *            current class
     * @param uid
     *            property type
     * @param value
     *            property values\
     * @return
     *      new property
     */
    public static <P> Property<P> create(String className, String uid, String value) {
        assertHasLength(className);
        assertHasLength(uid);
        try {
            return (Property<P>) Class.forName(className)
                    .getConstructor(String.class, String.class)
                    .newInstance(uid, value);
        } catch (Exception e) {
            throw new IllegalArgumentException("Cannot instantiate '" + className
                    + "' check default constructor : " + e.getMessage(), e);
        }
    }

    /** {@inheritDoc} */
    public T getValue() {
        return getValue(new FF4jEvaluationContext());
    }

    /** {@inheritDoc} */
    public T getValue(FF4jEvaluationContext context) {
        return (evaluationStrategy == null) ? value : evaluationStrategy.evaluate(context);
    }

    /**
     * Serialized value as String
     *
     * @return current value as a string or null
     */
    public String getValueAsString() {
        return getValueAsString(new FF4jEvaluationContext());
    }

    /**
     * Serialized value as String
     *
     * @return current value as a string or null
     */
    public String getValueAsString(FF4jEvaluationContext context) {
        T value = getValue(context);
        return (value == null) ? null : getSerializer().serialize(value);
    }

    /**
     * Map Short Name to class name
     * @param shortName
     *      host name
     * @return
     *      class name or empty
     */
    public static Optional<String> getClassNameFromShortName(String shortName) {
        //return "org.ff4j.Property" + shortName.substring(0, 1).toUpperCase() + shortName.substring(1);
        return switch(shortName) {
            case "bigDecimal"     -> Optional.of(PropertyBigDecimal.class.getName());
            case "bigInteger"     -> Optional.of(PropertyBigInteger.class.getName());
            case "boolean"        -> Optional.of(PropertyBoolean.class.getName());
            case "byte"           -> Optional.of(PropertyByte.class.getName());
            case "calendar"       -> Optional.of(PropertyCalendar.class.getName());
            case "class"          -> Optional.of(PropertyClass.class.getName());
            case "date"           -> Optional.of(PropertyDate.class.getName());
            case "double"         -> Optional.of(PropertyDouble.class.getName());
            case "duration"       -> Optional.of(PropertyDuration.class.getName());
            case "float"          -> Optional.of(PropertyFloat.class.getName());
            case "instant"        -> Optional.of(PropertyInstant.class.getName());
            case "integer"        -> Optional.of(PropertyInteger.class.getName());
            case "localDate"      -> Optional.of(PropertyLocalDate.class.getName());
            case "localDateTime"  -> Optional.of(PropertyLocalDateTime.class.getName());
            case "logLevel"       -> Optional.of(PropertyLogLevel.class.getName());
            case "long"           -> Optional.of(PropertyLong.class.getName());
            case "short"          -> Optional.of(PropertyShort.class.getName());
            case "string"         -> Optional.of(PropertyString.class.getName());

            case "listBigDecimal" -> Optional.of(PropertyListBigDecimal.class.getName());
            case "listBigInteger" -> Optional.of(PropertyListBigInteger.class.getName());
            case "listBoolean"    -> Optional.of(PropertyListBoolean.class.getName());
            case "listByte"       -> Optional.of(PropertyListByte.class.getName());
            case "listCalendar"   -> Optional.of(PropertyListCalendar.class.getName());
            default -> Optional.empty();
        };
    }

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getClassName() {
        return getClass().getName();
    }

    /**
     * Gets creationDate
     *
     * @return value of creationDate
     */
    public Instant getCreationDate() {
        return creationDate;
    }

    /**
     * Set value for creationDate
     *
     * @param creationDate new value for creationDate
     */
    public void setCreationDate(Instant creationDate) {
        this.creationDate = creationDate;
    }

    /**
     * Gets lastModified
     *
     * @return value of lastModified
     */
    public Instant getLastModified() {
        return lastModified;
    }

    /**
     * Set value for lastModified
     *
     * @param lastModified new value for lastModified
     */
    public void setLastModified(Instant lastModified) {
        this.lastModified = lastModified;
    }

    /**
     * Gets uid
     *
     * @return value of uid
     */
    public String getUid() {
        return uid;
    }

    /**
     * Set value for uid
     *
     * @param uid new value for uid
     */
    public void setUid(String uid) {
        this.uid = uid;
    }

    /**
     * Gets description
     *
     * @return value of description
     */
    public Optional<String> getDescription() {
        return Optional.ofNullable(description);
    }

    /**
     * Set value for description
     *
     * @param description new value for description
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Gets tags
     *
     * @return value of tags
     */
    public Set<String> getTags() {
        return tags;
    }

    /**
     * Set value for tags
     *
     * @param tags new value for tags
     */
    public void setTags(Set<String> tags) {
        this.tags = tags;
    }

    /**
     * Gets createdBy
     *
     * @return value of createdBy
     */
    public String getCreatedBy() {
        return createdBy;
    }

    /**
     * Set value for createdBy
     *
     * @param createdBy new value for createdBy
     */
    public void setCreatedBy(String createdBy) {
        this.createdBy = createdBy;
    }

    /**
     * Gets lastModifiedBy
     *
     * @return value of lastModifiedBy
     */
    public String getLastModifiedBy() {
        return lastModifiedBy;
    }

    /**
     * Set value for lastModifiedBy
     *
     * @param lastModifiedBy new value for lastModifiedBy
     */
    public void setLastModifiedBy(String lastModifiedBy) {
        this.lastModifiedBy = lastModifiedBy;
    }

    /**
     * Gets namespace
     *
     * @return value of namespace
     */
    public String getNamespace() {
        return namespace;
    }

    /**
     * Set value for namespace
     *
     * @param namespace new value for namespace
     */
    public void setNamespace(String namespace) {
        this.namespace = namespace;
    }

    /**
     * Getter accessor for attribute 'fixedValues'.
     *
     * @return current value of 'fixedValues'
     */
    public Set<T> getFixedValues() {
        return fixedValues;
    }

    /**
     * Setter accessor for attribute 'value'.
     * 
     * @param value
     *            new value for 'value '
     */
    public Property<T> setValue(T value) {
        this.value = value;
        validateValue();
        return this;
    }

    /**
     * Update all fixed values.
     *
     * @param perms
     *      all fixed values.
     * @return
     *      current object
     */
    public Property<T> setFixedValues(Set<T> perms) {
        fixedValues = perms;
        validateValue();
        return this;
    }

    /**
     * Update all fixed values.
     *
     * @param value
     *      new fixed values.
     * @return
     *      current object
     */
    public Property<T> addFixedValue(T value) {
        if (fixedValues == null) fixedValues = new HashSet<>();
        fixedValues.add(value);
        return this;
    }

    /**
     * Update all fixed values.
     *
     * @param value
     *      new fixed values.
     * @return
     *      current object
     */
    public Property<T> addFixedValueString(String value) {
        return addFixedValue(getSerializer().deserialize(value));
    }

    /**
     * Retrieve fixed values as String.
     *
     * @return
     *      fixed values as string
     */
    public Set<String> getFixedValuesAsString() {
        if (fixedValues == null) return null;
        return fixedValues.stream().map(this.getSerializer()::serialize).collect(Collectors.toSet());
    }

    /**
     * Update all fixed values.
     *
     * @param perms
     *      all fixed values.
     * @return
     *      current object
     */
    @SuppressWarnings("unchecked")
    public Property<T> setFixedValues(T... perms) {
        setFixedValues(Set.of(perms));
        return this;
    }

    /**
     * Gets evaluationStrategy
     *
     * @return value of evaluationStrategy
     */
    public FF4jEvaluationPolicy<T> getEvaluationStrategy() {
        return evaluationStrategy;
    }

    /**
     * Set value for evaluationStrategy
     *
     * @param evaluationStrategy new value for evaluationStrategy
     */
    public void setEvaluationStrategy(FF4jEvaluationPolicy<T> evaluationStrategy) {
        this.evaluationStrategy = evaluationStrategy;
    }

    /**
     * Validate that current value is part of fixed Values.
     */
    private void validateValue() {
        if (fixedValues!= null && !fixedValues.contains(value)) {
            throw new IllegalArgumentException("Cannot update property <" + getUid() + "> invalid value <"
                    + getValue() + "> expected one of " + fixedValues);
        }
    }

    /**
     * Compare 2 properties.
     *
     * @param p
     *      current property.
     * @return
     *      comparison
     */
    @Override
    public int compareTo(Property<T> p) {
        return getUid().compareTo(p.getUid());
    }

    
}
