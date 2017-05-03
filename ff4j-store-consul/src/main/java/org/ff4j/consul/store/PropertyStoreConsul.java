package org.ff4j.consul.store;

import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.ff4j.consul.ConsulConnection;
import org.ff4j.consul.ConsulConstants;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.store.kv.KeyValuePropertyStore;
import org.ff4j.utils.json.PropertyJsonParser;

/**
 * Generic {@link PropertyStore} to persist properties in a JCache (JSR107) compliant storage.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreConsul extends KeyValuePropertyStore< ConsulConnection > {
    
    /**
     * Default contructor.
     */
    public PropertyStoreConsul() {
        super();
    }
    
    /**
     * Initialization with cache manager.
     *
     * @param cacheManager
     */
    public PropertyStoreConsul(ConsulConnection connection) {
        super(connection);
    }
    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return PropertyJsonParser.parseProperty(getDriver().getValue(
                        getDriver().getPropertyKey(name)));
    }

    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        Property<?> fp = readProperty(name);
        fp.setValueFromString(newValue);
        updateProperty(fp);
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null");
        }
        if (!existProperty(property.getName())) {
            throw new PropertyNotFoundException(property.getName());
        }
        driver.putValue(getDriver()
              .getPropertyKey(property.getName()), property.toJson());
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        driver.deleteKey(driver.getPropertyKey(name));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return getDriver().getKeyValueClient()
                .getValuesAsString(ConsulConstants.FF4J_PREFIXKEY_PROPERTIES)
                .stream()
                .map(PropertyJsonParser::parseProperty)
                .collect(Collectors.toMap(Property::getName, Function.identity()));
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return getDriver().getKeyValueClient()
                          .getKeys(ConsulConstants.FF4J_PREFIXKEY_PROPERTIES)
                          .stream()
                          .map(driver::getPropertyName)
                          .collect(Collectors.toSet());
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        getDriver().getKeyValueClient()
                .deleteKeys(ConsulConstants.FF4J_PREFIXKEY_PROPERTIES);
    }

}
