package org.ff4j.store.kv;

import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

/**
 * Property store to work with key/value.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class KeyValuePropertyStore< D extends KeyValueDriver >  extends AbstractPropertyStore {

    /** Driver to access a K/V Store. */
    protected D driver;
    
    /**
     * Default constructor
     */
    public KeyValuePropertyStore() {
    }
            
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValuePropertyStore(D driver) {
        this.driver = driver;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "Property name");
        return getDriver().existKey(getDriver().getPropertyKey(name));
    }
    
    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        getDriver().putValue(getDriver()
                .getPropertyKey(property.getName()), property.toJson());
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void deleteProperty(String name) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, Property<?>> readAllProperties() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Set<String> listPropertyNames() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void clear() {
       
    }

    /**
     * Getter accessor for attribute 'driver'.
     *
     * @return
     *       current value of 'driver'
     */
    public D getDriver() {
        if (driver == null) {
            throw new IllegalStateException("Cannot access target K/V store");
        }
        return driver;
    }

    /**
     * Setter accessor for attribute 'driver'.
     * @param driver
     *      new value for 'driver '
     */
    public void setDriver(D driver) {
        this.driver = driver;
    }
        

}
