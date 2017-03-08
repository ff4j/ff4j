package org.ff4j.commonsconf;

/*
 * #%L
 * ff4j-archaius
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


import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.apache.commons.configuration.AbstractConfiguration;
import org.apache.commons.configuration.Configuration;
import org.ff4j.exception.InvalidPropertyTypeException;
import org.ff4j.property.Property;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;

/**
 * Bridge from {@link PropertyStore} to commons configuration {@link Configuration}.
 * 
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jConfiguration extends AbstractConfiguration {
    
    /** Target delimiter for multivalued. */
    private static final String DELIMITER = ",";
    
    /**
     * Source
     */
    private PropertyStore ff4jStore;
    
    /**
     * Default constructor.
     */
    public FF4jConfiguration() {
    }
            
    /**
     * Initialized with default value.
     *
     * @param ff4jPropertyStore
     *      default store.
     */
    public FF4jConfiguration(PropertyStore ff4jPropertyStore) {
        this.ff4jStore = ff4jPropertyStore;
    }
    
    /** {@inheritDoc} */
    @Override
    public Configuration subset(String prefix) {
        Map < String, Property<?>> myProps = ff4jStore().readAllProperties();
        PropertyStore ps = new InMemoryPropertyStore();
        for (Map.Entry< String, Property<?>>  prop : myProps.entrySet()) {
            if (prop.getKey().startsWith(prefix)) {
                ps.createProperty(prop.getValue());
            }
        }
        return new FF4jConfiguration(ps);
    }  
    
    /** {@inheritDoc} */
    @Override
    public Properties getProperties(String key) {
        Properties props = new Properties();
        if (key == null) return props;
        Map < String, Property<?>> myProps = ff4jStore().readAllProperties();
        for (Map.Entry< String, Property<?>>  prop : myProps.entrySet()) {
            if (prop.getKey().startsWith(key)) {
                props.put(prop.getKey(), prop.getValue().getValue());
            }
        }
        return props;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        return ff4jStore().isEmpty();
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean containsKey(String key) {
        if (key == null) return false;
        return ff4jStore().existProperty(key);
    }
    
    /** {@inheritDoc} */
    @Override
    public void addProperty(String key, Object value) {
        ff4jStore().createProperty(PropertyFactory.createProperty(key, value));
    }
    
    /** {@inheritDoc} */
    @Override
    protected void addPropertyDirect(String key, Object value) {
        ff4jStore().createProperty(PropertyFactory.createProperty(key, value));
    }
    
    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    @Override
    public void setProperty(String key, Object value) {
        if (!ff4jStore.existProperty(key)) {
            addProperty(key, value);
        }
        Property<Object> ap = (Property<Object>) ff4jStore().readProperty(key);
        ap.setValue(String.valueOf(value));
        ff4jStore().updateProperty(ap);
    }
    
    /** {@inheritDoc} */
    @Override
    public void clearProperty(String key) {
        ff4jStore().deleteProperty(key);
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        ff4jStore().clear();
    }

    /** {@inheritDoc} */
    @Override
    public Object getProperty(String key) {
        return ff4jStore().readProperty(key).getValue();
    }

    /** {@inheritDoc} */
    @Override
    public Iterator<String> getKeys(String prefix) {
        Set< String > setOfNames = ff4jStore().listPropertyNames();
        Set< String > results = new HashSet<String>();
        if (setOfNames != null && setOfNames.size() > 0) {
            for (String name : setOfNames) {
                if (name.startsWith(prefix)) {
                    results.add(name);
                }
            }
        }
        return results.iterator();
    }
    
    /**
     * The read will raise PropertyNotFound if not exist, null could be cast in any type.
     * 
     * @param key
     *      current property name
     * @return
     *      property value
     */
    private Object getValue(String key) {
        Util.assertHasLength(key);
        return ff4jStore().readProperty(key).asString();
    }

    /** {@inheritDoc} */
    @Override
    public Iterator<String> getKeys() {
        Set< String > init = new HashSet<String>();
        Set < String > result = ff4jStore().listPropertyNames();
        return (result == null) ? init.iterator() : result.iterator();
    }    
    
    /**
     * {@inheritDoc}
     *
     * Use the object : 
     * <li> To handle 'null' and avoid NPE on unboxing
     * <li> To raise classCast if invalid property type...
     */
    @Override
    public boolean getBoolean(String key) {
        String value = (String) getValue(key);
        if (!Boolean.TRUE.toString().equals(value.toLowerCase()) &&
            !Boolean.FALSE.toString().equals(value.toLowerCase()) ) {
            throw new InvalidPropertyTypeException("Cannot create Boolean from " + value);
        }
        return new Boolean(value).booleanValue();
    }
    
    /** {@inheritDoc} */
    @Override
    public Boolean getBoolean(String key, Boolean defaultValue) {
        return ff4jStore().existProperty(key) ? getBoolean(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public boolean getBoolean(String key, boolean defaultValue) {
        return ff4jStore().existProperty(key) ? getBoolean(key) : defaultValue;
    }    

    /** {@inheritDoc} */
    @Override
    public byte getByte(String key) {
        String value = null;
        try {
            value = (String) getValue(key);
            return Byte.parseByte(value);
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Byte from " + value, nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public byte getByte(String key, byte defaultValue) {
        return ff4jStore().existProperty(key) ? getByte(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public Byte getByte(String key, Byte defaultValue) {
        return ff4jStore().existProperty(key) ? getByte(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public double getDouble(String key) {
        try {
            return new Double((String) getValue(key)).doubleValue();
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Double from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public double getDouble(String key, double defaultValue) {
        return ff4jStore().existProperty(key) ? getDouble(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public Double getDouble(String key, Double defaultValue) {
        return ff4jStore().existProperty(key) ? getDouble(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public float getFloat(String key) {
        try {
            return new Float((String) getValue(key)).floatValue();
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Float from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public float getFloat(String key, float defaultValue) {
        return ff4jStore().existProperty(key) ? getFloat(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public Float getFloat(String key, Float defaultValue) {
        return ff4jStore().existProperty(key) ? getFloat(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public int getInt(String key) {
        try {
            return new Integer((String) getValue(key)).intValue();
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Integer from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public int getInt(String key, int defaultValue) {
        return ff4jStore().existProperty(key) ? getInt(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public Integer getInteger(String key, Integer defaultValue) {
        return ff4jStore().existProperty(key) ? getInt(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public long getLong(String key) {
        try {
            return new Long((String) getValue(key)).longValue();
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Long from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Long getLong(String key, Long defaultValue) {
        return ff4jStore().existProperty(key) ? getLong(key) : defaultValue;
    }
    
    /** {@inheritDoc} */
    @Override
    public long getLong(String key, long defaultValue) {
        return ff4jStore().existProperty(key) ? getLong(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public short getShort(String key) {
        try {
            return new Short((String) getValue(key)).shortValue();
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create Short from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public short getShort(String key, short defaultValue) {
        return ff4jStore().existProperty(key) ? getShort(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public Short getShort(String key, Short defaultValue) {
        return ff4jStore().existProperty(key) ? getShort(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public BigDecimal getBigDecimal(String key) {
        try {
            return new BigDecimal((String) getValue(key));
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create BigDecimal from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public BigDecimal getBigDecimal(String key, BigDecimal defaultValue) {
        return ff4jStore().existProperty(key) ? getBigDecimal(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public BigInteger getBigInteger(String key) {
        try {
            return new BigInteger((String) getValue(key));
        } catch(NumberFormatException nbe) {
            throw new InvalidPropertyTypeException("Cannot create BigInteger from " + getValue(key), nbe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public BigInteger getBigInteger(String key, BigInteger defaultValue) {
        return ff4jStore().existProperty(key) ? getBigInteger(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public String getString(String key) {
       return (String) getValue(key);
    }

    /** {@inheritDoc} */
    @Override
    public String getString(String key, String defaultValue) {
        return ff4jStore().existProperty(key) ? getString(key) : defaultValue;
    }

    /** {@inheritDoc} */
    @Override
    public List<Object> getList(String key) {
        return new ArrayList<Object>(
                Arrays.asList(
                        ((String) getValue(key)).split(DELIMITER)));
    }
    
    /** {@inheritDoc} */
    @Override
    public List<Object> getList(String key, List<?> defaultValue) {
        if (ff4jStore().existProperty(key)) return getList(key);
        return new ArrayList<Object>(defaultValue);
    }
    
    /** {@inheritDoc} */
    @Override
    public String[] getStringArray(String key) {
        return getList(key).toArray(new String[0]);
    }
    
    /** 
     * Read FF4J Store from its attribute.
     * 
     * @return
     *      current ff4j store.
     */
    public PropertyStore ff4jStore() {
        if (ff4jStore == null) {
            throw new IllegalStateException("Cannot load property from store as not initialized please set 'ff4jStore' property");
        }
        return getFf4jStore();
    }

    /**
     * Getter accessor for attribute 'ff4jStore'.
     *
     * @return
     *       current value of 'ff4jStore'
     */
    public PropertyStore getFf4jStore() {
        return ff4jStore;
    }

    /**
     * Setter accessor for attribute 'ff4jStore'.
     * @param ff4jStore
     * 		new value for 'ff4jStore '
     */
    public void setFf4jStore(PropertyStore ff4jStore) {
        this.ff4jStore = ff4jStore;
    }
   
}
