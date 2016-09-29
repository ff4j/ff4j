package org.ff4j.audit.proxy;

import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_CREATESCHEMA;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.audit.EventConstants.TARGET_PROPERTY;
import static org.ff4j.audit.EventConstants.TARGET_PSTORE;

import java.util.Collection;

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

import java.util.Map;
import java.util.Set;

import org.ff4j.FF4j;
import org.ff4j.audit.EventBuilder;
import org.ff4j.audit.EventPublisher;
import org.ff4j.property.Property;
import org.ff4j.property.store.PropertyStore;

/**
 * Implementation of audit on top of store.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class PropertyStoreAuditProxy implements PropertyStore {

    /** Current FeatureStore. */
    private PropertyStore target = null;
    
    /** Reference. */
    private FF4j ff4j = null;
    
    /**
     * Only constructor.
     *
     * @param pTarget
     */
    public PropertyStoreAuditProxy(FF4j pFF4j, PropertyStore pTarget) {
        this.target = pTarget;
        this.ff4j   = pFF4j;
    }

    /** {@inheritDoc} */
    public  < T > void createProperty(Property<T> prop) {
        long start = System.nanoTime();
        target.createProperty(prop);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_CREATE)
                    .property(prop.getName())
                    .value(prop.asString())
                    .duration(duration));
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        long start = System.nanoTime();
        target.updateProperty(name, newValue);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_UPDATE)
                    .property(name)
                    .value(newValue)
                    .duration(duration));
    }

    /** {@inheritDoc} */
    public <T> void updateProperty(Property<T> prop) {
        long start = System.nanoTime();
        target.updateProperty(prop);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_UPDATE)
                    .property(prop.getName())
                    .value(prop.asString())
                    .duration(duration));
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        long start = System.nanoTime();
        target.deleteProperty(name);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_DELETE)
                    .property(name)
                    .duration(duration));
    }
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        return target.existProperty(name);
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        return target.readProperty(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name, Property<?> defaultValue) {
        return target.readProperty(name, defaultValue);
    }
    
    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        return target.readAllProperties();
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return target.listPropertyNames();
    }

    /** {@inheritDoc} */
    public boolean isEmpty() {
        return target.isEmpty();
    }

    /** {@inheritDoc} */
    public void clear() {
        long start = System.nanoTime();
        target.clear();
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_CLEAR).type(TARGET_PSTORE)
                .name(ff4j.getPropertiesStore().getClass().getName())
                .duration(duration));
    }
    
    /** {@inheritDoc} */
    public void importProperties(Collection<Property<?>> properties) {
        // Do not use target as the delete/create operation will be traced
        if (properties != null) {
            for (Property<?> property : properties) {
                if (existProperty(property.getName())) {
                    deleteProperty(property.getName());
                }
                createProperty(property);
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        target.createSchema();
        publish(builder(ACTION_CREATESCHEMA).feature("For Properties"));
    }
    
    /**
     * Init a new builder;
     *
     * @return
     *      new builder
     */
    private EventBuilder builder(String action) {
        return new EventBuilder(ff4j).type(TARGET_PROPERTY).action(action);
    }
    
    /**
     * Publish target event to {@link EventPublisher}
     *
     * @param eb
     *      current builder
     */
    private void publish(EventBuilder eb) {
        ff4j.getEventPublisher().publish(eb.build());
    }

	/**
	 * Getter accessor for attribute 'target'.
	 *
	 * @return
	 *       current value of 'target'
	 */
	public PropertyStore getTarget() {
		return target;
	}

       

}
