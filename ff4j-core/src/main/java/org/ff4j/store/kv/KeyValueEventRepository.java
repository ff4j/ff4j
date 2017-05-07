package org.ff4j.store.kv;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.mapper.EventMapper;
import org.ff4j.utils.Util;

/**
 * Common implementation of time-series and audit using J/V stores.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <D>
 *      target driver
 */
public class KeyValueEventRepository < VALUE > extends AbstractEventRepository {

    /** Driver to access a K/V Store. */
    protected KeyValueDriver < String, VALUE > driver;
    
    /** Work with Mapping. */
    protected EventMapper < VALUE > eventMapper;
   
    /**
     * Default constructor
     */
    public KeyValueEventRepository() {
    }
            
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValueEventRepository(KeyValueDriver < String, VALUE > driver) {
        this.driver = driver;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // Nothing to do ,created with the keys
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        Util.assertEvent(e);
        if (EventConstants.ACTION_CHECK_OK.equalsIgnoreCase(e.getAction())) {
            getDriver().putValue(
                    getDriver().getHitCountKey(e), 
                    eventMapper.toStore(e));
        } else if (EventConstants.ACTION_CHECK_OFF.equalsIgnoreCase(e.getAction())) {
            getDriver().putValue(
                    getDriver().getMissKey(e), 
                    eventMapper.toStore(e));
        } else {
            getDriver().putValue(
                    getDriver().getAuditTrailKey(e), 
                    eventMapper.toStore(e));
        }
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
    }

    /**
     * Getter accessor for attribute 'driver'.
     *
     * @return
     *       current value of 'driver'
     */
    public KeyValueDriver < String, VALUE >  getDriver() {
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
    public void setDriver(KeyValueDriver < String, VALUE >  driver) {
        this.driver = driver;
    }

    
    

}
