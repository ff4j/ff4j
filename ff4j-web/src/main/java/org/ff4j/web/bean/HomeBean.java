package org.ff4j.web.bean;

import static org.ff4j.web.bean.WebConstants.PIC_DISABLE;

/*
 * #%L
 * ff4j-console
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

import java.io.Serializable;

import org.ff4j.FF4j;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.security.AuthorizationsManager;

/**
 * Webbean to display home information
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class HomeBean implements Serializable {

    /** serial. */
    private static final long serialVersionUID = 9115704270593636619L;

    /** uptime. */
    private String uptime;

    /** class of store. */
    private String featureStore = PIC_DISABLE;

    /** class of store. */
    private String propertyStore = PIC_DISABLE;

    /** authorizationManager. */
    private String security = PIC_DISABLE;

    /** class of monitoring. */
    private String monitoring = PIC_DISABLE;

    /** cmass of cache Manager if available. */
    private String caching = PIC_DISABLE;

    /** version of target ff4j. */
    private String version = PIC_DISABLE;

    /** number of features to display. */
    private int nbFeature = 0;

    /** number of features to display. */
    private int nbProperties = 0;

    /** number of groups to display. */
    private int nbGroup = 0;

    /** number of events used. */
    private int nbEvents = 0;

    /**
     * Default constructor.
     */
    public HomeBean() {
    }

    /**
     * Default constructor.
     */
    public HomeBean(FF4j ff4j) {
        this.version = ff4j.getVersion();

        setUptime(ff4j.getStartTime());

        // Feature Store
        if (ff4j.getFeatureStore() != null) {
            FeatureStore fs = ff4j.getConcreteFeatureStore();
            this.featureStore = fs.getClass().getSimpleName();
            this.nbFeature = fs.readAll().size();
            this.nbGroup   = fs.readAllGroups().size();
            featureStore = featureStore.replaceAll("FeatureStore", "").toLowerCase();
        }

        // Property Store
        if (ff4j.getPropertiesStore() != null) {
            PropertyStore ps = ff4j.getConcretePropertyStore();
            this.propertyStore = ps.getClass().getSimpleName();
            this.nbProperties = ps.listPropertyNames().size();
            propertyStore = propertyStore.replaceAll("PropertyStore", "").toLowerCase();
        }

        // Monitoring
        EventRepository evtRepository = ff4j.getEventRepository();
        if (evtRepository != null) {
            this.monitoring = evtRepository.getClass().getSimpleName();
            monitoring = monitoring.replaceAll("EventRepository", "").toLowerCase();
        }

        // Security
        AuthorizationsManager authManager = ff4j.getAuthorizationsManager();
        if (authManager != null) {
            this.security = authManager.getClass().getSimpleName();
            security = security.replaceAll("AuthorisationManager", "");
            security = security.replaceAll("AuthorizationManager", "");
            security = security.toLowerCase();
        }

        // Caching
        FF4jCacheProxy cacheProxy = ff4j.getCacheProxy();
        if (cacheProxy != null) {
           this.caching = cacheProxy.getCacheManager().getCacheProviderName().toLowerCase();
        }
    }

    /**
     * Getter accessor for attribute 'security'.
     *
     * @return current value of 'security'
     */
    public String getSecurity() {
        return security;
    }

    /**
     * Getter accessor for attribute 'monitoring'.
     *
     * @return current value of 'monitoring'
     */
    public String getMonitoring() {
        return monitoring;
    }

    /**
     * Getter accessor for attribute 'version'.
     *
     * @return current value of 'version'
     */
    public String getVersion() {
        return version;
    }

    /**
     * Getter accessor for attribute 'uptime'.
     *
     * @return current value of 'uptime'
     */
    public String getUptime() {
        return uptime;
    }

    /**
     * Setter accessor for attribute 'uptime'.
     *
     * @param ff4jStartTime
     *            new value for 'uptime '
     */
    public void setUptime(long ff4jStartTime) {
        StringBuilder sb = new StringBuilder();
        long uptime = System.currentTimeMillis() - ff4jStartTime;
        long daynumber = uptime / (1000 * 3600 * 24L);
        uptime = uptime - daynumber * 1000 * 3600 * 24L;
        long hourNumber = uptime / (1000 * 3600L);
        uptime = uptime - hourNumber * 1000 * 3600L;
        long minutenumber = uptime / (1000 * 60L);
        uptime = uptime - minutenumber * 1000 * 60L;
        long secondnumber = uptime / 1000L;
        sb.append(daynumber + " days ");
        sb.append(hourNumber + " hours ");
        sb.append(minutenumber + " min ");
        sb.append(secondnumber + " sec");
        this.uptime = sb.toString();
    }

    /**
     * Getter accessor for attribute 'nbFeature'.
     *
     * @return current value of 'nbFeature'
     */
    public int getNbFeature() {
        return nbFeature;
    }

    /**
     * Getter accessor for attribute 'nbGroup'.
     *
     * @return current value of 'nbGroup'
     */
    public int getNbGroup() {
        return nbGroup;
    }

    /**
     * Getter accessor for attribute 'nbEvents'.
     *
     * @return current value of 'nbEvents'
     */
    public int getNbEvents() {
        return nbEvents;
    }

    /**
     * Getter accessor for attribute 'featureStore'.
     *
     * @return
     *       current value of 'featureStore'
     */
    public String getFeatureStore() {
        return featureStore;
    }

    /**
     * Getter accessor for attribute 'propertyStore'.
     *
     * @return
     *       current value of 'propertyStore'
     */
    public String getPropertyStore() {
        return propertyStore;
    }

    /**
     * Getter accessor for attribute 'nbProperties'.
     *
     * @return
     *       current value of 'nbProperties'
     */
    public int getNbProperties() {
        return nbProperties;
    }

    /**
     * Getter accessor for attribute 'caching'.
     *
     * @return
     *       current value of 'caching'
     */
    public String getCaching() {
        return caching;
    }

}
