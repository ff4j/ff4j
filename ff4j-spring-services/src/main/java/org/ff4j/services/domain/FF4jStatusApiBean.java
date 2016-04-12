package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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


import org.ff4j.FF4j;

import java.io.Serializable;


/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FF4jStatusApiBean implements Serializable {

    private static final long serialVersionUID = 3126369513162358650L;

    private String uptime;

    private boolean autocreate;

    private String version = "N/A";

    private FeatureStoreApiBean featuresStore;

    private EventRepositoryApiBean eventRepository;

    private AuthorizationsManagerApiBean authorizationsManager;

    public FF4jStatusApiBean() {
        super();
    }

    public FF4jStatusApiBean(FF4j ff4j) {
        // UpTime
        long up = System.currentTimeMillis() - ff4j.getStartTime();
        long daynumber = up / (1000 * 3600 * 24);
        up = up - (daynumber * 1000 * 3600 * 24);
        long hourNumber = up / (1000 * 3600);
        up = up - (hourNumber * 1000 * 3600);
        long minutenumber = up / (1000 * 60);
        up = up - (minutenumber * 1000 * 60);
        long secondnumber = up / 1000;
        uptime = daynumber + " day(s) ";
        uptime += hourNumber + " hours(s) ";
        uptime += minutenumber + " minute(s) ";
        uptime += secondnumber + " seconds\"";
        autocreate = ff4j.isAutocreate();
        version = ff4j.getVersion();
        if (null != ff4j.getFeatureStore()) {
            featuresStore = new FeatureStoreApiBean(ff4j.getFeatureStore());
        }
        if (null != ff4j.getEventRepository()) {
            eventRepository = new EventRepositoryApiBean(ff4j.getEventRepository(), null, null);
        }
        if (null != ff4j.getAuthorizationsManager()) {
            authorizationsManager = new AuthorizationsManagerApiBean(ff4j.getAuthorizationsManager());
        }
    }

    public String getUptime() {
        return uptime;
    }

    public boolean isAutocreate() {
        return autocreate;
    }

    public String getVersion() {
        return version;
    }

    public FeatureStoreApiBean getFeaturesStore() {
        return featuresStore;
    }

    public EventRepositoryApiBean getEventRepository() {
        return eventRepository;
    }

    public AuthorizationsManagerApiBean getAuthorizationsManager() {
        return authorizationsManager;
    }

    public void setUptime(String uptime) {
        this.uptime = uptime;
    }

    public void setAutocreate(boolean autocreate) {
        this.autocreate = autocreate;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public void setFeaturesStore(FeatureStoreApiBean featuresStore) {
        this.featuresStore = featuresStore;
    }

    public void setEventRepository(EventRepositoryApiBean eventRepository) {
        this.eventRepository = eventRepository;
    }

    public void setAuthorizationsManager(AuthorizationsManagerApiBean authorizationsManager) {
        this.authorizationsManager = authorizationsManager;
    }
}
