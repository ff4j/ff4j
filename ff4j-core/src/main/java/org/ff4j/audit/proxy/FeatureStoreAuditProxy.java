package org.ff4j.audit.proxy;

import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_CREATESCHEMA;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.audit.EventConstants.TARGET_FSTORE;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;

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
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;

/**
 * Proxy to publish operation to audit.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeatureStoreAuditProxy implements FeatureStore {

    /** Current FeatureStore. */
    private FeatureStore target = null;
    
    /** Reference. */
    private FF4j ff4j = null;
    
    /**
     * Only constructor.
     *
     * @param pTarget
     */
    public FeatureStoreAuditProxy(FF4j pFF4j, FeatureStore pTarget) {
        this.target = pTarget;
        this.ff4j   = pFF4j;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        target.createSchema();
        publish(builder(ACTION_CREATESCHEMA).feature("For Features"));
    }
    
    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        long start = System.nanoTime();
        target.enable(uid);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_TOGGLE_ON).feature(uid).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        long start = System.nanoTime();
        target.disable(uid);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_TOGGLE_OFF).feature(uid).duration(duration));
    }    

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        long start = System.nanoTime();
        target.create(fp);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_CREATE).feature(fp.getUid()).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        long start = System.nanoTime();
        target.delete(uid);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_DELETE).feature(uid).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        long start = System.nanoTime();
        target.update(fp);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_UPDATE).feature(fp.getUid()).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        long start = System.nanoTime();
        target.grantRoleOnFeature(uid, roleName);
        long duration = System.nanoTime() - start;
        publish(builder("GRANT ROLE " + roleName).feature(uid).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        long start = System.nanoTime();
        target.removeRoleFromFeature(uid, roleName);
        long duration = System.nanoTime() - start;
        publish(builder("REMOVE ROLE " + roleName).feature(uid).duration(duration));
    }
    
    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        long start = System.nanoTime();
        target.enableGroup(groupName);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_TOGGLE_ON).group(groupName).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        long start = System.nanoTime();
        target.disableGroup(groupName);
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_TOGGLE_OFF).group(groupName).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        long start = System.nanoTime();
        target.addToGroup(uid, groupName);
        long duration = System.nanoTime() - start;
        publish(builder("ADD TO GROUP " + groupName).feature(uid).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        long start = System.nanoTime();
        target.removeFromGroup(uid, groupName);
        long duration = System.nanoTime() - start;
        publish(builder("ADD TO GROUP " + groupName).feature(uid).duration(duration));
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        long start = System.nanoTime();
        target.clear();
        long duration = System.nanoTime() - start;
        publish(builder(ACTION_CLEAR).type(TARGET_FSTORE)
                .name(ff4j.getFeatureStore().getClass().getName())
                .duration(duration));
    }
    
    /**
     * Init a new builder;
     *
     * @return
     *      new builder
     */
    private EventBuilder builder(String action) {
        return new EventBuilder(ff4j).type(TARGET_FEATURE).action(action);
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
    
    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        return target.exist(uid);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        return target.read(uid);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        return target.readAll();
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        return target.existGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        return target.readGroup(groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        return target.readAllGroups();
    }
    
    /** {@inheritDoc} */
    @Override
    public void importFeatures(Collection<Feature> features) {
        // Do not use target as the delete/create operation will be traced
        if (features != null) {
            for (Feature feature : features) {
                if (exist(feature.getUid())) {
                    delete(feature.getUid());
                }
                create(feature);
            }
        }
    }

	/**
	 * Getter accessor for attribute 'target'.
	 *
	 * @return
	 *       current value of 'target'
	 */
	public FeatureStore getTarget() {
		return target;
	}
}