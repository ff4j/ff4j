package org.ff4j.feature.repository;

/*-
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

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.attributeAsJson;
import static org.ff4j.utils.JsonUtils.cacheJson;
import static org.ff4j.utils.JsonUtils.collectionAsJson;
import static org.ff4j.utils.JsonUtils.objectAsJson;
import static org.ff4j.utils.Util.setOf;

import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.FF4jRepository;
import org.ff4j.FF4jRepositoryListener;
import org.ff4j.FF4jRepositorySupport;
import org.ff4j.event.repository.AuditTrail;
import org.ff4j.exception.ItemNotFoundException;
import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.feature.exception.GroupNotFoundException;

/**
 * Specialization of the {@link FF4jRepository} to work with {@link Feature}. 
 * 
 * Implementation of dedicated operation related to feature through {@link FeaturesRepository}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class FeaturesRepositorySupport 
                extends FF4jRepositorySupport<Feature, FeaturesRepositoryListener > 
                implements FeaturesRepository {

    /** serialVersionUID. */
    private static final long serialVersionUID = -7450698535116107530L;

    /** Json Attribute. */
    public static final String JSON_ATTRIBUTE_FEATURECOUNT = "featuresCount";
    
    /** Json Attribute. */
    public static final String JSON_ATTRIBUTE_FEATURENAMES = "featuresNames";
    
    /** Json Attribute. */
    public static final String JSON_ATTRIBUTE_GROUPCOUNT   = "groupsCount";
    
    /** Json Attribute. */
    public static final String JSON_ATTRIBUTE_GROUPNAMES   = "groupNames";
    
    /** Listener Name. */
    private static final String LISTENERNAME_AUDIT = "FeatureStoreAuditListener";
   
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    protected void assertFeatureExist(String uid) {
        try {
            assertItemExist(uid);
        } catch(ItemNotFoundException infEx) {
            throw new FeatureNotFoundException(uid, infEx);
        }
    }
    
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    protected void assertGroupExist(String groupName) {
        assertHasLength(groupName, "groupName");
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
    }
    
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    protected void assertFeatureNotNull(Feature feature) {
        assertNotNull(feature);
    }
    
    /** {@inheritDoc} */
    protected String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(attributeAsJson(JSON_ATTRIBUTE_CLASSNAME, this.getClass().getCanonicalName()));
        // Features
        Set<String> myFeatures = setOf(findAll().map(Feature::getUid));
        sb.append(attributeAsJson(JSON_ATTRIBUTE_FEATURECOUNT, myFeatures.size()));
        sb.append(objectAsJson(JSON_ATTRIBUTE_FEATURENAMES, collectionAsJson(myFeatures)));
        // Groups
        Set<String> groups = listGroupNames().collect(Collectors.toSet());
        sb.append(attributeAsJson(JSON_ATTRIBUTE_GROUPCOUNT, groups.size()));
        sb.append(objectAsJson(JSON_ATTRIBUTE_GROUPNAMES, collectionAsJson(groups)));
        // Cache
        sb.append(cacheJson(this));
        sb.append("}");
        return sb.toString();
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        return toJson();
    }
    
    /** {@inheritDoc} */
    public void save(Iterable<Feature> iterFeatures) {
        assertNotNull(iterFeatures);
        iterFeatures.forEach(feature -> {
            preUpdate(feature);
            saveFeature(feature);
            // Notify all listeners registered (like AuditTrail)
            this.notify(l -> l.onUpdate(feature));
        });
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(Iterable<String> iterIds) {
        assertNotNull(iterIds);
        iterIds.forEach(uid -> {
            assertFeatureExist(uid);
            deleteFeature(uid);
            this.notify(l -> l.onDelete(uid));
        });
    }
    
    /** {@inheritDoc} */
    @Override
    public void toggleOn(String uid) {
        if (!exists(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        // The toggleOn method also update the last modified date
        saveFeature(read(uid).toggleOn());
        // AuditTrail or any subscriber
        notify(l -> l.onToggleOnFeature(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void toggleOff(String uid) {
        if (!exists(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        saveFeature(read(uid).toggleOff());
        // AuditTrail or any subscriber
        notify(l -> l.onToggleOffFeature(uid));
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        delete(findAllIds());
        this.notify(l -> l.onDeleteAll());
    }
    
    /** {@inheritDoc} */
    @Override
    public void toggleOnGroup(String groupName) {
        this.notify(l -> l.onToggleOnGroup(groupName));
        readGroup(groupName).map(Feature::getUid).forEach(this::toggleOn);
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOffGroup(String groupName) {
        this.notify(l -> l.onToggleOffGroup(groupName));
        readGroup(groupName).map(Feature::getUid).forEach(this::toggleOff);
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertHasLength(uid, "uid");
        assertHasLength(groupName, "groupName");
        this.notify(l -> l.onAddFeatureToGroup(uid, groupName));
        saveFeature(read(uid).group(groupName));
    }
    
    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        this.notify(l -> l.onRemoveFeatureFromGroup(uid, groupName));
        Feature currentFeature = find(uid).get();
        if (currentFeature.getGroup().isPresent() && 
            currentFeature.getGroup().get().equals(groupName)) {
            saveFeature(find(uid).get().group(null));
        }
    } 
    
    /** {@inheritDoc} */
    @Override
    public void registerListener(String name, FF4jRepositoryListener<Feature> listener) {
        // Enforce subclass to reach AbstractObservable.registerListener(..)
        registerListener(name, (FeaturesRepositoryListener) listener);
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerAuditListener(AuditTrail auditTrail) {
        this.registerListener(LISTENERNAME_AUDIT, new FeaturesRepositoryListenerAudit(auditTrail));
    }
    
    /** {@inheritDoc} */
    @Override
    public void unRegisterAuditListener() {
        this.unregisterListener(LISTENERNAME_AUDIT);
    }
    
}
