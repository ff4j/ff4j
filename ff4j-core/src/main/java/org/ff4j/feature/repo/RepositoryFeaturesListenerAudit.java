package org.ff4j.feature.repo;

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

import org.ff4j.event.Event.Action;
import org.ff4j.event.Event.Scope;
import org.ff4j.event.repo.AbstractAuditTrailListener;
import org.ff4j.event.repo.AuditTrail;
import org.ff4j.feature.Feature;

/**
 * Proposition of superclass to allow audit trail trackings.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 */
public class RepositoryFeaturesListenerAudit extends AbstractAuditTrailListener < Feature > 
                                             implements RepositoryFeaturesListener {
    
    public RepositoryFeaturesListenerAudit(AuditTrail auditTrail) {
        super(auditTrail, Scope.FEATURE, Scope.FEATURESTORE);
    }
    
    /** {@inheritDoc} */
    @Override
    public void onToggleOnFeature(String uid) {
        log(createEvent(Action.TOGGLE_ON, Scope.FEATURE).targetUid(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void onToggleOffFeature(String uid) {
        log(createEvent(Action.TOGGLE_OFF, Scope.FEATURE).targetUid(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void onToggleOnGroup(String groupName) {
        log(createEvent(Action.TOGGLE_ON, Scope.FEATURE_GROUP).targetUid(groupName));
    }

    /** {@inheritDoc} */
    @Override
    public void onToggleOffGroup(String groupName) {
        log(createEvent(Action.TOGGLE_OFF, Scope.FEATURE_GROUP).targetUid(groupName));
    }

    /** {@inheritDoc} */
    @Override
    public void onAddFeatureToGroup(String uid, String groupName) {
        log(createEvent(Action.ADD_TO_GROUP, Scope.FEATURE)
                .targetUid(groupName)
                .put("targetGroup", groupName));
    }

    /** {@inheritDoc} */
    @Override
    public void onRemoveFeatureFromGroup(String uid, String groupName) {
        log(createEvent(Action.REMOVE_FROM_GROUP, Scope.FEATURE)
                .targetUid(groupName)
                .put("targetGroup", groupName));
    }

}
