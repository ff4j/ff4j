package org.ff4j.couchbase.store.mapper;

/*
 * #%L
 * ff4j-store-springcouchbase
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

import org.ff4j.audit.Event;
import org.ff4j.core.Feature;
import org.ff4j.couchbase.store.document.EventDocument;
import org.ff4j.couchbase.store.document.FeatureDocument;
import org.ff4j.couchbase.store.document.PropertyDocument;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by farrellyja on 10/11/2017.
 */
public class DocumentMapper {
    private DocumentMapper() {

    }

    public static FeatureDocument featureToFeatureDocument(Feature fp) {
        FeatureDocument featureDocument = new FeatureDocument();
        featureDocument.setUid(fp.getUid());
        featureDocument.setEnable(fp.isEnable());
        featureDocument.setDescription(fp.getDescription());
        featureDocument.setCustomProperties(fp.getCustomProperties());
        featureDocument.setFlippingStrategy(fp.getFlippingStrategy());
        featureDocument.setGroup(fp.getGroup());
        featureDocument.setPermissions(fp.getPermissions());
        return featureDocument;
    }

    public static Feature featureDocumentToFeature(FeatureDocument featureDocument) {
        Feature fp = new Feature(featureDocument.getUid());
        fp.setEnable(featureDocument.getEnable());
        fp.setDescription(featureDocument.getDescription());
        fp.setCustomProperties(featureDocument.getCustomProperties());
        fp.setFlippingStrategy(featureDocument.getFlippingStrategy());
        fp.setGroup(featureDocument.getGroup());
        fp.setPermissions(featureDocument.getPermissions());
        return fp;
    }

    public static PropertyDocument propertyToPropertyDocument(Property property) {
        PropertyDocument propertyDocument = new PropertyDocument();
        propertyDocument.setName(property.getName());
        propertyDocument.setDescription(property.getDescription());
        propertyDocument.setReadOnly(property.isReadOnly());
        propertyDocument.setFixedValues(property.getFixedValues());
        propertyDocument.setType(property.getType());
        propertyDocument.setValue(property.getValue());
        return propertyDocument;
    }

    public static Property propertyDocumentToProperty(PropertyDocument propertyDocument) {
        return PropertyFactory.createProperty(
            propertyDocument.getName(),
            propertyDocument.getType(),
            propertyDocument.getValue().toString(),
            propertyDocument.getDescription(),
            propertyDocument.getFixedValues() != null ? ((Set<Object>) propertyDocument.getFixedValues()).stream()
                .map(v -> v.toString())
                .collect(Collectors.toSet()) : new HashSet<>()
        );
    }

    public static EventDocument eventToEventDocument(Event event) {
        EventDocument eventDocument = new EventDocument();
        eventDocument.setUuid(event.getUuid());
        eventDocument.setTimestamp(event.getTimestamp());
        eventDocument.setDuration(event.getDuration());
        eventDocument.setHostName(event.getHostName());
        eventDocument.setSource(event.getSource());
        eventDocument.setUser(event.getUser());
        eventDocument.setName(event.getName());
        eventDocument.setType(event.getType());
        eventDocument.setAction(event.getAction());
        eventDocument.setValue(event.getValue());
        eventDocument.setCustomKeys(event.getCustomKeys());
        return eventDocument;
    }

    public static Event eventDocumentToEvent(EventDocument eventDocument) {
        Event event = new Event();
        event.setUuid(eventDocument.getUuid());
        event.setTimestamp(eventDocument.getTimestamp());
        event.setDuration(eventDocument.getDuration());
        event.setHostName(eventDocument.getHostName());
        event.setSource(eventDocument.getSource());
        event.setUser(eventDocument.getUser());
        event.setName(eventDocument.getName());
        event.setType(eventDocument.getType());
        event.setAction(eventDocument.getAction());
        event.setValue(eventDocument.getValue());
        event.setCustomKeys(eventDocument.getCustomKeys());
        return event;
    }
}
