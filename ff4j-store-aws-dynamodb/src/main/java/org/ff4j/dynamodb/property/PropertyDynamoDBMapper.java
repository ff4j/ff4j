package org.ff4j.dynamodb.property;


/*
 * #%L
 * ff4j-store-aws-dynamodb
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


import com.amazonaws.services.dynamodbv2.document.Item;
import com.amazonaws.util.CollectionUtils;
import com.amazonaws.util.StringUtils;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import java.util.HashSet;
import java.util.Set;

import static org.ff4j.dynamodb.DynamoDBConstants.*;

/**
 * Implementation of {@link PropertyMapper} for DynamoDB store
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class PropertyDynamoDBMapper implements PropertyMapper<Item> {

    /**
     * {@inheritDoc}
     */
    @Override
    public Item toStore(Property<?> property) {
        Item item = new Item();
        item.withString(PROPERTY_NAME, property.getName());
        item.withString(PROPERTY_VALUE, property.asString());
        item.withString(PROPERTY_TYPE, property.getType());

        if (!StringUtils.isNullOrEmpty(property.getDescription())) {
            item.withString(PROPERTY_DESCRIPTION, property.getDescription());
        }

        Set<String> fixedValues = new HashSet<String>();
        if (!CollectionUtils.isNullOrEmpty(property.getFixedValues())) {
            for (Object fixedValue : property.getFixedValues()) {
                fixedValues.add(fixedValue.toString());
            }
            item.withStringSet(PROPERTY_VALUES, fixedValues);
        }
        return item;
    }

    @Override
    public Property<?> fromStore(Item item) {
        return PropertyFactory.createProperty(item.getString(PROPERTY_NAME),
                item.getString(PROPERTY_TYPE),
                item.getString(PROPERTY_VALUE),
                item.getString(PROPERTY_DESCRIPTION),
                CollectionUtils.isNullOrEmpty(item.getStringSet(PROPERTY_VALUES)) ? new HashSet<String>() : new HashSet<String>(item.getStringSet(PROPERTY_VALUES)));
    }
}
