package org.ff4j.dynamodb.property;

/*-
 * #%L
 * ff4j-store-aws-dynamodb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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



import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import software.amazon.awssdk.utils.CollectionUtils;

import java.util.HashSet;
import java.util.Set;

/**
 * Implementation of {@link PropertyMapper} for DynamoDB store
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class PropertyDynamoDBMapper implements PropertyMapper<DynamoDbProperty> {

    /**
     * {@inheritDoc}
     */
    @Override
    public DynamoDbProperty toStore(Property<?> property) {
        DynamoDbProperty item = new DynamoDbProperty();
        item.setName(property.getName());
        item.setValue(property.asString());
        item.setType(property.getType());
        item.setDescription(property.getDescription());

        Set<String> fixedValues = new HashSet<>();
        if (!CollectionUtils.isNullOrEmpty(property.getFixedValues())) {
            for (Object fixedValue : property.getFixedValues()) {
                fixedValues.add(fixedValue.toString());
            }
            item.setValues(fixedValues);
        }
        return item;
    }

    @Override
    public Property<?> fromStore(DynamoDbProperty item) {
        return PropertyFactory.createProperty(item.getName(),
                item.getType(),
                item.getValue(),
                item.getDescription(),
                CollectionUtils.isNullOrEmpty(item.getValues()) ? new HashSet<>() : new HashSet<>(item.getValues()));
    }
}
