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

import org.ff4j.dynamodb.DynamoDbBaseIntegrationTest;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.jupiter.api.AfterEach;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;

/**
 * Run against a real AWS Environment
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
// Needs an AWS environment, not available in Travis, this is why it is ignored
public class PropertyStoreDynamoDBIT extends PropertyStoreTestSupport implements DynamoDbBaseIntegrationTest {

    private PropertyStoreDynamoDB store;

    @AfterEach
    public void clean() {
        store.clear();
    }

    @Override
    protected PropertyStoreDynamoDB initPropertyStore() {
        DynamoDbClient dynamoDB = createDynamoDbClient();
        createPropertiesTable(dynamoDB);
        store = new PropertyStoreDynamoDB(dynamoDB);
        store.importPropertiesFromXmlFile("test-ff4j-features.xml");

        return store;
    }
}
