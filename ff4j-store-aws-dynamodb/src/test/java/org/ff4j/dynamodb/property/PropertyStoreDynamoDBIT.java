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

import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;

/**
 * Run against a real AWS Environment
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
// Needs an AWS environment, not available in Travis, this is why it is ignored
@Ignore
public class PropertyStoreDynamoDBIT extends PropertyStoreTestSupport {

    private static PropertyStoreDynamoDB store;

    @BeforeClass
    public static void init() {
        DynamoDbClient dynamoDB = DynamoDbClient.create();
        store = new PropertyStoreDynamoDB(dynamoDB);
        store.importPropertiesFromXmlFile("test-ff4j-features.xml");
    }

    @AfterClass
    public static void clean() {
       store.clear();
    }

    @Override
    protected PropertyStoreDynamoDB initPropertyStore() {
        return store;
    }


}
