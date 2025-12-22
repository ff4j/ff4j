package org.ff4j.dynamodb.feature;

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

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.dynamodb.DynamoDbBaseIntegrationTest;
import org.ff4j.test.store.FeatureStoreTestSupport;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;

import static org.ff4j.test.TestsFf4jConstants.AWESOME;
import static org.ff4j.test.TestsFf4jConstants.ROLE_TEST;

/**
 * Run against a real AWS environment.
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
// Needs an AWS environment, not available in Travis, this is why it is ignored
public class FeatureStoreDynamoDBIT extends FeatureStoreTestSupport implements DynamoDbBaseIntegrationTest {

    private FeatureStoreDynamoDB store;

    @AfterEach
    public void clean() {
        store.clear();
    }

    @Override
    protected FeatureStore initStore() {
        DynamoDbClient dynamoDB = createDynamoDbClient();
        createFeaturesTable(dynamoDB);
        store = new FeatureStoreDynamoDB(dynamoDB);
        store.clearData();
        store.importFeaturesFromXmlFile("test-ff4j-features.xml");
        return store;
    }

    // TODO : move up but fails with Neo4j
    @Test
    public void addRoleToFeatureWithoutRole() {
        // Given
        assertFf4j.assertThatFeatureHasNotRole(AWESOME, ROLE_TEST);

        // When
        testedStore.grantRoleOnFeature(AWESOME, ROLE_TEST);

        // Then
        assertFf4j.assertThatFeatureHasRole(AWESOME, ROLE_TEST);
    }

    @Test
    public void addEmptyStringAsRole() {
        // given
        Feature feature = testedStore.read(AWESOME);
        Assertions.assertTrue(feature.getPermissions().isEmpty());

        // when
        feature.getPermissions().add("");
        testedStore.update(feature);

        // then
        Assertions.assertTrue(feature.getPermissions().isEmpty());
    }

}
