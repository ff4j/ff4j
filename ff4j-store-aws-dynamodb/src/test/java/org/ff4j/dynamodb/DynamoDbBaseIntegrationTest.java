package org.ff4j.dynamodb;

/*-
 * #%L
 * ff4j-store-aws-dynamodb
 * %%
 * Copyright (C) 2013 - 2025 FF4J
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

import org.junit.jupiter.api.TestInstance;
import org.testcontainers.containers.localstack.LocalStackContainer;
import org.testcontainers.junit.jupiter.Container;
import org.testcontainers.junit.jupiter.Testcontainers;
import org.testcontainers.utility.DockerImageName;
import software.amazon.awssdk.auth.credentials.AwsBasicCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentials;
import software.amazon.awssdk.auth.credentials.AwsCredentialsProvider;
import software.amazon.awssdk.auth.credentials.StaticCredentialsProvider;
import software.amazon.awssdk.regions.Region;
import software.amazon.awssdk.services.dynamodb.DynamoDbClient;
import software.amazon.awssdk.services.dynamodb.model.AttributeDefinition;
import software.amazon.awssdk.services.dynamodb.model.BillingMode;
import software.amazon.awssdk.services.dynamodb.model.CreateTableRequest;
import software.amazon.awssdk.services.dynamodb.model.GlobalSecondaryIndex;
import software.amazon.awssdk.services.dynamodb.model.KeySchemaElement;
import software.amazon.awssdk.services.dynamodb.model.Projection;
import software.amazon.awssdk.services.dynamodb.model.ProjectionType;
import software.amazon.awssdk.services.dynamodb.model.ProvisionedThroughput;
import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@Testcontainers
public interface DynamoDbBaseIntegrationTest {

    @Container
    LocalStackContainer localstack = new LocalStackContainer(DockerImageName
            .parse("localstack/localstack:4.12"))
            .withServices(LocalStackContainer.Service.DYNAMODB);

    default DynamoDbClient createDynamoDbClient() {
        return DynamoDbClient.builder()
                .endpointOverride(localstack.getEndpointOverride(LocalStackContainer.Service.DYNAMODB))
                .region(Region.of(localstack.getRegion()))
                .credentialsProvider(StaticCredentialsProvider.create(AwsBasicCredentials
                        .create(localstack.getAccessKey(), localstack.getSecretKey())))
                .build();
    }

    String featuresTableName = "ff4j-features";
    String propertiesTableName = "ff4jproperties";

    default void createFeaturesTable(final DynamoDbClient dynamoDbClient) {
        try {
            dynamoDbClient.createTable(CreateTableRequest.builder()
                            .tableName(featuresTableName)
                            .attributeDefinitions(
                                    AttributeDefinition.builder()
                                            .attributeName("featureUid")
                                            .attributeType(ScalarAttributeType.S)
                                            .build(),
                                    AttributeDefinition.builder()
                                            .attributeName("groupName")
                                            .attributeType(ScalarAttributeType.S)
                                            .build()
                                    )
                            .keySchema(KeySchemaElement.builder()
                                    .attributeName("featureUid")
                                    .keyType("HASH")
                                    .build())
                            .globalSecondaryIndexes(GlobalSecondaryIndex.builder()
                                    .indexName("ff4j-feature-groups")
                                    .keySchema(KeySchemaElement.builder()
                                            .attributeName("groupName")
                                            .keyType("HASH")
                                            .build())
                                    .projection(Projection.builder()
                                            .projectionType(ProjectionType.ALL)
                                            .build())
                                    .provisionedThroughput(ProvisionedThroughput.builder()
                                            .readCapacityUnits(100L)
                                            .writeCapacityUnits(100L)
                                            .build())
                                    .build())
                            .billingMode(BillingMode.PROVISIONED)
                            .provisionedThroughput(ProvisionedThroughput.builder()
                                    .readCapacityUnits(100L)
                                    .writeCapacityUnits(100L)
                                    .build())
                            .build());
        } catch (Exception e) {
            // Ignored
        }
    }

    default void createPropertiesTable(final DynamoDbClient dynamoDbClient) {
        try {
            dynamoDbClient.createTable(CreateTableRequest.builder()
                            .tableName(propertiesTableName)
                            .attributeDefinitions(
                                    AttributeDefinition.builder()
                                            .attributeName("name")
                                            .attributeType(ScalarAttributeType.S)
                                            .build()
                            )
                            .keySchema(KeySchemaElement.builder()
                                    .attributeName("name")
                                    .keyType("HASH")
                                    .build())
                            .billingMode(BillingMode.PROVISIONED)
                            .provisionedThroughput(ProvisionedThroughput.builder()
                                    .readCapacityUnits(4L)
                                    .writeCapacityUnits(4L)
                                    .build())
                            .build());
        } catch (Exception e) {
            // Ignored
        }
    }


}
