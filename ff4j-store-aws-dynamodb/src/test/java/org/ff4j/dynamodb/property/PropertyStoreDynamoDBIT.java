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

import cloud.localstack.docker.LocalstackDockerTestRunner;
import cloud.localstack.docker.annotation.LocalstackDockerProperties;
import com.amazonaws.client.builder.AwsClientBuilder;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDB;
import com.amazonaws.services.dynamodbv2.AmazonDynamoDBClientBuilder;
import org.ff4j.dynamodb.property.PropertyStoreDynamoDB;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.runner.RunWith;

/**
 * Test using the localstack framework to get AWS stack locally. See https://github.com/localstack/localstack
 * This test needs Docker.
 * If the test fails with timeout, it is probably due to the first download of docker image. In that case, docker pull localstack/localstack.
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
@RunWith(LocalstackDockerTestRunner.class)
@LocalstackDockerProperties(services = {"dynamodb"})
//Needs Docker to be installed which is not there is Travis, as such commenting
@Ignore
public class PropertyStoreDynamoDBIT extends PropertyStoreTestSupport {

    private static AmazonDynamoDB dynamoDB;
    private static PropertyStoreDynamoDB store;

    @BeforeClass
    public static void init() {
        dynamoDB = AmazonDynamoDBClientBuilder.standard()
                .withEndpointConfiguration(new AwsClientBuilder.EndpointConfiguration("http://localhost:4569", "eu-central-1"))
                .build();
        store = new PropertyStoreDynamoDB(dynamoDB);
        store.importPropertiesFromXmlFile("test-ff4j-features.xml");
    }

    @AfterClass
    public static void clean() {
        dynamoDB.shutdown();
    }

    @Override
    protected PropertyStoreDynamoDB initPropertyStore() {
        return store;
    }


}
