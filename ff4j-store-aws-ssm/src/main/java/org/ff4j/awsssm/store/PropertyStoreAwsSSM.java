package org.ff4j.awsssm.store;

/*
 * #%L
 * ff4j-store-aws-ssm
 * %%
 * Copyright (C) 2013 Ff4J
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

import com.amazonaws.services.simplesystemsmanagement.AWSSimpleSystemsManagement;
import com.amazonaws.services.simplesystemsmanagement.AWSSimpleSystemsManagementClientBuilder;
import com.amazonaws.services.simplesystemsmanagement.model.*;
import org.ff4j.conf.XmlParser;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

import java.io.InputStream;
import java.util.*;

/**
 * Implementation of {@link PropertyStore} with Amazon Web Services SSM Parameter Store.
 * (<a href="https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-paramstore.html">SSM Parameter Store</a>)
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class PropertyStoreAwsSSM extends AbstractPropertyStore {

    private AWSSimpleSystemsManagement client;
    private String path;

    /**
     * Default constructor with default AWS configuration. <br />
     * If you need more control on AWS connection (credentials, proxy, ...), use {@link #PropertyStoreAwsSSM(AWSSimpleSystemsManagement, String)}
     * @param path Path of the properties (eg. "/Path/to/properties"). Must start with "/" and must not finish with "/"
     */
    public PropertyStoreAwsSSM(String path) {
        this(AWSSimpleSystemsManagementClientBuilder.defaultClient(), path);
    }

    /**
     * Constructor with custom builder for advanced connection to AWS environment
     * @param client AWS SSM client
     * @param path Path of the properties (eg. "/Path/to/properties"). Must start with "/" and must not finish with "/"
     */
    public PropertyStoreAwsSSM(AWSSimpleSystemsManagement client, String path) {
        Util.assertNotNull(client);
        Util.assertNotNull(path);
        if (!path.startsWith("/") || path.endsWith("/")) {
            throw new IllegalArgumentException("[Assertion failed] - path must start with '/' and not finish with '/'");
        }
        this.path = path;
        this.client = client;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        try {
            readProperty(name);
            return true;
        } catch (PropertyNotFoundException e) {
            return false;
        }
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
        createOrUpdate(value, false);
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        Util.assertHasLength(name);
        try {
            GetParameterResult result = client.getParameter(new GetParameterRequest().withName(path + "/" + name));
            return new PropertyString(name, result.getParameter().getValue());
        } catch (ParameterNotFoundException e) {
            throw new PropertyNotFoundException(name);
        } catch (ParameterVersionNotFoundException e) {
            throw new PropertyNotFoundException(name);
        }
    }

    /** {@inheritDoc} */
    @Override
    public <T> void updateProperty(Property<T> prop) {
        createOrUpdate(prop, true);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        Util.assertHasLength(name);
        try {
            client.deleteParameter(new DeleteParameterRequest().withName(path + "/" + name));
        } catch (ParameterNotFoundException e) {
            throw new PropertyNotFoundException(name);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        List<Parameter> parameters = getParameters();

        Map<String, Property<?>> properties = new HashMap<String, Property<?>>();
        for (Parameter parameter : parameters) {
            properties.put(parameter.getName().replace(path + "/", ""), new PropertyString(parameter.getValue()));
        }
        return properties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        List<Parameter> parameters = getParameters();

        Set<String> propertyNames = new HashSet<String>();
        for (Parameter parameter : parameters) {
            propertyNames.add(parameter.getName().replace(path + "/", ""));
        }
        return propertyNames;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        Set<String> names = listPropertyFullNames();
        if (!Util.isEmpty(names)) {
            client.deleteParameters(new DeleteParametersRequest().withNames(names));
        }
    }

    /**
     * Load configuration through FF4J.vml file.
     *
     * @param fileName
     *            xml filename
     */
    public void loadFromXMLFile(String fileName) {
        loadFromXMLFileStream(getClass().getClassLoader().getResourceAsStream(fileName));
    }

    /**
     * Load configuration through FF4J.vml file.
     *
     * @param xmlIN
     *            xml file input stream
     */
    public void loadFromXMLFileStream(InputStream xmlIN) {
        if (xmlIN == null) {
            throw new IllegalArgumentException("Cannot parse stream with properties");
        }
        Map<String, Property<?>> properties = new XmlParser().parseConfigurationFile(xmlIN).getProperties();
        for (String prop : properties.keySet()) {
            createProperty(properties.get(prop));
        }
    }

    private <T> void createOrUpdate(Property<T> property, boolean overwrite) {
        Util.assertNotNull(property);
        Util.assertHasLength(property.getName());
        if (overwrite && !existProperty(property.getName())) {
            throw new PropertyNotFoundException(property.getName());
        }
        try {
            client.putParameter(new PutParameterRequest()
                    .withName(path + "/" + property.getName())
                    .withType(ParameterType.String)
                    .withValue(property.asString())
                    .withOverwrite(overwrite)
                    .withDescription(property.getDescription()));
        } catch (ParameterAlreadyExistsException pae) {
            throw new PropertyAlreadyExistException(property.getName());
        }
    }

    private Set<String> listPropertyFullNames() {
        List<Parameter> parameters = getParameters();

        Set<String> propertyNames = new HashSet<String>();
        for (Parameter parameter : parameters) {
            propertyNames.add(parameter.getName());
        }
        return propertyNames;
    }

    private List<Parameter> getParameters() {
        GetParametersByPathResult result = client.getParametersByPath(new GetParametersByPathRequest().withPath(path).withRecursive(false));
        return result.getParameters();
    }

}
