package org.ff4j.awsssm.store;

/*-
 * #%L
 * ff4j-store-aws-ssm
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

import org.ff4j.conf.XmlParser;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import software.amazon.awssdk.http.urlconnection.UrlConnectionHttpClient;
import software.amazon.awssdk.services.ssm.SsmClient;
import software.amazon.awssdk.services.ssm.model.*;

import java.io.InputStream;
import java.util.*;

/**
 * Implementation of {@link PropertyStore} with Amazon Web Services SSM Parameter Store.
 * (<a href="https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-paramstore.html">SSM Parameter Store</a>)
 *
 * @author <a href="mailto:jeromevdl@gmail.com">Jerome VAN DER LINDEN</a>
 */
public class PropertyStoreAwsSSM extends AbstractPropertyStore {

    private SsmClient client;
    private String path;

    /**
     * Default constructor with default AWS configuration. <br />
     * If you need more control on AWS connection (credentials, proxy, ...), use {@link #PropertyStoreAwsSSM(SsmClient, String)}
     * @param path Path of the properties (eg. "/Path/to/properties"). Must start with "/" and must not finish with "/"
     */
    public PropertyStoreAwsSSM(String path) {
        this(SsmClient.builder().httpClientBuilder(UrlConnectionHttpClient.builder()).build(), path);
    }

    /**
     * Constructor with custom builder for advanced connection to AWS environment
     * @param client AWS SSM client
     * @param path Path of the properties (eg. "/Path/to/properties"). Must start with "/" and must not finish with "/"
     */
    public PropertyStoreAwsSSM(SsmClient client, String path) {
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
            GetParameterResponse response = client.getParameter(builder -> builder.name(path + "/" + name));
            return new PropertyString(name, response.parameter().value());
        } catch (ParameterNotFoundException e) {
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
            client.deleteParameter(builder -> builder.name(path + "/" + name));
        } catch (ParameterNotFoundException e) {
            throw new PropertyNotFoundException(name);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        List<Parameter> parameters = getParameters();

        Map<String, Property<?>> properties = new HashMap<>();
        for (Parameter parameter : parameters) {
            properties.put(parameter.name().replace(path + "/", ""), new PropertyString(parameter.value()));
        }
        return properties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        List<Parameter> parameters = getParameters();

        Set<String> propertyNames = new HashSet<>();
        for (Parameter parameter : parameters) {
            propertyNames.add(parameter.name().replace(path + "/", ""));
        }
        return propertyNames;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        Set<String> names = listPropertyFullNames();
        if (!Util.isEmpty(names)) {
            client.deleteParameters(builder -> builder.names(names));
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
            client.putParameter(builder -> builder
                    .name(path + "/" + property.getName())
                    .type(ParameterType.STRING)
                    .value(property.asString())
                    .overwrite(overwrite)
                    .description(property.getDescription()));
        } catch (ParameterAlreadyExistsException pae) {
            throw new PropertyAlreadyExistException(property.getName());
        }
    }

    private Set<String> listPropertyFullNames() {
        List<Parameter> parameters = getParameters();

        Set<String> propertyNames = new HashSet<String>();
        for (Parameter parameter : parameters) {
            propertyNames.add(parameter.name());
        }
        return propertyNames;
    }

    private List<Parameter> getParameters() {
        GetParametersByPathResponse response = client.getParametersByPath(builder -> builder.path(path).recursive(false));
        return response.parameters();
    }

}
