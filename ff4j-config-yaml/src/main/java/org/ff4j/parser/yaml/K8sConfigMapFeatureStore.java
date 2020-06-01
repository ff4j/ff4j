package org.ff4j.parser.yaml;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2020 FF4J
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

import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.store.AbstractFeatureStore;

/**
 * Read Feature from Environment values.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class K8sConfigMapFeatureStore extends AbstractFeatureStore {
    
    /** Feature prefix. */
    private String prefixKeyFeature = "ff4j.feature";
    
    /** Property prefix. */
    private String prefixKeyProperty = "ff4j.property";
    
    /** Default constructor. */
    public K8sConfigMapFeatureStore() {}
    
    /**
     * Constructor with configuration fileName.
     * 
     * @param fileName
     *            fileName present in classPath or on fileSystem.
     */
    public K8sConfigMapFeatureStore(String pFeat, String pProp, String fileName) {
        if (fileName == null || fileName.isEmpty()) {
            throw new IllegalArgumentException(
                    "fileName is required, cannot be null nor empty : the file must exist in classpath");
        }
        //InputStream is = getClass().getClassLoader().getResourceAsStream(fileName);
        //XmlConfig config = new XmlParser().parseConfigurationFile(is);
    }

    @Override
    public boolean exist(String featId) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Feature read(String featureUid) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    /**
     * Getter accessor for attribute 'prefixKeyFeature'.
     *
     * @return
     *       current value of 'prefixKeyFeature'
     */
    public String getPrefixKeyFeature() {
        return prefixKeyFeature;
    }

    /**
     * Setter accessor for attribute 'prefixKeyFeature'.
     * @param prefixKeyFeature
     * 		new value for 'prefixKeyFeature '
     */
    public void setPrefixKeyFeature(String prefixKeyFeature) {
        this.prefixKeyFeature = prefixKeyFeature;
    }

    /**
     * Getter accessor for attribute 'prefixKeyProperty'.
     *
     * @return
     *       current value of 'prefixKeyProperty'
     */
    public String getPrefixKeyProperty() {
        return prefixKeyProperty;
    }

    /**
     * Setter accessor for attribute 'prefixKeyProperty'.
     * @param prefixKeyProperty
     * 		new value for 'prefixKeyProperty '
     */
    public void setPrefixKeyProperty(String prefixKeyProperty) {
        this.prefixKeyProperty = prefixKeyProperty;
    }
    

}
