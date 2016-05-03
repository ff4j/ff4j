package org.ff4j.placeholder;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.PropertyInt;
import org.ff4j.spring.autowire.FF4JFeature;
import org.ff4j.spring.autowire.FF4JProperty;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-placeholder.xml")
public class PlaceHolderTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    private SampleBean sb;

    // --------------------------------------------

    @FF4JProperty("pInt")
    private PropertyInt pro;

    @FF4JProperty("pInt")
    private Integer pro2 = 2;

    @FF4JProperty("pInt")
    private int pro3 = 3;

    @FF4JProperty(value = "toto", required = false)
    private PropertyInt pro4 = null;

    @FF4JProperty(value = "pExo")
    private PropertyExotic exo;

    @FF4JFeature("AwesomeFeature")
    private Feature feat = null;
    ;

    @FF4JFeature("AwesomeFeature")
    private Boolean feat2 = false;

    @FF4JFeature("AwesomeFeature")
    private boolean feat3 = false;

    @FF4JFeature(value = "i-dont-exist", required = false)
    private Feature feat4 = null;

    @Test
    public void testPlaceholderWithinXMLFile() {
        // Given, configuration loaded
        Assert.assertNotNull(ff4j);
        Assert.assertNotNull(ff4j.getPropertiesStore());
        Assert.assertNotNull(ff4j.getFeatureStore());
        // When (injection)
        // Then
        Assert.assertEquals(1, sb.getP());
        Assert.assertEquals(true, sb.isF());
    }

    @Test
    public void testPlaceHolderWithAnnotation() {
        Assert.assertEquals(new Integer(1), pro.getValue());
        Assert.assertEquals(new Integer(1), pro2);
        Assert.assertEquals(1, pro3);
        Assert.assertNull(pro4);

        Assert.assertEquals(true, feat.isEnable());
        Assert.assertEquals(true, feat2);
        Assert.assertEquals(true, feat3);
        Assert.assertNull(feat4);
        Assert.assertNotNull(exo);
    }

}