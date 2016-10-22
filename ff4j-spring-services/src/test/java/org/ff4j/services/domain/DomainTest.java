package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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

import org.junit.Test;
import org.meanbean.test.BeanTester;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class DomainTest {

    private static final BeanTester BEAN_TESTER = new BeanTester();

    @Test
    public void getterAndSetterCorrectness() throws Exception {
        BEAN_TESTER.testBean(FeatureApiBean.class);
        BEAN_TESTER.testBean(FeatureStoreApiBean.class);
        BEAN_TESTER.testBean(PropertyStoreApiBean.class);
        BEAN_TESTER.testBean(FlippingStrategyApiBean.class);
        BEAN_TESTER.testBean(BarChartApiBean.class);
        BEAN_TESTER.testBean(PieChartApiBean.class);
        BEAN_TESTER.testBean(AuthorizationsManagerApiBean.class);
        BEAN_TESTER.testBean(PropertyApiBean.class);
        BEAN_TESTER.testBean(FF4jStatusApiBean.class);
        BEAN_TESTER.testBean(EventRepositoryApiBean.class);
        BEAN_TESTER.testBean(CacheApiBean.class);
        BEAN_TESTER.testBean(GroupDescApiBean.class);
        BEAN_TESTER.testBean(BarSeriesApiBean.class);
        BEAN_TESTER.testBean(PieSectorApiBean.class);
    }

}
