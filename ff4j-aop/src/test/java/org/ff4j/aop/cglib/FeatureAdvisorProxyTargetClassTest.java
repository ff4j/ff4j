package org.ff4j.aop.cglib;

/*-
 * #%L
 * ff4j-aop
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

import org.ff4j.FF4j;
import org.ff4j.aop.test.greeting.GreetingService;
import org.ff4j.aop.test.wholeclass.WholeClassFlipping;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

/**
 * Reproduces GitHub issue #761: {@code @Flip} does not switch to {@code alterBean} when
 * Spring uses CGLIB proxies (the default in Spring Boot 4 with
 * {@code spring.aop.proxy-target-class=true}).
 *
 * <p>The {@code proxyTargetClass} profile activates {@link ProxyTargetClassConfiguration}
 * which sets {@code proxyTargetClass=true} on {@code ff.autoproxy}, causing {@link org.ff4j.aop.FeatureAutoProxy}
 * to create CGLIB subclass proxies instead of JDK interface proxies.
 */
@ActiveProfiles("proxyTargetClass")
@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class FeatureAdvisorProxyTargetClassTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("greeting.english")
    private GreetingService greeting;

    @Autowired
    @Qualifier("whole.english")
    private WholeClassFlipping wholeClassFlipping;

    @Before
    public void createFeatures() {
        if (!ff4j.exist("language-english")) {
            ff4j.createFeature("language-english");
        }
        if (!ff4j.exist("language-french")) {
            ff4j.createFeature("language-french");
        }
    }

    @After
    public void disableFeatures() {
        ff4j.disable("language-french");
        ff4j.disable("language-english");
    }

    /**
     * Verifies that the greeting bean is a CGLIB proxy — confirming the test exercises
     * the correct code path (the root cause of issue #761).
     */
    @Test
    public void testBeanIsProxiedWithCglib() {
        Assert.assertTrue("Expected a CGLIB proxy but got: " + greeting.getClass(),
                AopUtils.isCglibProxy(greeting));
    }

    /**
     * Issue #761: when the feature is enabled, the alter bean must be called even though
     * the proxy is CGLIB and {@code @Flip} lives on the interface method.
     */
    @Test
    public void testAlterBeanCalledWhenFeatureEnabledUnderCglibProxy() {
        ff4j.disable("language-french");
        Assert.assertTrue("Expected English greeting", greeting.sayHello("CLU").startsWith("Hello"));

        ff4j.enable("language-french");
        Assert.assertTrue("Service did not flip to French alter bean under CGLIB proxying",
                greeting.sayHello("CLU").startsWith("Bonjour"));
    }

    /**
     * Verifies that class-level {@code @Flip} on an interface also works under CGLIB proxying.
     */
    @Test
    public void testClassLevelFlipOnInterfaceWorksUnderCglibProxy() {
        Assert.assertTrue(wholeClassFlipping.hello1().startsWith("Hello"));
        Assert.assertTrue(wholeClassFlipping.hello2().startsWith("Big"));

        ff4j.enable("language-french");

        Assert.assertTrue("Class-level @Flip did not flip hello1 under CGLIB",
                wholeClassFlipping.hello1().startsWith("Francais"));
        Assert.assertTrue("Class-level @Flip did not flip hello2 under CGLIB",
                wholeClassFlipping.hello2().startsWith("Tour"));
    }
}
