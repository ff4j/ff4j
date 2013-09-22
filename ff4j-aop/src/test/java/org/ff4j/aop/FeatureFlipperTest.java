package org.ff4j.aop;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.junit.After;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class FeatureFlipperTest {

    @Autowired
    @Qualifier("greeting.english")
    private GreetingService greeting;

    @Autowired
    @Qualifier("goodbye.french")
    private GoodbyeService goodbye;

    // Working With singleton
    private final FF4j ff4j = FF4j.getInstance();

    @BeforeClass
    public static void createFeatures() {
        FF4j.getInstance().autoCreateFeature();
        FF4j.getInstance().createFeature("language-english").createFeature("language-french");

    }

    @After
    public void disableFeatures() {
        ff4j.disableFeature("language-french");
        ff4j.disableFeature("language-english");
    }

    @Test
    public void testAnnotatedFlipping_with_alterBean() {
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));

        FF4j.getInstance().enableFeature("language-french");
        Assert.assertTrue("Service did not flipped", greeting.sayHello("CLU").startsWith("Bonjour"));
    }

    @Test
    @Ignore
    public void testAnnotatedFlipping_with_alterClazz() {
        Assert.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Hi"));
        FF4j.sEnableFeature("language-french");
        Assert.assertTrue("Service did not flipped", greeting.sayHelloWithClass("CLU").startsWith("Salut"));
    }

    @Test
    public void testAnnotatedFlipping_if_qualified_implementation_is_not_the_first_class_qualified_name_in_natural_ordering() {
        Assert.assertTrue(goodbye.sayGoodbye("CLU").startsWith("Au revoir"));

        FF4j.sEnableFeature("language-english");
        Assert.assertTrue("Service did not flipped", goodbye.sayGoodbye("CLU").startsWith("Goodbye"));
    }

    @Test
    @Ignore
    public void testAnnotatedFlipping_with_alterClazz_if_qualified_implementation_is_not_the_first_class_qualified_name_in_natural_ordering() {
        Assert.assertTrue(goodbye.sayGoodbyeWithClass("CLU").startsWith("A plus"));

        FF4j.sEnableFeature("language-english");
        Assert.assertTrue("Service did not flipped", goodbye.sayGoodbyeWithClass("CLU").startsWith("See you"));
    }
}