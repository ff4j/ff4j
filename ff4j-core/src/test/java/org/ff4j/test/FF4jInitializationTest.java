package org.ff4j.test;

import junit.framework.Assert;

import org.ff4j.FF4j;
import org.ff4j.store.InMemoryFeatureStore;
import org.ff4j.test.security.DefaultAuthorisationManager;
import org.junit.Test;

/**
 * Test class for {@link FF4j} initializations.
 * 
 * This test will test FF4J class initializations through different constructor but also with static access.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jInitializationTest {

    /**
     * Factorisation of assertions for different ff4j initializations.
     */
    private void testingGeneratedFF4j(FF4j f) {
        Assert.assertNotNull(f.getStore());
        Assert.assertTrue(f.getStore() instanceof InMemoryFeatureStore);
        Assert.assertFalse(f.isFlipped("new"));
    }

    @Test
    public void testInitConstructor1() {
        FF4j f1 = new FF4j();
        f1.setAutocreate(true);
        testingGeneratedFF4j(f1);
    }

    @Test
    public void testInitConstructor2() {
        FF4j f2 = new FF4j(new InMemoryFeatureStore());
        f2.setAutocreate(true);
        testingGeneratedFF4j(f2);
    }

    @Test
    public void testInitConstructor3() {
        FF4j f3 = new FF4j(new InMemoryFeatureStore(), new DefaultAuthorisationManager());
        f3.setAutocreate(true);
        testingGeneratedFF4j(f3);
        Assert.assertNotNull(f3.getAuthorizationsManager());
    }

    @Test
    public void testInitStatic() {
        testingGeneratedFF4j(FF4j.getInstance());
    }

    @Test
    public void testInitStaticSingleton() {
        FF4j.sInitStore(new InMemoryFeatureStore());
        FF4j.sInitAuthorizationManager(new DefaultAuthorisationManager());
        FF4j.sAutoCreateFeature(true);

        testingGeneratedFF4j(FF4j.getInstance());
        Assert.assertNotNull(FF4j.getInstance().getStore());
        Assert.assertNotNull(FF4j.getInstance().getAuthorizationsManager());
        Assert.assertNotNull(FF4j.sGetFeature("new"));
    }

}
