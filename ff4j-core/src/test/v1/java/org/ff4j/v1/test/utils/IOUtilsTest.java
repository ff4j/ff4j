package org.ff4j.v1.test.utils;

import org.ff4j.v1.utils.Util;
import org.junit.Assert;
import org.junit.Test;

import IOUtil;

public class IOUtilsTest {
    
    @Test
    public void testInit() throws Exception {
        Assert.assertNotNull(Util.instanciatePrivate(IOUtil.class));
    }
    
    @Test
    public void testResolveOK() throws Exception {
        IOUtil.setUseInetAddress(true);
        IOUtil.resolveHostName();
        Assert.assertTrue(IOUtil.isUseInetAddress());
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testResolveKO() throws Exception {
        IOUtil.setUseInetAddress(false);
        IOUtil.resolveHostName();
        IOUtil.setUseInetAddress(true);
        Assert.fail();
    }

}
