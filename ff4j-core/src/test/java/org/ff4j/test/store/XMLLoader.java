package org.ff4j.test.store;

import java.io.IOException;
import java.io.InputStream;
import java.util.LinkedHashMap;
import java.util.Map;

import junit.framework.Assert;
import junit.framework.TestCase;

import org.ff4j.Feature;
import org.ff4j.store.FeatureLoader;
import org.junit.Test;


/**
 * Unit Testing
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class XMLLoader extends TestCase {
	
	@Test
	public void testLoadConfigurationFile() throws IOException {
		InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
		Map < String, Feature> maps = FeatureLoader.loadFeatures(in);
		Assert.assertEquals(5, maps.size());
		Assert.assertEquals(2, maps.get("forth").getAuthorizations().size());
	}
	
	@Test
	public void testExportCOnfiguration() throws IOException {
		// Import
		InputStream in = getClass().getClassLoader().getResourceAsStream("ff4j.xml");
		LinkedHashMap < String, Feature> maps = FeatureLoader.loadFeatures(in);
		FeatureLoader.exportFeatures(maps);
	}

}
