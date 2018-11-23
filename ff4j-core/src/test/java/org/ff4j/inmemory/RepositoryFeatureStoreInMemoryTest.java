package org.ff4j.inmemory;

import org.ff4j.feature.repository.FeaturesRepository;
import org.ff4j.feature.repository.FeaturesRepositoryInMemory;

/**
 * Testing implementation of {@link FeaturesRepository} for DB : MEMORY
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class RepositoryFeatureStoreInMemoryTest extends RepositoryFeaturesTestSupport {

    /** {@inheritDoc} */
    @Override
    public FeaturesRepository initStore() {
        return new FeaturesRepositoryInMemory("ff4j-testDataset.xml");
    }
    
}
