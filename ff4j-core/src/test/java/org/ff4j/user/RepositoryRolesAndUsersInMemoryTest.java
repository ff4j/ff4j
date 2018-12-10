package org.ff4j.user;

import org.ff4j.user.repository.RolesAndUsersRepository;
import org.ff4j.user.repository.RolesAndUsersRepositoryInMemory;
import org.junit.jupiter.api.DisplayName;

/**
 * Access to user and roles repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@DisplayName("Testing INMEMORY | USERS Repository")

public class RepositoryRolesAndUsersInMemoryTest extends RepositoryRolesAndUsersTestSupport {

    /** {@inheritDoc} */
    @Override
    public RolesAndUsersRepository initStore() {
        return new RolesAndUsersRepositoryInMemory("ff4j-testDataset.xml");
    }

}
