package org.ff4j.security;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.util.Optional;
import java.util.stream.Stream;

import org.ff4j.monitoring.AuditTrail;
import org.ff4j.repository.FF4jRepositoryListener;
import org.ff4j.repository.FF4jRepositorySupport;
import org.ff4j.security.domain.FF4jUser;

/**
 * Superclass as helper to implements user repository.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class AbstractRepositoryUsers 
    extends FF4jRepositorySupport < FF4jUser , FF4jRepositoryListener< FF4jUser >> 
    implements RepositoryUsers {

    /** serialVersionUID. */
    private static final long serialVersionUID = 2472380934533153376L;

    /** {@inheritDoc} */
    @Override
    public void delete(String entityId) {
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists(String id) {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public Stream<FF4jUser> findAll() {
        return Stream.empty();
    }

    /** {@inheritDoc} */
    @Override
    public Optional<FF4jUser> findById(String id) {
        return Optional.empty();
    }

    /** {@inheritDoc} */
    @Override
    public void create(FF4jUser entity) {
    }

    /** {@inheritDoc} */
    @Override
    public void update(FF4jUser entity) {
    }

    /** {@inheritDoc} */
    @Override
    public void registerAuditListener(AuditTrail auditTrail) {
    }

    /** {@inheritDoc} */
    @Override
    public void unRegisterAuditListener() {
    }

}
