package org.ff4j.web.services.exception;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 Ff4J
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

import javax.ws.rs.core.Response.Status;
import javax.ws.rs.ext.Provider;

/**
 * Once Runtime Error occur, return 500 with only the error message
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Provider
public final class RuntimeExceptionMapper extends AbstractExceptionMapper<RuntimeException> {

    /** {@inheritdoc} */
    @Override
    public Status getStatus() {
        return Status.INTERNAL_SERVER_ERROR;
    }

}