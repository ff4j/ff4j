package org.ff4j.audit;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

/**
 * Different types of events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public enum EventType {

    /** create new feature in the repoitory. */
    CREATE,

    /** delete new feature in the repoitory. */
    DELETE,

    /** enable feature. */
    ENABLE,

    /** disable feature. */
    DISABLE,

    /** enable group. */
    ENABLE_GROUP,

    /** disable group. */
    DISABLE_GROUP,

    /** flipped. */
    HIT_FLIPPED,

    /** not flipped. */
    HIT_NOT_FLIPPED

}
