package org.ff4j.aop;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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
 * Enumeration that represents the location of the flipping context execution
 * which will be passed to the flipping strategy.
 *
 * {@see Flip} annotation
 * {@see FlippingExecutionContext}
 */
public enum ContextLocation {
    /**
     * {@link Flip} will not use a {@link org.ff4j.core.FlippingExecutionContext} to switch the feature.
     */
    NONE,

    /**
     * {@link Flip} will use the first method parameter that is an instance of {@link org.ff4j.core.FlippingExecutionContext}.
     */
    PARAMETER,

    /**
     * {@link Flip} will use the {@link org.ff4j.FF4j} instance to retrieve the current context.
     */
    FF4J
}
