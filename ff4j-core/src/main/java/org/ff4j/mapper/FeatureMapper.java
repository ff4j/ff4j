package org.ff4j.mapper;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import org.ff4j.core.Feature;

/**
 * Specialization of the interface.
 *
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <STORE_OBJ>
 *      target driver object.
 */
public interface FeatureMapper <STORE_OBJ> extends Mapper< Feature, STORE_OBJ> {}
