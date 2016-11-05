package org.ff4j.elastic.optional;

/*
 * #%L
 * ff4j-store-elastic
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


import java.util.Optional;

public class OptionalUtils {

	public static <T> T of(Object value, T defaultValue) {
		Optional<Object> tmp = Optional.ofNullable(value);
		if (!tmp.isPresent()) {
			return defaultValue;
		}
		Object gettingValue = tmp.get();
		if (gettingValue instanceof String) {
			return (T) String.valueOf(gettingValue);
		} else if (gettingValue instanceof Integer) {
			return (T) ((Integer) gettingValue);
		}
		return defaultValue;
	}
}
