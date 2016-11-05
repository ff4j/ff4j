package org.ff4j.elastic.utils;

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

/**
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 *
 */
import java.util.Optional;

import io.searchbox.client.JestResult;
import io.searchbox.core.SearchResult;

public class ResultUtils {

	public static boolean exists(JestResult result) {
		Optional<JestResult> tmp = Optional.ofNullable(result);
		if (!tmp.isPresent() || !tmp.get().isSucceeded()) {
			return false;
		}
		return ((SearchResult) tmp.get()).getTotal() >= 1;
	}

	public static boolean isSucceeded(JestResult result) {
		Optional<JestResult> tmp = Optional.ofNullable(result);
		if (!tmp.isPresent()) {
			return false;
		}
		return tmp.get().isSucceeded();
	}
}
