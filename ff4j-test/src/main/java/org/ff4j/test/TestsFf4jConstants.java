package org.ff4j.test;

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
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

/**
 * For coherence, each store implementation will be tested with same dataset. Here are the constants contains in this DATASET.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class TestsFf4jConstants {

    /** Initial feature number. */
    public static final int EXPECTED_FEATURES_NUMBERS = 5;

    /** Feature Name. */
    public static final String F1 = "first";

    /** Feature Name. */
    public static final String F2 = "second";

    /** Feature Name. */
    public static final String F3 = "third";

    /** Feature Name. */
    public static final String F4 = "forth";

    /** Feature Name. */
    public static final String AWESOME = "AwesomeFeature";

    /** Custom property name. */
    public static final String CUSTOM_PROPERTY = "KEY-1";

    /** Group Name. */
    public static final String G0 = "GRP0";

    /** Group Name. */
    public static final String G1 = "GRP1";

    /** Group Name. */
    public static final String F_DOESNOTEXIST = "invalid-feature-id";

    /** Group Name. */
    public static final String G_DOESNOTEXIST = "invalid-group-name";

    /** Feature Name. */
    public static final String FEATURE_NEW = "new";

    /** Feature Name. */
    public static final String FEATURE_X = "x";

    /** Feature Name. */
    public static final String ROLE_USER = "USER";

    /** Feature Name. */
    public static final String ROLE_ADMIN = "ADMINISTRATOR";

    /** Feature Name. */
    public static final String ROLE_TEST = "BETA-TESTER";

    /** Feature Name. */
    public static final String ROLE_NEW = "ROLE_NEW";

    /** Test file. */
    public static final String TEST_FEATURES_FILE = "test-ff4j-features.xml";

    private TestsFf4jConstants() {}

}
