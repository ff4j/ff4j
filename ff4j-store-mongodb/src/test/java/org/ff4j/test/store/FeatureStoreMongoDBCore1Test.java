package org.ff4j.test.store;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.Arrays;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.store.FeatureStoreMongoDB;
import org.ff4j.utils.ParameterUtils;
import org.junit.Rule;

import com.github.fakemongo.junit.FongoRule;

public class FeatureStoreMongoDBCore1Test extends AbstractStoreJUnitTest {

    /**
     * DataBase.
     */
    @Rule
    public FongoRule fongoRule = new FongoRule(false);

    @Override
    protected FeatureStore initStore() {
        FeatureStoreMongoDB storeMongoDB = new FeatureStoreMongoDB(fongoRule.getDB().getCollection("ff4j"));
        storeMongoDB.create(new Feature("AwesomeFeature", true, "some desc"));
        // First
        storeMongoDB.create(new Feature("first", true, "description", null, Arrays.asList("USER")));
        // Second
        storeMongoDB.create(new Feature("second", false, "description", "GRP0", Arrays.asList("USER")));
        // Third
        storeMongoDB.create(new Feature("third", false, "ThirdJDBC", "GRP1", Arrays.asList("ADMINISTRATOR", "BETA-TESTER")));
        // Forth ?? Fourth ?
        FlippingStrategy strategy = new org.ff4j.strategy.el.ExpressionFlipStrategy();
        strategy.init("forth", ParameterUtils.toMap("expression=third|second"));
        storeMongoDB.create(new Feature("forth", true, "ForthJDBC", "GRP1", Arrays.asList("ADMINISTRATOR", "BETA-TESTER"),
                strategy));

        return storeMongoDB;
    }
}
