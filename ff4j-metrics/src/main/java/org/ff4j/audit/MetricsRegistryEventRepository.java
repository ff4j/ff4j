package org.ff4j.audit;

/*
 * #%L
 * ff4j-metrics
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


import java.util.Set;

import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.repository.EventRepository;

import com.codahale.metrics.MetricRegistry;

/**
 * Class to TODO
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class MetricsRegistryEventRepository extends MetricRegistry implements EventRepository {

    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        getCounters().get(e.getFeatureName()).inc();
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public PieChart getHitsPieChart(long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public BarChart getHitsBarChart(Set<String> featNameSet, long startTime, long endTime, int nbslot) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public BarChart getHitsBarChart(long startTime, long endTime, int nbslot) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public PieChart getFeatureHitsPie(String featureId, long startTime, long endTime) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public int getTotalEventCount() {
        return 0;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> getFeatureNames() {
        return null;
    }

}
