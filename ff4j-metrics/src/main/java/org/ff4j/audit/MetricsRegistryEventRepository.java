package org.ff4j.audit;

import java.util.Set;

import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.repository.EventRepository;

import com.codahale.metrics.MetricRegistry;

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

}
