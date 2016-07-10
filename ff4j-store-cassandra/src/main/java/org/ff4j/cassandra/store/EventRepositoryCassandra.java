package org.ff4j.cassandra.store;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;

public class EventRepositoryCassandra extends AbstractEventRepository {

    @Override
    public boolean saveEvent(Event e) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getFeatureUsageHistory(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeFeatureUsage(long starTime, long endTime) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, MutableHitCount> getHostHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getUserHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Map<String, MutableHitCount> getSourceHitCount(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public TimeSeriesChart getAverageResponseTime(long startTime, long endTime, TimeUnit tu, Set<String> filteredFeatures) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public EventSeries getAuditTrail(long startTime, long endTime) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void purgeAuditTrail(long starTime, long endTime) {
        // TODO Auto-generated method stub
        
    }

}
