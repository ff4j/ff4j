package org.ff4j.audit.repository;

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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.ConcurrentHashMap;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.BarSeries;
import org.ff4j.audit.graph.MutableInt;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;
import org.ff4j.utils.Util;

import static org.ff4j.audit.EventConstants.*;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class InMemoryEventRepository extends AbstractEventRepository {
    
    /** default retention. */
    private static final int DEFAULT_QUEUE_CAPACITY = 100000;

    /** current capacity. */
    private int queueCapacity = DEFAULT_QUEUE_CAPACITY;

    /** Store : < TARGET | UID | Event > */
    private final Map<String, Map < String, Queue<Event> > > events = 
            new ConcurrentHashMap<String,  Map < String, Queue<Event>>>();
    
    /**
     * Default constructor with default capacity to 100.000
     */
    public InMemoryEventRepository() {
        this(DEFAULT_QUEUE_CAPACITY);
    }

    /**
     * Constructor to tune capacity.
     * 
     * @param queueCapacity
     *            default queue capacity
     */
    public InMemoryEventRepository(int queueCapacity) {
        this.queueCapacity = queueCapacity;
    }

    /** {@inheritDoc} */
    public boolean saveEvent(Event e) {
        Queue<Event> myQueue = getQueue(e);
        if (myQueue.size() >= queueCapacity) {
            myQueue.poll();
        }
        return myQueue.offer(e);
    }

    /** {@inheritDoc} */
    @Override
	public List<Event> search(EventQueryDefinition query) {
		List < Event > targetEvents = new ArrayList<Event>();
		if (query != null) {
			// Loop over all events
			for (Map.Entry<String , Map < String, Queue<Event> > > entry : events.entrySet()) {
				for(Map.Entry<String ,  Queue<Event> > entry2 : entry.getValue().entrySet()) {
					for (Event evt : entry2.getValue()) {
						targetEvents.add(evt);
					}
				}
				/* Filter over target if exist
				if (query.getTargetsFilter() == null || 
					query.getTargetsFilter().isEmpty() ||  
					query.getTargetsFilter().contains(entry.getKey())) {
					
					Map < String, Queue<Event> > elements = entry.getValue();
					for(Map.Entry<String ,  Queue<Event> > entry2 : elements.entrySet()) {
						
						// Filter over UID if exist
						if (query.getNamesFilter() == null || 
							query.getNamesFilter().isEmpty() ||  
							query.getNamesFilter().contains(entry2.getKey())) {
							
							// Loop in the Queue
				            for (Event evt : entry2.getValue()) {
				            	
				            	// Filter over Action if expected
				            	if (query.getActionFilter() == null || 
										query.getActionFilter().isEmpty() ||  
										query.getActionFilter().contains(evt.getAction())) {
				            		
				            		// Filter overTime
				            		if (isEventInInterval(query, evt)) {
				            			targetEvents.add(evt);
				            		}
				            	}
				            }
						}
					}
				}*/
			}
		}
		return targetEvents;
	}
    
    /**
     * Retrieve event for same feature and name.
     *
     * @param e
     *      current event
     * @return
     *      nouvel queue
     */
    private Queue<Event> getQueue(Event e) {
        if (!events.containsKey(e.getType())) {
            events.put(e.getType(), new ConcurrentHashMap<String, Queue<Event>>());
        }
        Map < String, Queue<Event>> mapOfQueues = events.get(e.getType());
        if (!mapOfQueues.containsKey(e.getName())) {
            mapOfQueues.put(e.getName(), new ArrayBlockingQueue<Event>(queueCapacity));
        }
        return mapOfQueues.get(e.getName());
    }
    
    /** {@inheritDoc} */
    public PieChart featuresListDistributionPie(long startTime, long endTime) {
        PieChart pieGraph = new PieChart(TITLE_PIE_HITCOUNT);
        Map < String, Queue<Event>> eventsFeatures = events.get(TARGET_FEATURE);
        if (eventsFeatures != null) {
            List < String > colors   = Util.getColorsGradient(eventsFeatures.size());
            List < String > features = new ArrayList<String>(eventsFeatures.keySet());
            for(int idx = 0; idx < eventsFeatures.size();idx++) {
                Queue< Event > qEvents = eventsFeatures.get(features.get(idx));
                int counter = 0;
                for (Event evt : qEvents) {
                    if (isEventOK(evt, startTime, endTime)) {
                        counter++;
                    }
                }
                pieGraph.getSectors().add(new PieSector(features.get(idx), counter, colors.get(idx)));
            }
        }
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    public PieChart featureDistributionPie(String uid, long startTime, long endTime) {
        Queue< Event > queueOfEvents = events.get(TARGET_FEATURE).get(uid);
        
        // Loop over event and keep those in the time windows
        Map < String , MutableInt > freq = new HashMap<String, MutableInt>();
        for (Event evt : queueOfEvents) {
            if (evt.getTimestamp() > startTime && evt.getTimestamp() < endTime) {
                MutableInt count = freq.get(evt.getAction());
                if (count == null) {
                    freq.put(evt.getAction(), new MutableInt());
                }
                else {
                    count.inc();
                }
            }
        }
        
        // Create PieCharts
        PieChart pieGraph = new PieChart("Hits Count for " + uid);
        List < String > colors = Util.getColorsGradient(freq.size());
        int idx = 0;
        for (String action:freq.keySet()) {
            pieGraph.getSectors().add(new PieSector(action, freq.get(action).get(), colors.get(idx)));
            idx++;
        }
        return pieGraph;
    }
    
    /** {@inheritDoc} */
    public BarChart getFeaturesUsageOverTime(Set < String > featNameSet, long startTime, long endTime, int nbslot) {
        
        // Build Labels
        BarChart barChart = initFeaturesOverTimeBarchart(featNameSet, startTime, endTime, nbslot);
       
        // Build Statistics, loop over each serie
        Map < String, Queue< Event > > eventsPerFeatures = events.get(TARGET_FEATURE);
        
        if (eventsPerFeatures != null) {
            long slotWitdh = (endTime - startTime) / nbslot;
            for (String name : eventsPerFeatures.keySet()) {
                
              // Retrieve events for target feature
              Queue<Event> myQueue = eventsPerFeatures.get(name);
              
              // Create series for this feature (even if not present)
              BarSeries currentSeries = barChart.getSeries().get(name);
              if (myQueue != null) {
                  for (Iterator<Event> itEvt = myQueue.iterator(); itEvt.hasNext();) {
                     Event evt = itEvt.next();
                     long t = evt.getTimestamp();
                     if (isEventOK(evt, startTime, endTime)) {
                          currentSeries.incrCount((int) ((t - startTime) / slotWitdh));
                     }   
                  }
               }
            }
        }
        return barChart;
    }

    /** {@inheritDoc} */
    public int getTotalEventCount() {
        int total = 0;
        for(String evt : events.keySet()) {
            for(String name : events.get(evt).keySet()) {
                total += events.get(evt).get(name).size();
            }
        }
        return total;
    }

    /** {@inheritDoc} */
    public Set<String> getFeatureNames() {
        if (!events.containsKey(TARGET_FEATURE)) return new HashSet<String>();
        return events.get(TARGET_FEATURE).keySet();
    }
	
}
