package org.ff4j.audit.repository;

/*
 * #%L
 * ff4j-core
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

import org.ff4j.audit.Event;
import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;

/**
 * Implementation of in memory {@link EventRepository} with limited events.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcEventRepository extends AbstractEventRepository {

	@Override
	public boolean saveEvent(Event e) {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public PieChart getHitsPieChart(long startTime, long endTime) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BarChart getHitsBarChart(Set<String> featNameSet, long startTime,
			long endTime, int nbslot) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public BarChart getHitsBarChart(long startTime, long endTime, int nbslot) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public PieChart getFeatureHitsPie(String featureId, long startTime,
			long endTime) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public int getTotalEventCount() {
		// TODO Auto-generated method stub
		return 0;
	}

}
