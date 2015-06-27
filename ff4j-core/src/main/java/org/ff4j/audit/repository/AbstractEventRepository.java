package org.ff4j.audit.repository;

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
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import java.util.Calendar;
import java.util.Date;

import org.ff4j.audit.graph.BarChart;
import org.ff4j.audit.graph.PieChart;
import org.ff4j.audit.graph.PieSector;


/**
 * Superclass implementing the custom serialization.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractEventRepository implements EventRepository { 
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");

        // Today
        Calendar c = Calendar.getInstance();
        c.set(Calendar.HOUR_OF_DAY, 0);
        c.set(Calendar.MINUTE, 0);
        c.set(Calendar.SECOND, 0);
        
        // Tomorrow 00:00
        Calendar c2 = Calendar.getInstance();
        c2.setTime(new Date(System.currentTimeMillis() + 1000 * 3600 * 24));
        c2.set(Calendar.HOUR_OF_DAY, 0);
        c2.set(Calendar.MINUTE, 0);
        c2.set(Calendar.SECOND, 0);
        
        // Create PIE
        PieChart pie = getHitsPieChart(c.getTimeInMillis(), c2.getTimeInMillis());
        sb.append(",\"todayHitsPie\": " + pie.toJson());
        
        // Create BARCHART
        BarChart barChart = getHitsBarChart(c.getTimeInMillis(), c2.getTimeInMillis(), 24);
        sb.append(",\"todayHitsBarChart\": " + barChart.toJson());

        // Total Count
        int total = 0;
        for(PieSector sector : pie.getSectors()) {
            total += sector.getValue();
        }
        sb.append(",\"todayHitTotal\":" + total);
        sb.append("}");
        return sb.toString();
    }
}
