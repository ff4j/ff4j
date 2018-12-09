package org.ff4j.feature.togglestrategy.time;

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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.feature.togglestrategy.ToggleContext;
import org.ff4j.property.PropertyListString;

/**
 * Implemenetation of an office hour strategy.
 * 
 * Expression { "monday":["08:00-12:00", "13:30-18:00"], "tuesday":[], "wednesday":[], "thursday":[],"friday":[],"saturday":[] }
 *
 * @author Cedrick Lunven (@clunven)
 */
public class OfficeHourToggleStrategy extends AbstractToggleStrategy {
    
    /** Serial. */
    private static final long serialVersionUID = -812341519896298400L;

    /** Parsing date expression. */
    private static final DateFormat SDF_DATE = new SimpleDateFormat("yyyy-MM-dd");
    
    /** Constants. */
    public static final String MONDAY           = "monday";
    public static final String TUESDAY          = "tuesday";
    public static final String WEDNESDAY        = "wednesday";
    public static final String THURSDAY         = "thursday";
    public static final String FRIDAY           = "friday";
    public static final String SATURDAY         = "saturday";
    public static final String SUNDAY           = "sunday";
    public static final String PUBLICHOLIDAY    = "publicHolidays";
    public static final String SPECIAL_OPENINGS = "specialOpenings";
    public static final String OVERRIDE_DATE    = "overridedDate";
    
    /** time table. */
    private Map < Integer, List <HourInterval>> weekTimeTable = new HashMap<Integer, List<HourInterval>>();
    
    /** openings. */
    private Map < String, List < HourInterval>> specialTimeTable = new HashMap< String, List<HourInterval>>();
    
    /** public holiday. */
    private List < String > publicHolidays = new ArrayList<String>();
   
    /** {@inheritDoc} */
    @Override
    public void initialize() {
        
        // Update Days
        getProperty(MONDAY).ifPresent( 
                p -> weekTimeTable.put(Calendar.MONDAY, parseIntervalsExpression(p.asString())));
        getProperty(TUESDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.TUESDAY, parseIntervalsExpression(p.asString())));
        getProperty(WEDNESDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.WEDNESDAY, parseIntervalsExpression(p.asString())));
        getProperty(THURSDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.THURSDAY, parseIntervalsExpression(p.asString())));
        getProperty(FRIDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.FRIDAY, parseIntervalsExpression(p.asString())));
        getProperty(SATURDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.SATURDAY, parseIntervalsExpression(p.asString())));
        getProperty(SUNDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.SUNDAY, parseIntervalsExpression(p.asString())));
        
        // Update publiholidays
        getProperty(PUBLICHOLIDAY).ifPresent(publicHoliday -> {
            PropertyListString pls = (PropertyListString) publicHoliday;
            for (String day : pls.get()) {
               try {
                   Calendar c = Calendar.getInstance();
                   c.setTime(SDF_DATE.parse(day.trim()));
                   c.set(Calendar.MILLISECOND, 0);
                   c.set(Calendar.SECOND, 0);
                   c.set(Calendar.MINUTE, 0);
                   c.set(Calendar.HOUR_OF_DAY, 0);
                   publicHolidays.add(SDF_DATE.format(c.getTime()));
                } catch (ParseException e) {
                   throw new IllegalArgumentException("Invalid Syntax for <" + day + "> expected 'yyyy-MM-dd'", e);
                }
            }
        });
        
        // Update exclusive openings
        getProperty(SPECIAL_OPENINGS).ifPresent(specialOpenings -> {
            PropertyListString pls = (PropertyListString) specialOpenings;
            for (String day : pls.get()) {
                String[] partDay = day.split("@");
                if (partDay.length != 2) {
                    throw new IllegalArgumentException("Invalid Syntax");
                }
                      
                // Check format at loading
                String dateExpression = partDay[1].trim();
                try {
                    SDF_DATE.parse(dateExpression);
                    String inter = partDay[0].trim();
                    String extractIntervals = inter.substring(1, inter.length() -1);
                    specialTimeTable.put(dateExpression,  parseIntervalsExpression(extractIntervals));
                 } catch (ParseException e) {
                     throw new IllegalArgumentException("Invalid Syntax for '" + dateExpression + "' expected 'yyyy-MM-dd'", e);
                 }
             }
        });
    }
    
    @Override
    public boolean test(ToggleContext ctx) {
        // Check current date agains interval
        Calendar now = Calendar.getInstance();
        if (ctx != null && ctx.containsKey(OVERRIDE_DATE)) {
            now = (Calendar) ctx.getValue(OVERRIDE_DATE, false);
        }
        
        // Priority 1 : Special Opening
        String currentDate = SDF_DATE.format(now.getTime());
        if (specialTimeTable.containsKey(currentDate)) {
            // Today is in special openings, apply
            return matches(now, specialTimeTable.get(currentDate));
        }
        
        // Priority 2 : Public Holiday => CLOSED
        if (publicHolidays.contains(currentDate)) {
            return false;
        }
        
        // Default behavior, get current day, retriesve intervals and check
        return matches(now, weekTimeTable.get(now.get(Calendar.DAY_OF_WEEK)));
    }
    
    /**
     * Parse Target expression.
     *
     * @param expression
     *      target expression
     * @return
     *      list of hour interval
     */
    public List < HourInterval > parseIntervalsExpression(String expression) {
        // Always close
        List < HourInterval > lhi = new ArrayList<HourInterval>();
        if (expression != null && !"".equals(expression)) {
           String[] chunks = expression.split(",");
           for (String chunk : chunks) {
               lhi.add(new HourInterval(chunk));
           }
        }
        return lhi;
    }
    
    /**
     * Check if present time is at least in of the hour Interval.
     *
     * @param listOfHI
     *      enable to list hour intervals 
     * @return
     *      if one of the interval matches
     */
    public boolean matches(Calendar cal, List < HourInterval > listOfHI) {
        if (listOfHI == null) return false;
        int idx = 0;
        boolean found = false;
        while (!found && idx<listOfHI.size()) {
            found = listOfHI.get(idx).matches(cal);
            idx++;
        }
        return found;
    }

}
