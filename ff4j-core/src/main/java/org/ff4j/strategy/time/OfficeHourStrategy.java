package org.ff4j.strategy.time;

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

import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.strategy.AbstractFlipStrategy;

/**
 * Implemenetation of an office hour strategy.
 * 
 * Expression { "monday":["08:00-12:00", "13:30-18:00"], "tuesday":[], "wednesday":[], "thursday":[],"friday":[],"saturday":[] }
 *
 * @author Cedrick Lunven (@clunven)
 */
public class OfficeHourStrategy extends AbstractFlipStrategy {
    
    /** Parsing date expression. */
    private static final DateFormat SDF_DATE = new SimpleDateFormat("yyyy-MM-dd");
    
    /** Constants. */
    private static final String MONDAY = "monday";
    
    /** Constants. */
    private static final String TUESDAY = "tuesday";
    
    /** Constants. */
    private static final String WEDNESDAY = "wednesday";
    
    /** Constants. */
    private static final String THURSDAY = "thursday";
    
    /** Constants. */
    private static final String FRIDAY = "friday";
    
    /** Constants. */
    private static final String SATURDAY = "saturday";
    
    /** Constants. */
    private static final String SUNDAY = "sunday";
    
    /** Constants. */
    private static final String PUBLICHOLIDAY = "publicHolidays";
    
    /** Constants. */
    private static final String SPECIAL_OPENINGS = "specialOpenings";
    
    /** Contacts. */
    public static final String OVERRIDE_DATE = "overridedDate";
    
    /** time table. */
    private Map < Integer, List <HourInterval>> weekTimeTable = new HashMap<Integer, List<HourInterval>>();
    
    /** openings. */
    private Map < String, List < HourInterval>> specialTimeTable = new HashMap< String, List<HourInterval>>();
    
    /** public holiday. */
    private List < String > publicHolidays = new ArrayList<String>();
    
    /** {@inheritDoc} */
    @Override
    public void init(String featureName, Map<String, String> initParam) {
        super.init(featureName, initParam);
        
        // Update week timetable
        weekTimeTable.put(Calendar.MONDAY,      parseIntervalsExpression(initParam.get(MONDAY)));
        weekTimeTable.put(Calendar.TUESDAY,     parseIntervalsExpression(initParam.get(TUESDAY)));
        weekTimeTable.put(Calendar.WEDNESDAY,   parseIntervalsExpression(initParam.get(WEDNESDAY)));
        weekTimeTable.put(Calendar.THURSDAY,    parseIntervalsExpression(initParam.get(THURSDAY)));
        weekTimeTable.put(Calendar.FRIDAY,      parseIntervalsExpression(initParam.get(FRIDAY)));
        weekTimeTable.put(Calendar.SATURDAY,    parseIntervalsExpression(initParam.get(SATURDAY)));
        weekTimeTable.put(Calendar.SUNDAY,      parseIntervalsExpression(initParam.get(SUNDAY)));
        
        // Update publiholidays
        if (initParam.containsKey(PUBLICHOLIDAY)) {
            String[] days = initParam.get(PUBLICHOLIDAY).split(",");
            for (String day : days) {
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
        }
        
        // Update exclusive openings
        if (initParam.containsKey(SPECIAL_OPENINGS)) {
            String[] days = initParam.get(SPECIAL_OPENINGS).split(";");
            for (String day : days) {
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
        }
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
    
    /** {@inheritDoc} */
    @Override
    public boolean evaluate(String featureName, FeatureStore store, FlippingExecutionContext executionContext) {
        // Check current date agains interval
        Calendar now = Calendar.getInstance();
        if (executionContext != null && executionContext.containsKey(OVERRIDE_DATE)) {
            now = (Calendar) executionContext.getValue(OVERRIDE_DATE, false);
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
        
        // Default behavior, get current day, retrive intervals and check
        return matches(now, weekTimeTable.get(now.get(Calendar.DAY_OF_WEEK)));
    }

}
