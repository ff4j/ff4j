package org.ff4j.feature.togglestrategy.time;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.backend.BackendSupport;
import org.ff4j.feature.Feature;
import org.ff4j.feature.togglestrategy.AbstractToggleStrategy;
import org.ff4j.property.PropertyCalendar;
import org.ff4j.property.evaluate.FF4jEvaluationContext;
import org.ff4j.property.list.PropertyListString;

/**
 * Implementation of an office hour strategy.
 * 
 * Expression { "monday":["08:00-12:00", "13:30-18:00"], "tuesday":[], "wednesday":[], "thursday":[],"friday":[],"saturday":[] }
 */
public class OfficeHourToggleStrategy extends AbstractToggleStrategy {

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
    public static final String PUBLIC_HOLIDAY   = "publicHolidays";
    public static final String SPECIAL_OPENINGS = "specialOpenings";
    public static final String OVERRIDE_DATE    = "overrideDate";
    
    /** DAY_OF_WEEK -> List<HourSlot> with begin/end. */
    private final Map < Integer, List <HourInterval>> weekTimeTable = new HashMap<>();
    
    /** DATE -> List<HourSlot> with begin/end. */
    private final Map < String, List < HourInterval>> specialTimeTable = new HashMap<>();
    
    /** Public holiday (dates). */
    private final List < String > publicHolidays = new ArrayList<>();

    /**
     * Constructor with everything needed
     *
     * @param backend
     *      current backend
     * @param relatedFeature
     *      current feature
     * @param config
     *      configuration
     */
    public OfficeHourToggleStrategy(BackendSupport backend, Feature relatedFeature, FF4jEvaluationContext config) {
        super(backend, relatedFeature,config);
        parseWeek();
        parsePublicHoliday();
        parseSpecialOpening();
    }

    /**
     * Populate local map of public holiday
     */
    private void parseWeek() {
        // Update Days
        getConfig().findProperty(MONDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.MONDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(TUESDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.TUESDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(WEDNESDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.WEDNESDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(THURSDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.THURSDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(FRIDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.FRIDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(SATURDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.SATURDAY, parseIntervalsExpression(p.getValueAsString())));
        getConfig().findProperty(SUNDAY).ifPresent(
                p -> weekTimeTable.put(Calendar.SUNDAY, parseIntervalsExpression(p.getValueAsString())));
    }

    /**
     * Populate local map of public holiday
     */
    private void parsePublicHoliday() {
        getConfig().findProperty(PUBLIC_HOLIDAY).ifPresent(publicHoliday ->
                ((PropertyListString) publicHoliday).getValue().stream().forEach(day -> {
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
                }));
    }

    /**
     * Populate local map of special opening
     */
    private void parseSpecialOpening() {
        getConfig().findProperty(SPECIAL_OPENINGS).ifPresent(specialOpenings ->
            ((PropertyListString) specialOpenings).getValue().stream().forEach(day -> {
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
                    specialTimeTable.put(dateExpression, parseIntervalsExpression(extractIntervals));
                 } catch (ParseException e) {
                     throw new IllegalArgumentException("Invalid Syntax for '" + dateExpression + "' expected 'yyyy-MM-dd'", e);
                 }
        }));
    }
    
    @Override
    public boolean test(FF4jEvaluationContext ctx) {
        Calendar now = Calendar.getInstance();
        // Check current date against interval, else today is used
        if (ctx != null && ctx.findProperty(OVERRIDE_DATE).isPresent()) {
            now = ((PropertyCalendar) ctx.getProperty(OVERRIDE_DATE)).getValue();
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
        
        // Default behavior, get current day, retrieve intervals and check
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
        List < HourInterval > lhi = new ArrayList<>();
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
