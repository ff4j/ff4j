package org.ff4j.feature.togglestrategy.time;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.ff4j.utils.Assert;

/**
 * Date Interval.
*/
public final class HourInterval {
    
   /** Parsing expression. */
   private static final DateFormat SDF_HOUR = new SimpleDateFormat("HH:mm");
   
   /** Lower bound of interval. */
   private Calendar from = Calendar.getInstance();
   
   /** Upper bound of interval. */
   private Calendar to = Calendar.getInstance();
   
   /**
    * Default.
    */
   public HourInterval() {
   }
   
   /**
    * Initialization through init param structure : HH:MM-HH:MM
    * 
    * @param expression
    *      expression to be parsed
    */
   public HourInterval(String expression) {
       Assert.assertHasLength(expression);
       String[] bounds = expression.split("-");
       if (bounds.length != 2) {
           throw new IllegalArgumentException("Invalid syntax, expected HH:mm-HH:MM " + expression);
       }
       init(bounds[0], bounds[1]);
   }
   
   /**
    * Constructor by string expressions
    * @param f
    *      from date as HH:MM
    * @param t
    *      to date as HH:MM
    */
   public HourInterval(String f, String t) {
      init(f, t);
   }
   
   /**
    * Initialize component.
    *
    * @param from
    *      from date
    * @param tos
    *      to date
    */
   public HourInterval(Date from, Date tos) {
       if (from.before(tos)) {
           this.from.setTime(from);
           to.setTime(tos);
       } else {
           this.from.setTime(tos);
           to.setTime(from);
       }
   }
   
   /**
    * Initialize component.
    *
    * @param from
    *      from calendar
    * @param tos
    *      to calendar
    */
   public HourInterval(Calendar from, Calendar tos) {
       this(from.getTime(), tos.getTime());
   }
   
   /**
    * Initialization by dates
    *
    * @param f
    *      lower bound
    * @param t
    *      upper bound
    */
   public void init(String f, String t) {
       try {
           from.setTime(SDF_HOUR.parse(f));
           to.setTime(SDF_HOUR.parse(t));
           // Exchange bound if required
           if (!from.before(to)) {
               Calendar cal = to;
               to = from;
               from = cal;
           }
       } catch (ParseException e) {
           throw new IllegalArgumentException("Cannot parse incoming expressions <" + f + ">, <" + t + ">", e);
       }
   }
   
   /**
    * Check bounds against current date
    *
    * @return
    *       if expression matches
    */
   public boolean matches() {
       return matches(Calendar.getInstance());
   }
   
   /**
    * Check bounds against defined date.
    *
    * @return
    *       if expression matches
    */
   public boolean matches(Calendar cal) {
       // Align date, compare only hours here
       from.set(Calendar.YEAR, cal.get(Calendar.YEAR));
       from.set(Calendar.DAY_OF_YEAR, cal.get(Calendar.DAY_OF_YEAR));
       to.set(Calendar.YEAR, cal.get(Calendar.YEAR));
       to.set(Calendar.DAY_OF_YEAR, cal.get(Calendar.DAY_OF_YEAR));
       return from.before(cal) && to.after(cal);
   }

   /**
    * Getter accessor for attribute 'from'.
    *
    * @return
    *       current value of 'from'
    */
   public Calendar getFrom() {
       return from;
   }

   /**
    * Setter accessor for attribute 'from'.
    * @param from
    *      new value for 'from '
    */
   public void setFrom(Calendar from) {
       this.from = from;
   }

   /**
    * Getter accessor for attribute 'to'.
    *
    * @return
    *       current value of 'to'
    */
   public Calendar getTo() {
       return to;
   }

   /**
    * Setter accessor for attribute 'to'.
    * @param to
    *      new value for 'to '
    */
   public void setTo(Calendar to) {
       this.to = to;
   }
   
}
