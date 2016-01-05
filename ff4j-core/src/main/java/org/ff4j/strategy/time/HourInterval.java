package org.ff4j.strategy.time;

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


import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import org.ff4j.utils.Util;

/**
 * Date Interval.
*
* @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
       Util.assertHasLength(expression);
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
    * @param froms
    *      from date
    * @param tos
    *      to date
    */
   public HourInterval(Date froms, Date tos) {
       if (froms.before(tos)) {
           from.setTime(froms);
           to.setTime(tos);
       } else {
           from.setTime(tos);
           to.setTime(froms);
       }
   }
   
   /**
    * Initialize component.
    *
    * @param froms
    *      from calendar
    * @param tos
    *      to calendar
    */
   public HourInterval(Calendar froms, Calendar tos) {
       this(froms.getTime(), tos.getTime());
   }
   
   /**
    * Initialization by dates
    *
    * @param from
    *      lower bound
    * @param to
    *      uppoer bound
    */
   public void init(String f, String t) {
       try {
           from.setTime(SDF_HOUR.parse(f));
           to.setTime(SDF_HOUR.parse(t));
           // Exchang bound if required
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
    * @return
    */
   public boolean matches() {
       return matches(Calendar.getInstance());
   }
   
   /**
    * Check bounds against defined date.
    *
    * @return
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
