package org.ff4j.jmx;

/*
 * #%L
 * ff4j-jmx
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

import javax.management.MalformedObjectNameException;

public class SampleJMX {
        
    public static void main(String[] args) throws MalformedObjectNameException {
        //MBeanServer mbs = ManagementFactory.getPlatformMBeanServer(); 
        //ObjectName name = new ObjectName("com.example:type=Hello"); 
        /*
         *  http://www.jmdoudoux.fr/java/dej/chap-jmx.htm
   
        mbs.registerMBean(mbean, name);
        
        
        lic class Premier extends NotificationBroadcasterSupport implements
        09.
        PremierMBean {
        10.
         
        11.
        private static String nom            = "PremierMBean";
        12.
         
        13.
        private int           valeur         = 100;
        14.
         
        15.
        private static long   numeroSequence = 0l;
        16.
         
        17.
        public String getNom() {
        18.
        return nom;
        19.
        }
        20.
         
        21.
        public int getValeur() {
        22.
        return valeur;
        23.
        }
        24.
         
        25.
        public synchronized void setValeur(int valeur) {
        26.
        numeroSequence++;
        27.
        Notification notif = new AttributeChangeNotification(this,
        28.
        numeroSequence, System.currentTimeMillis(),
        29.
        "Modification de la valeur", "Valeur", "int", this.valeur, valeur);
        30.
         
        31.
        this.valeur = valeur;
        32.
         
        33.
        sendNotification(notif);*/
        
        
        
    }
}
