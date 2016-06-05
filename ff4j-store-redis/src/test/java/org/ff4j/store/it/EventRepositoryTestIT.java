package org.ff4j.store.it;

/*
 * #%L
 * ff4j-store-redis
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


/**
 * Utilisation des commandes : 
 * https://github.com/xetorthio/jedis/blob/master/src/test/java/redis/clients/jedis/tests/commands/SortedSetCommandsTest.java
 * 
 * References des commandes
 * http://redis.io/commands#sorted_set
 * 
 * Exemples :
 * 
 * { "id": "d74d9943-348c-4c9d-bf2d-7c0dc0735c41", 
 *  "timestamp":1458418610240, 
 *  "hostName": "MCCEL02", 
 *  "source": "JAVA_API", 
 *  "name": "F1", "type": "feature", 
 *  "action": "checkOn", 
 *  "duration":0}
 *  
 * { "id": "235f5181-435f-469f-9232-d43af0c07f6b", 
 *   "timestamp":1458418710240, "hostName": "MCCEL02", 
 *   "source": "JAVA_API", "user": "Admin", "name": "P1", 
 *   "type": "property", "action": "update", 
 *   "value": "NewValue", "duration":1}
 *   
 * @author CEL
 */
public class EventRepositoryTestIT {

}
