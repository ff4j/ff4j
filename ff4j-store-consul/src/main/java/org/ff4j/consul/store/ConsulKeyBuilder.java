package org.ff4j.consul.store;

/*-
 * #%L
 * ff4j-store-consul
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.audit.Event;

import java.time.format.DateTimeFormatter;

import static java.time.Instant.ofEpochSecond;

public class ConsulKeyBuilder {

  /** audit key. */
  private static final DateTimeFormatter KDF = DateTimeFormatter.ofPattern("yyyyMMdd");

  private final String prefix;

  /**
   * Generates a {@link ConsulKeyBuilder} with the default key format.
   */
  public ConsulKeyBuilder() {
    this("FF4J");
  }

  /**
   * Creates a key builder with a custom prefix.
   *
   * @param prefix custom prefix
   */
  public ConsulKeyBuilder(String prefix) {
    this.prefix = prefix;
  }

  public String getKeyName(String featureName) {
    return prefix + "/FEATURES/" + featureName;
  }

  public String getFeatureName(String consulKey) {
    return consulKey.replace( prefix + "/FEATURES/", "");
  }

  public String getFeaturesDictionaryKey() {
    return prefix + "/FEATURES_DICTIONARY";
  }

  public String getPropertyKey(String propertyName) {
    return prefix + "/PROPERTIES/" + propertyName;
  }

  public String getPropertyName(String consulKey) {
    return consulKey.replace(prefix + "/PROPERTIES/", "");
  }

  public String getPropertiesDictionaryKey() {
    return prefix + "/PROPERTIES_DICTIONARY";
  }

  public String getHitCountKey(Event e) {
    return prefix + "/HITS/" +
      KDF.format(ofEpochSecond(e.getTimestamp())) + "/"   +
      e.getName() + "/" + e.getUuid();
  }

  public String getMissKey(Event e) {
    return prefix + "/MISS/" +
      KDF.format(ofEpochSecond(e.getTimestamp())) + "/"   +
      e.getName() + "/" + e.getUuid();
  }

  public String getAuditTrailKey(Event e) {
    return prefix + "/AUDIT/" +
      KDF.format(ofEpochSecond(e.getTimestamp())) + "/"   +
      e.getName() + "/" + e.getUuid();
  }
}
