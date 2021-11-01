package org.ff4j.consul.store;

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
