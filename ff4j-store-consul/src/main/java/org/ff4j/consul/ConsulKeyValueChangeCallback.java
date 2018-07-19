package org.ff4j.consul;

import com.google.common.base.Optional;
import com.google.common.base.Preconditions;
import com.orbitz.consul.Consul;
import com.orbitz.consul.KeyValueClient;
import com.orbitz.consul.async.ConsulResponseCallback;
import com.orbitz.consul.model.ConsulResponse;
import com.orbitz.consul.model.kv.Value;
import com.orbitz.consul.option.QueryOptions;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;



//Wraps the in memory cache manager so that cache can be updated from watches in Consul.
//Based this off of John & Dan's callback work done in HITL.

//This class will be contributed back to FF4J to allow community to be able to
//take advantage of updating the cache from consul's watch/callback capabilities.

/**
 * The ConsulKeyValueChangeCallback class is responsible for listening for and applying
 * changes to the cache if consul is updated outside of FF4j.
 */
public final class ConsulKeyValueChangeCallback implements ConsulResponseCallback<Optional<Value>> {

  private static final Logger LOGGER = LoggerFactory.getLogger(ConsulKeyValueChangeCallback.class);
  private KeyValueClient keyValueClient;
  private AtomicReference<BigInteger> index;
  private Consumer<String> onChangeCallback;
  private Integer waitSeconds;
  private String keyPath;

  /**
   * Creates an instance of the ConsulKeyValueChangeCallback which listens for changes
   * in consul and updates the ConsulCacheManager with the new value.
   * @param consul - instance of consul.
   * @param keyPath - the key path being monitored.
   * @param onChangeCallback - the method to call to do the update when a change occurs.
   * @param waitSeconds - the time period to wait for the check.
   */
  public ConsulKeyValueChangeCallback(Consul consul, String keyPath, Consumer<String>
          onChangeCallback, int waitSeconds) {
    Preconditions.checkNotNull(onChangeCallback);
    Preconditions.checkNotNull(keyPath);
    this.onChangeCallback = onChangeCallback;
    this.waitSeconds = waitSeconds;
    this.keyPath = keyPath;
    this.keyValueClient = consul.keyValueClient();
    index = new AtomicReference<>(BigInteger.valueOf(0));

    keyValueClient.getValue(keyPath,
            QueryOptions.blockSeconds(waitSeconds, BigInteger.valueOf(0)).build(),
            this);
  }

  @Override
  public void onComplete(ConsulResponse<Optional<Value>> consulResponse) {

    if (consulResponse.getResponse().isPresent()) {
      Value v = consulResponse.getResponse().get();
      onChangeCallback.accept(v.getValueAsString().orNull());
      LOGGER.info("Updating cache for {} with {}.", v.getKey(), v.getValueAsString());
    }
    index.set(consulResponse.getIndex());
    watch();

  }

  @Override
  public void onFailure(Throwable throwable) {
    LOGGER.error("Failed to update the cache with changed value.", throwable);
    watch();
  }

  private void watch() {
    LOGGER.info("Watching for {} for further changes.", keyPath);
    keyValueClient.getValue(keyPath,
            QueryOptions.blockMinutes(waitSeconds, index.get()).build(), this);

  }
}