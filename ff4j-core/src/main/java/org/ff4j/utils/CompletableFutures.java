package org.ff4j.utils;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * Syntax sugar to work with {@link CompletableFuture}.
 */
public class CompletableFutures {

    /**
     * Hidden constructor.
     */
    private CompletableFutures() {}
    
    /**
     * Execute operation in synchronous manner.
     *
     * @param <T>
     *      any object
     * @param stage
     *      execute in synchronous
     * @return
     *      expected output
     */
    public static <T> T sync(CompletionStage<T> stage) {
        boolean interrupted = false;
        try {
            while (true) {
                try {
                    return stage.toCompletableFuture().get();
                } catch (InterruptedException e) {
                    interrupted = true;
                } catch (ExecutionException e) {
                    Throwable cause = e.getCause();
                    throw new RuntimeException(cause);
                }
            }
        } finally {
            if (interrupted) {
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Return a completion stage that completes when all inputs are successful, or fails if any of them failed.
     * @param inputs
     *      list of inputs
     * @param <T>
     *       parameter
     * @return
     *      grouped completion stage
     */
    public static <T> CompletionStage<Void> allSuccessful(List<CompletionStage<T>> inputs) {
      CompletableFuture<Void> result = new CompletableFuture<>();
      if (inputs.isEmpty()) {
        result.complete(null);
      } else {
        final int todo = inputs.size();
        final AtomicInteger done = new AtomicInteger();
        final CopyOnWriteArrayList<Throwable> errors = new CopyOnWriteArrayList<>();
        for (CompletionStage<?> input : inputs) {
          input.whenComplete(
              (v, error) -> {
                if (error != null) {
                  errors.add(error);
                }
                if (done.incrementAndGet() == todo) {
                  if (errors.isEmpty()) {
                    result.complete(null);
                  } else {
                    Throwable finalError = errors.get(0);
                    for (int i = 1; i < errors.size(); i++) {
                      finalError.addSuppressed(errors.get(i));
                    }
                    result.completeExceptionally(finalError);
                  }
                }
              });
        }
      }
      return result;
    }

    /**
     * Return a single {@link CompletionStage} when all list is completed
     * @param inputs
     *      list of input completable futures
     * @param <T>
     *       current parameters
     * @return
     *      single completion future
     */
    public static <T> CompletionStage<Void> allDone(List<CompletionStage<T>> inputs) {
        CompletableFuture<Void> result = new CompletableFuture<>();
        if (inputs.isEmpty()) {
          result.complete(null);
        } else {
          final int todo = inputs.size();
          final AtomicInteger done = new AtomicInteger();
          for (CompletionStage<?> input : inputs) {
            input.whenComplete(
                (v, error) -> {
                  if (done.incrementAndGet() == todo) {
                    result.complete(null);
                  }
                });
          }
        }
        return result;
    }
    
    /**
     * Transforms a <pre>{@code List<CompletableFuture<T>>}</pre> into a <pre>{@code CompletableFuture<List<T>>}</pre>
     * @param <X> the computed result type
     * @param <T> some CompletableFuture
     * @return a CompletableFuture of <pre>{@code CompletableFuture<List<T>>}</pre> that is complete when all collected CompletableFutures are complete.
     */
    public static <X, T extends CompletableFuture<X>> Collector<T, ?, CompletableFuture<List<X>>> collectResult(){
        return Collectors.collectingAndThen(Collectors.toList(), joinResult());
    }

    /**
     * Private Method to Join results.
     *
     * @param <X>
     *     param for completable future
     * @param <T>
     *     paran for list
     * @return
     *      function
     */
    private static <X, T extends CompletableFuture<X>> Function<List<T>, CompletableFuture<List<X>>> joinResult() {
        return ls-> allOf(ls)
                .thenApply(v -> ls
                        .stream()
                        .map(CompletableFuture::join)
                        .collect(Collectors.toList()));
    }

    /**
     * Private method for all of.
     *
     * @param ls
     *      list of futures
     * @param <T>
     *       param
     * @return
     *      single future
     */
    private static <T extends CompletableFuture<?>> CompletableFuture<Void> allOf(List<T> ls) {
        return CompletableFuture.allOf(ls.toArray(new CompletableFuture[0]));
    }
    
    
      

}
