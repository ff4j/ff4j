package org.ff4j;


import org.ff4j.feature.Feature;

import java.util.NoSuchElementException;
import java.util.function.Function;
import java.util.function.Supplier;

public interface Foldable {
    boolean isRight();

    boolean isLeft();

    Feature get();

    default <U> U fold(Supplier<? extends U> leftMapper, Function<Feature, ? extends U> rightMapper) {
        if (isRight()) {
            return rightMapper.apply(get());
        } else {
            return leftMapper.get();
        }
    }

    final class DisabledFeature implements Foldable {

        @Override
        public boolean isRight() {
            return false;
        }

        @Override
        public boolean isLeft() {
            return true;
        }

        @Override
        public Feature get() {
            throw new NoSuchElementException("Feature not enabled");
        }
    }

    final class EnabledFeature implements Foldable {

        private final Feature feature;

        public EnabledFeature(Feature feature) {
            this.feature = feature;
        }

        @Override
        public boolean isRight() {
            return true;
        }

        @Override
        public boolean isLeft() {
            return false;
        }


        @Override
        public Feature get() {
            return feature;
        }
    }
}
