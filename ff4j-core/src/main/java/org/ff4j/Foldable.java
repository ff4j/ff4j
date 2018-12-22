package org.ff4j;

import org.ff4j.feature.Feature;

import java.util.function.Function;

public interface Foldable<L, R> {
    boolean isRight();

    boolean isLeft();

    L getLeft();

    R get();

    default <U> U fold(Function<? super L, ? extends U> leftMapper, Function<? super R, ? extends U> rightMapper) {
        if (isRight()) {
            return rightMapper.apply(get());
        } else {
            return leftMapper.apply(getLeft());
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
        public Object getLeft() {
            return null;
        }

        @Override
        public Object get() {
            return null;
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
        public Object getLeft() {
            return null;
        }

        @Override
        public Object get() {
            return null;
        }
    }
}
