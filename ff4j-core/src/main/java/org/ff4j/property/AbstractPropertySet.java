package org.ff4j.property;

import java.util.HashSet;
import java.util.Set;

/**
 * SuperClass for property as lists.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 * @param <T>
 *      current type
 */
public class AbstractPropertySet < T > extends AbstractPropertyMultiValued< T, Set <T>>{

    /** Serial. */
    private static final long serialVersionUID = 4064427839404299895L;

    /** {@inheritDoc} */
    @Override
    public Set<T> fromString(String v) {
        return new HashSet<T>(super.fromString(v));
    }

}
