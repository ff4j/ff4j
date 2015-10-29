package org.ff4j.property;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.List;

/**
 * Create {@link AbstractProperty} from name type and value.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class PropertyFactory {

    /**
     * Hide constructor as util class.
     */
    private PropertyFactory() {}

    /**
     * Factory method to create property.
     *
     * @param pName
     *            property name.
     * @param pType
     *            property type
     * @param pValue
     *            property value
     * @return
     */
    public static AbstractProperty<?> createProperty(String pName, String pType, String pValue) {
        return PropertyFactory.createProperty(pName, pType, pValue, null, null);
    }

    /**
     * Factory method to create property.
     *
     * @param pName
     *            property name.
     * @param pType
     *            property type
     * @param pValue
     *            property value
     * @return
     */
    public static AbstractProperty<?> createProperty(String pName, String pType, String pValue, String desc, String fixedValues) {
        if (pName == null)
            throw new IllegalArgumentException("pName (param#0) is expected to create property");
        if (pType == null)
            throw new IllegalArgumentException("Type (param#1) is expected to create property");

        AbstractProperty<?> ap = new Property(pName, pValue);
        try {
            // Construction by dedicated constructor with introspection
            Constructor<?> constr = Class.forName(pType).getConstructor(String.class, String.class);
            ap = (AbstractProperty<?>) constr.newInstance(pName, pValue);
        } catch (InstantiationException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' check default constructor : " + e.getMessage(), e);
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' check constructor visibility : " + e.getMessage(), e);
        } catch (ClassNotFoundException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' class not found : " + e.getMessage(), e);
        } catch (InvocationTargetException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' error within constructor (InvocationTargetException)", e);
        } catch (NoSuchMethodException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' constructor not found :" + e.getMessage(), e);
        } catch (SecurityException e) {
            throw new IllegalArgumentException("Cannot instantiate '" + pType + "' check constructor visibility :" + e.getMessage(), e);
        }

        // Description
        if (desc != null && !"".equals(desc)) {
            ap.setDescription(desc);
        }

        // Is there any fixed Value ?
        if (fixedValues != null && !"".equals(fixedValues)) {
            List<String> listOfFixedValue = Arrays.asList(fixedValues.split(","));
            if (listOfFixedValue != null) {
                for (String v : listOfFixedValue) {
                    ap.add2FixedValueFromString(v.trim());
                }
                // Check fixed value
                if (ap.getFixedValues() != null && !ap.getFixedValues().contains(ap.getValue())) {
                    throw new IllegalArgumentException("Cannot create property <" + ap.getName() + "> invalid value <"
                            + ap.getValue() + "> expected one of " + ap.getFixedValues());
                }
            }
        }
        return ap;
    }
}
