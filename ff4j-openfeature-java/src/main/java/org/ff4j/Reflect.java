package org.ff4j;

import org.ff4j.backend.Backend;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;

import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;

public class Reflect {

    public static void main(String[] args) {

        try {
            Map<String, String > dudle = new HashMap<>();
            FF4jProvider f = (FF4jProvider) Class.forName("org.ff4j.FF4jProvider")
                    .getConstructor(FF4jClient.class, Backend.class, Property.class, Map.class)
                    .newInstance(new FF4jClient(), Backend.builder().build(), new PropertyString("uid", "value"), dudle);

        } catch (NoSuchMethodException e) {
            e.printStackTrace();
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        } catch (InvocationTargetException e) {
            e.printStackTrace();
        } catch (InstantiationException e) {
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }

    }
}
