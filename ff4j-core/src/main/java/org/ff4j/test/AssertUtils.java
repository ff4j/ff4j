package org.ff4j.test;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import java.util.Collection;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.function.Supplier;

import org.ff4j.exception.AssertionViolationException;

/**
 * Assertion utility classes used to evaluate expression everywhere. For parameters but  also in the code.
 * - If condition is not true an {@link AssertionViolationException} is raised.
 * - If used to validate method parameters (paramAssert*) will raise an {@link IllegalArgumentException} instead.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class AssertUtils {
   
    /** default timeout (in millis). */
    private static long DEFAULT_TIMEOUT = 2000;
    
    /** default polling interval (in millis). */
    private static long DEFAULT_POLLINGINTERVAL = 100;
    
    /** timeout. */
    private static long timeout = DEFAULT_TIMEOUT;
    
    /** Polling interval. */
    private static long pollingInterval = DEFAULT_POLLINGINTERVAL;
    
    /**
     * Hide public constructor (Utility class)
     */
    private AssertUtils() {}
    
    // -------------------------------------
    // ---------- ASSERT TRUE --------------
    // -------------------------------------
     
    /** default error message. */
    private static String DEFAULT_MESSAGE_TRUE = "Expected to be TRUE but was not";
    
    /**
     * Default, synchronous assertion, well it's a dummy test.
     *
     * @param result
     *      condition to evaluate
     * @throws AssertionViolationException
     *      specific runtime exception to detail invalid condition
     **/
    public static final void assertTrue(boolean result) {
        assertSync(true, result);
    }
    public static final void assertTrue(boolean result, String errorMessage) {
        assertSync(true, result, errorMessage);
    }
    
    /**
     * Synchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code> 
     * @return
     *          number of iteration until result correct
     *
     * @throws AssertionViolationException
     *          specific runtime exception to detail invalid condition
     */
    public static final <T> void assertTrue(Predicate<T> assertion, T param) {
        assertTrueAsync(assertion, param, -1, 0);
    }
    public static final <T> void assertTrue(Predicate<T> assertion, T param, String errorMessage) {
        assertTrueAsync(assertion, param, errorMessage, -1, 0);
    }
    
    /**
     * ASynchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code> 
     * @return
     *          number of iteration until result correct
     *
     * @throws AssertionViolationException
     *          specific runtime exception to detail invalid condition
     */
    public static final <T> int assertTrueAsync(Predicate<T> assertion, T param) {
        return assertTrueAsync(assertion, param, DEFAULT_MESSAGE_TRUE, timeout, pollingInterval);
    }
    public static final <T> int assertTrueAsync(Predicate<T> assertion, T param, String errorMessage) {
        return assertTrueAsync(assertion, param, errorMessage, timeout, pollingInterval);   
    }
    public static final <T> int assertTrueAsync(Predicate<T> assertion, T param, String errorMessage, long ptimeout, long pPollingInterval) {
        return assertAsync(true, assertion, param, ptimeout, pPollingInterval, errorMessage);
    }
    public static final <T> int assertTrueAsync(Predicate<T> assertion, T param, long ptimeout, long pPollingInterval) {
        return assertAsync(true, assertion, param, ptimeout, pPollingInterval, DEFAULT_MESSAGE_TRUE);
    }
    
   // -------------------------------------
   // ---------- ASSERT FALSE --------------
   // -------------------------------------
    
   /** default error message. */
   private static String DEFAULT_MESSAGE_FALSE = "Expected to be FALSE but was not";
   
   /**
    * Default, synchronous assertion, well it's a dummy test.
    *
    * @param result
    *      condition to evaluate
    * @throws AssertionViolationException
    *      specific runtime exception to detail invalid condition
    **/
   public static final void assertFalse(boolean result) {
       assertSync(false, result);
   }
   public static final void assertFalse(boolean result, String errorMessage) {
       assertSync(false, result, errorMessage);
   }
   
   /**
    * Synchronous assertion using a predicate.
    *
    * @param assertion
    *          predicate to evaluate
    * @param param
    *          expression as input of predicate <code>assertion.test(param)</code> 
    * @return
    *          number of iteration until result correct
    *
    * @throws AssertionViolationException
    *          specific runtime exception to detail invalid condition
    */
   public static final <T> void assertFalse(Predicate<T> assertion, T param) {
       assertFalseAsync(assertion, param, -1, 0);
   }
   
   public static final <T> void assertFalse(Predicate<T> assertion, T param, String errorMessage) {
       assertFalseAsync(assertion, param, errorMessage, -1, 0);
   }
   
   /**
    * ASynchronous assertion using a predicate.
    *
    * @param assertion
    *          predicate to evaluate
    * @param param
    *          expression as input of predicate <code>assertion.test(param)</code> 
    * @return
    *          number of iteration until result correct
    *
    * @throws AssertionViolationException
    *          specific runtime exception to detail invalid condition
    */
   public static final <T> int assertFalseAsync(Predicate<T> assertion, T param) {
       return assertFalseAsync(assertion, param, DEFAULT_MESSAGE_FALSE, timeout, pollingInterval);
   }
   public static final <T> int assertFalseAsync(Predicate<T> assertion, T param, String errorMessage) {
       return assertFalseAsync(assertion, param, errorMessage, timeout, pollingInterval);   
   }
   public static final <T> int assertFalseAsync(Predicate<T> assertion, T param, String errorMessage, long ptimeout, long pPollingInterval) {
       return assertAsync(false, assertion, param, ptimeout, pPollingInterval, errorMessage);
   }
   public static final <T> int assertFalseAsync(Predicate<T> assertion, T param, long ptimeout, long pPollingInterval) {
       return assertAsync(false, assertion, param, ptimeout, pPollingInterval, DEFAULT_MESSAGE_FALSE);
   }
   
   // -------------------------------------
   // ---------- ASSERT EQUALS ------------
   // -------------------------------------
   
   /** default error message. */
   private static String DEFAULT_MESSAGE_EQUALS = "<%s> and <%s> Expected to be EQUALS but was not";
  
   /**
    * Predicate to be evaluated for testing equals.
    *
    * @param <O>
    *      any object implementing 'equals' (all objects
    */
   private static class EqualsCheck<O> implements BiPredicate<O, O> {
       /** {@inheritDoc} */
       @Override
       public boolean test(O expected, O actual) {
           return (expected == null) ? (actual == null) : expected.equals(actual);
       }
   }
   
    /**
     * To evaluate equality a predicate is defined, then the assertTrue component will be
     * reused. This present test is performed synchronously.
     * 
     * @param expected
     *          expected value
     * @param actual    
     *          current value to test against
     */
    public static final <T> void assertEquals(T expected, T actual) {
        assertEquals(expected, actual, String.format(DEFAULT_MESSAGE_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static final <T> void assertEquals(T expected, T actual, String errorMessage) {
        assertTrue(x -> new EqualsCheck().test(expected, actual), errorMessage);
    }
    
    /**
     * To evaluate equality a predicate is defined, then the assertTrue component will be
     * reused. This present test is performed Asynchronously.
     * 
     * @param expected
     *          expected value
     * @param actual    
     *          current value to test against
     */
    public static final <T> int assertEqualsASync(T expected, Supplier<T> actual) {
        return assertEqualsASync(expected, actual, String.format(DEFAULT_MESSAGE_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static final <T> int assertEqualsASync(T expected, Supplier<T> actual, String errorMessage) {
        return assertTrueAsync(x -> new EqualsCheck().test(expected, actual.get()), errorMessage);
    }
    
    // ----------------------------------
    // ---------- ASSERT NOT EQUALS -----
    // ----------------------------------
      
    /** default error message. */
    private static String DEFAULT_MESSAGE_NOT_EQUALS = "<%s> and <%s> Expected to be DIFFERENT but was EQUALS";
    
    /**
     * To evaluate equality a predicate is defined, then the assertTrue component will be
     * reused. This present test is performed synchronously.
     * 
     * @param expected
     *          expected value
     * @param actual    
     *          current value to test against
     */
    public static final <T> void assertNotEquals(T expected, T actual) {
        assertNotEquals(expected, actual, String.format(DEFAULT_MESSAGE_NOT_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static final <T> void assertNotEquals(T expected, T actual, String errorMessage) {
        assertFalse(x -> new EqualsCheck().test(expected, actual), errorMessage);
    }
   
    /**
     * To evaluate equality a predicate is defined, then the assertTrue component will be
     * reused. This present test is performed Asynchronously.
     * 
     * @param expected
     *          expected value
     * @param actual    
     *          current value to test against
     */
    public static final <T> int assertNotEqualsASync(T expected, Supplier<T> actual) {
        return assertNotEqualsASync(expected, actual, String.format(DEFAULT_MESSAGE_NOT_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static final <T> int assertNotEqualsASync(T expected, Supplier<T> actual, String errorMessage) {
        return assertFalseAsync(x -> new EqualsCheck().test(expected, actual.get()), errorMessage);
    }
    
    // -------------------------------------
    // ---------- ASSERT NULL   ------------
    // -------------------------------------
    
    /** default error message. */
    private static String DEFAULT_MESSAGE_NULL = "<%s> is expected to be null but was not";
   
    /**
     * Default, synchronous assertion test nullity of parameter
     *
     * @param actual
     *      object to evaluate
     * @throws AssertionViolationException
     *      specific runtime exception to detail invalid condition
     **/
    public static final <T> void assertNull(T actual) {
        assertTrue((actual==null), DEFAULT_MESSAGE_NULL);
    }
    
    public static final <T> void assertNull(T actual, String errorMessage) {
        assertTrue((actual==null), errorMessage);
    }
    
    public static final <T> int assertNullAsync(Supplier<T> actual) {
        return assertEqualsASync(null, actual);
    }
    
    public static final <T> int assertNullAsync(Supplier<T> actual, String errorMessage) {
        return assertEqualsASync(null, actual, errorMessage);
    }
    
    // -------------------------------------
    // ------- ASSERT NOT NULL   -----------
    // -------------------------------------
    
    /** default error message. */
    private static String DEFAULT_MESSAGE_NOTNULL = "<%s> is expected to be not null";
    
    /**
     * Synchronous assertion test nullity of parameter
     *
     * @param actual
     *      object to evaluate
     * @throws AssertionViolationException
     *      specific runtime exception to detail invalid condition
     **/
    public static final <T> void assertNotNull(T actual) {
        assertNotNull(actual, DEFAULT_MESSAGE_NOTNULL);
    }
    
    public static final <T> void assertNotNull(T actual, String errorMessage) {
        assertFalse(actual==null, errorMessage);
    }
    
    public static final <T> int assertNotNullAsync(Supplier<T> actual) {
        return assertNotEqualsASync(null, actual);
    }

    // -------------------------------------
    // ------- ASSERT HAS LENGTH -----------
    // -------------------------------------
    
    /**
     * Predicate to evaluate if  string content is not empty
     *
     * @author Cedrick LUNVEN (@clunven)
     */
    private static final class StringHasLenghPredicate implements Predicate<String> {
        /** {@inheritDoc} */
        @Override
        public boolean test(final String str) {
            return (str!=null) && !str.isEmpty();
        }
    }
    
    /** Predicate to evaluate if  string content is not empty. */
    private static final StringHasLenghPredicate HAS_LENGTH = new StringHasLenghPredicate();
    
    /** default error message. */
    private static String MSG_HAS_LENGTH = "'%s' should not be null nor empty.";
    
    /**
     * ASSERT hasLength
     */
    public static void assertHasLength(String str) {
        assertTrue(HAS_LENGTH.test(str));
    }
    
    public static void assertHasLength(String str, String objectName) {
        assertTrue(HAS_LENGTH.test(str), String.format(MSG_HAS_LENGTH, objectName));
    }
    
    public static final int assertHasLengthAsync(Supplier<String> actual) {
        return assertTrueAsync(x -> HAS_LENGTH.test(actual.get()), String.format(MSG_HAS_LENGTH, actual.get()));
    }
    
    public static final int assertHasLengthAsync(Supplier<String> actual, String objectName) {
        return assertTrueAsync(x -> HAS_LENGTH.test(actual.get()), String.format(MSG_HAS_LENGTH, objectName));
    }
    
    // -------------------------------------
    // ------- ASSERT INSTANCE_OF ----------
    // -------------------------------------
    
    /** default error message. */
    private static String MSG_INSTANCE_OF = "Invalid class expected %s but was %s";
    
    /**
     * Evaluate if the target object is expected.
     *
     * @param obj
     *          current objet
     * @param expectedClass
     *          target class to test against
     */
    public static void assertInstanceOf(Object obj, Class<?> expectedClass) {
        assertInstanceOf(obj, expectedClass, 
                String.format(MSG_INSTANCE_OF, expectedClass, obj.getClass()));
    }
    
    public static void assertInstanceOf(Object obj, Class<?> expectedClass, String errorMessage) {
        if (obj != null) {
            assertTrue(obj.getClass().isInstance(expectedClass), errorMessage);
        }
    }
    
    // -------------------------------------
    // ------- ASSERT COLLECTION  ----------
    // -------------------------------------
    
    /**
     * Check that object is not null.
     * 
     * @param object
     *            target object
     */
    public static <T> void assertNotEmpty(Collection<T> myCollec) {
        assertNotNull(myCollec);
        assertFalse(myCollec.isEmpty());
    }
    
    /**
     * Check that string is not null
     *
     * @param object
     *            target object
     */
    public static <T> void assertNotEmpty(T[] collec) {
        assertNotNull(collec);
        assertTrue(collec.length > 0);
    }    
    
    /**
     * Test assertion several times.
     *
     * @param timeout
     *      if the assertion if still not true at end , fail.
     * @param pollingInterval
     *      time between 2 tries of the assertion
     * @param assertion
     *      value to test
     * @param param
     *      parameter
     * @param errorMessage
     *      target error message
     */
    private static final <T> int assertAsync(boolean expected, Predicate<T> assertion, T param, long ptimeout, long pPollingInterval, String errorMessage) {
        long start = System.currentTimeMillis();
        boolean result = assertion.test(param);
        try {
            int assessCount = 1;
            // As long as the expected condition is not validated (!expected)
            while ((result != expected) && (System.currentTimeMillis() - start) <= ptimeout) {
                result = assertion.test(param);
                Thread.sleep(pPollingInterval);
                assessCount++;
            }
            assertSync(expected, result, 
                    String.format("Expected <%s> but was <%s> in %s", expected, result, assertion.toString()));
            return assessCount;
        } catch (InterruptedException e) { 
            return -1;
        }
    }
    
    /**
     * Expect target condition to be true.
     *
     * @param expected
     *      expected value
     * @param result
     *      execution of condition
     */
    private static final void assertSync(boolean expected, boolean result) {
        assertSync(expected, result, String.format("Expected <%s> but was <%s>", expected, result));
    }
    
    /**
     * Expect target condition to be true.
     *
     * @param expected
     *      expected value
     * @param result
     *      execution of condition
     * @param errorMessage
     *      custom error message
     */
    private static final void assertSync(boolean expected, boolean result, String errorMessage) {
        if (result != expected) {
            throw new AssertionViolationException(errorMessage);
        }
    }
    
    // -------------------------------------------------------------------------
    // ------------------- ARGUMENTS VALIDATION  -------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Check condition as TRUE
     */
    public static final void assertTrueParam(String paramName, int idx, boolean condition) {
        assertTrueParam(paramName, idx, condition, null);
    }
    
    public static final void assertTrueParam(String paramName, int idx, boolean condition, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertTrue(condition);
            } else {
                assertTrue(condition, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    
    /**
     * Check condition as FALSE
     */
    public static final void assertFalseParam(String paramName, int idx, boolean condition) {
        assertFalseParam(paramName, idx, condition, null);
    }
    
    public static final void assertFalseParam(String paramName, int idx, boolean condition, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertFalse(condition);
            } else {
                assertFalse(condition, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    
    /**
     * Check condition NOT NULL
     */
    public static final <T> void assertNotNullParam(String paramName, int idx, T actual) {
        assertNotNullParam(paramName, idx, actual, null);
    }
    
    public static final <T> void assertNotNullParam(String paramName, int idx, T actual, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertNotNull(actual);
            } else {
                assertNotNull(actual, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    /**
     * Check condition NOT NULL
     */
    
    public static final <T> void assertNullParam(String paramName, int idx, T actual) {
        assertNullParam(paramName, idx, actual, null);
    }
    
    public static final <T> void assertNullParam(String paramName, int idx, T actual, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertNull(actual);
            } else {
                assertNull(actual, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    /**
     * Check condition EQUALS
     */
    public static final <T> void assertEqualsParam(String paramName, int idx, T expected, T actual) {
        assertEqualsParam(paramName, idx, actual, null);
    }
    
    public static final <T> void assertEqualsParam(String paramName, int idx, T expected, T actual, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertEquals(expected, actual);
            } else {
                assertEquals(expected, actual, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    
    /**
     * Check condition INSTANCE_OF
     */
    
    public static final void assertInstanceOfParam(String paramName, int idx, Object obj, Class<?> expectedClass) {
        assertInstanceOfParam(paramName, idx, obj, expectedClass, null);
    }
    
    public static final void assertInstanceOfParam(String paramName, int idx, Object obj, Class<?> expectedClass, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertInstanceOf(obj, expectedClass);
            } else {
                assertInstanceOf(obj, expectedClass, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    /**
     * Check not empty
     */
    public static final void assertHasLengthParam(String paramName, int idx, String expression) {
        assertHasLengthParam(paramName, idx, expression, null);
    }
    
    public static final void assertHasLengthParam(String paramName, int idx, String expression, String errorMessage) {
        try{
            if (errorMessage == null) {
                assertHasLength(expression);
            } else {
                assertHasLength(expression, errorMessage);
            }
        } catch(AssertionViolationException ave) {
            handleAssertException(paramName, idx, ave);
        }
    }
    
    /**
     * Handle assert exception.
     * 
     * @param paramName
     *          name of parameter
     * @param idx
     *          offset of parameter
     * @param ave
     *          current error
     */
    private static void handleAssertException(String paramName, int idx, AssertionViolationException ave) {
        throw new IllegalArgumentException(
                String.format("Param #%s (%s) %s", idx, paramName, ave.getMessage()), ave);
    }
 
    // -------------------------------------------------------------------------
    // ------------------- GETTERS & SETTERS -----------------------------------
    // -------------------------------------------------------------------------
    
    /**
     * Accessor to define the polling interval in asynchronous tests.
     *
     * @param pollingIntervalInMillis
     *      current polling interval in milliseconds
     */
    public static void setPollingInterval(long pollingIntervalInMillis) {
        AssertUtils.pollingInterval = pollingIntervalInMillis;
    }
    
    /**
     * Accessor to define the time limit for asynchronous tests.
     *
     * @param timeoutInMillis
     *      limit time in milliseconds
     */
    public static void setTimeout(long timeoutInMillis) {
        AssertUtils.timeout = timeoutInMillis;
    }

}
