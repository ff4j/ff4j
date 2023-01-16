package org.ff4j.utils;

import static java.util.concurrent.CompletableFuture.failedFuture;

import java.util.Collection;
import java.util.Objects;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.function.BiPredicate;
import java.util.function.Predicate;
import java.util.function.Supplier;

import javax.lang.model.type.NullType;

import org.ff4j.exception.AssertionViolationException;

/**
 * Assertion utility class used to evaluate expression.
 */
public class Assert {
   
    /** default timeout (in millis). */
    private static final long DEFAULT_TIMEOUT = 2000;
    
    /** default polling interval (in millis). */
    private static final long DEFAULT_POLLING_INTERVAL = 100;

    /**
     * Hide public constructor (Utility class)
     */
    private Assert() {}
    
    // -------------------------------------
    // ---------- ASSERT TRUE --------------
    // -------------------------------------

    /** default error message. */
    private static final String DEFAULT_MESSAGE_TRUE = "Expected to be TRUE but was not";

    /**
     * Default, synchronous assertion.
     *
     * @param result
     *      condition to evaluate
     **/
    public static void assertTrue(boolean result) {
        assertSync(true, result);
    }

    /**
     * Evaluate a condition as True.
     *
     * @param result
     *      condition to evaluate
     * @param errorMessage
     *      error message
     */
    public static void assertTrue(boolean result, String errorMessage) {
        assertSync(true, result, errorMessage);
    }
    
    /**
     * Synchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code> 
     * @param <T>
     *     predicate generic
     */
    public static <T> void assertTrue(Predicate<T> assertion, T param) {
        assertTrueWithRetries(assertion, param, -1, 0);
    }

    /**
     * Evaluate a predicate as true.
     *
     * @param assertion
     *      predicate to evaluate
     * @param param
     *      expression as input of predicate <code>assertion.test(param)</code>
     * @param errorMessage
     *      specialized error message
     * @param <T>
     *      predicate target
     */
    public static <T> void assertTrue(Predicate<T> assertion, T param, String errorMessage) {
        assertTrueWithRetries(assertion, param, errorMessage, -1, 0);
    }

    /**
     * Evaluate a predicate as true.
     *
     * @param assertion
     *      predicate to evaluate
     * @param param
     *      expression as input of predicate <code>assertion.test(param)</code>
     * @param errorMessage
     *      specialized error message
     * @param <T>
     *      predicate target
     */
    public static <T> CompletionStage<Void> assertTrueASync(Predicate<T> assertion, T param, String errorMessage) {
        return CompletableFuture.supplyAsync(() -> assertion.test(param))
                                .thenCompose(b -> b ? new CompletableFuture<>() :
                        failedFuture(new AssertionViolationException(errorMessage)));
    }

    /**
     * Asynchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code> 
     * @return
     *          number of iteration until result correct
     */
    public static <T> int assertTrueWithRetries(Predicate<T> assertion, T param) {
        return assertTrueWithRetries(assertion, param, DEFAULT_MESSAGE_TRUE, DEFAULT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
    }

    /**
     * Asynchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code>
     * @param errorMessage
     *          specialized error message
     * @return
     *          number of iteration until result correct
     */
    public static <T> int assertTrueWithRetries(Predicate<T> assertion, T param, String errorMessage) {
        return assertTrueWithRetries(assertion, param, errorMessage, DEFAULT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
    }

    /**
     * Asynchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code>
     * @param timeout
     *          timeout for retries
     * @param pollingInterval
     *          polling interval for retries
     * @return
     *      number of iteration until result correct
     */
    public static <T> int assertTrueWithRetries(Predicate<T> assertion, T param, long timeout, long pollingInterval) {
        return assertWithRetries(true, assertion, param, timeout, pollingInterval, DEFAULT_MESSAGE_FALSE);
    }

    /**
     * Asynchronous assertion using a predicate.
     *
     * @param assertion
     *          predicate to evaluate
     * @param param
     *          expression as input of predicate <code>assertion.test(param)</code>
     * @param errorMessage
     *          specialized error message
     * @return
     *          number of iteration until result correct
     */
    public static <T> int assertTrueWithRetries(Predicate<T> assertion, T param, String errorMessage, long timeout, long pPollingInterval) {
        return assertWithRetries(true, assertion, param, timeout, pPollingInterval, errorMessage);
    }
    
   // -------------------------------------
   // ---------- ASSERT FALSE --------------
   // -------------------------------------
    
   /** default error message. */
   private static final String DEFAULT_MESSAGE_FALSE = "Expected to be FALSE but was not";
   
   /**
    * Default, synchronous assertion.
    *
    * @param result
    *      condition to evaluate
    * @throws AssertionViolationException
    *      specific runtime exception to detail invalid condition
    **/
   public static void assertFalse(boolean result) {
       assertSync(false, result);
   }
   public static void assertFalse(boolean result, String errorMessage) {
       assertSync(false, result, errorMessage);
   }
   
   /**
    * Synchronous assertion using a predicate.
    *
    * @param assertion
    *          predicate to evaluate
    * @param param
    *          expression as input of predicate <code>assertion.test(param)</code>
    */
   public static <T> void assertFalse(Predicate<T> assertion, T param) {
       assertFalseAsync(assertion, param, -1, 0);
   }
   
   public static <T> void assertFalse(Predicate<T> assertion, T param, String errorMessage) {
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
   public static <T> int assertFalseAsync(Predicate<T> assertion, T param) {
       return assertFalseAsync(assertion, param, DEFAULT_MESSAGE_FALSE, DEFAULT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
   }
   public static <T> int assertFalseAsync(Predicate<T> assertion, T param, String errorMessage) {
       return assertFalseAsync(assertion, param, errorMessage, DEFAULT_TIMEOUT, DEFAULT_POLLING_INTERVAL);
   }
   public static <T> int assertFalseAsync(Predicate<T> assertion, T param, String errorMessage, long timeout, long pPollingInterval) {
       return assertWithRetries(false, assertion, param, timeout, pPollingInterval, errorMessage);
   }
   public static <T> int assertFalseAsync(Predicate<T> assertion, T param, long timeout, long pPollingInterval) {
       return assertWithRetries(false, assertion, param, timeout, pPollingInterval, DEFAULT_MESSAGE_FALSE);
   }
   
   // -------------------------------------
   // ---------- ASSERT EQUALS ------------
   // -------------------------------------
   
   /** default error message. */
   private static final String DEFAULT_MESSAGE_EQUALS = "<%s> and <%s> Expected to be EQUALS but was not";
  
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
           return Objects.equals(expected, actual);
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
    public static <T> void assertEquals(T expected, T actual) {
        assertEquals(expected, actual, String.format(DEFAULT_MESSAGE_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static  <T> void assertEquals(T expected, T actual, String errorMessage) {
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
    public static <T> int assertEqualsASync(T expected, Supplier<T> actual) {
        return assertEqualsASync(expected, actual, String.format(DEFAULT_MESSAGE_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static  <T> int assertEqualsASync(T expected, Supplier<T> actual, String errorMessage) {
        return assertTrueWithRetries(x -> new EqualsCheck().test(expected, actual.get()), errorMessage);
    }
    
    // ----------------------------------
    // ---------- ASSERT NOT EQUALS -----
    // ----------------------------------
      
    /** default error message. */
    private static final String DEFAULT_MESSAGE_NOT_EQUALS = "<%s> and <%s> Expected to be DIFFERENT but was EQUALS";
    
    /**
     * To evaluate equality a predicate is defined, then the assertTrue component will be
     * reused. This present test is performed synchronously.
     * 
     * @param expected
     *          expected value
     * @param actual    
     *          current value to test against
     */
    public static <T> void assertNotEquals(T expected, T actual) {
        assertNotEquals(expected, actual, String.format(DEFAULT_MESSAGE_NOT_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static  <T> void assertNotEquals(T expected, T actual, String errorMessage) {
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
    public static <T> int assertNotEqualsASync(T expected, Supplier<T> actual) {
        return assertNotEqualsASync(expected, actual, String.format(DEFAULT_MESSAGE_NOT_EQUALS, expected, actual));
    }
    
    @SuppressWarnings({"unchecked","rawtypes"})
    public static <T> int assertNotEqualsASync(T expected, Supplier<T> actual, String errorMessage) {
        return assertFalseAsync(x -> new EqualsCheck().test(expected, actual.get()), errorMessage);
    }
    
    // -------------------------------------
    // ---------- ASSERT NULL   ------------
    // -------------------------------------
    
    /** default error message. */
    private static final String DEFAULT_MESSAGE_NULL = "<%s> is expected to be null but was not";
   
    /**
     * Default, synchronous assertion test nullity of parameter
     *
     * @param actual
     *      object to evaluate
     * @throws AssertionViolationException
     *      specific runtime exception to detail invalid condition
     **/
    public static <T> void assertNull(T actual) {
        assertTrue((actual==null), DEFAULT_MESSAGE_NULL);
    }
    
    public static <T> void assertNull(T actual, String errorMessage) {
        assertTrue((actual==null), errorMessage);
    }
    
    public static <T> int assertNullAsync(Supplier<T> actual) {
        return assertEqualsASync(null, actual);
    }
    
    public static <T> int assertNullAsync(Supplier<T> actual, String errorMessage) {
        return assertEqualsASync(null, actual, errorMessage);
    }
    
    // -------------------------------------
    // ------- ASSERT NOT NULL   -----------
    // -------------------------------------
    
    /** default error message. */
    private static final String DEFAULT_MESSAGE_NOTNULL = "<%s> must not be null";
    
    /**
     * Synchronous assertion test nullity of parameter
     *
     * @param actual
     *      object to evaluate
     * @throws AssertionViolationException
     *      specific runtime exception to detail invalid condition
     **/
    public static <T> void assertNotNull(T actual) {
        assertNotNull(actual, DEFAULT_MESSAGE_NOTNULL);
    }
    
    public static <T> void assertNotNull(T actual, String errorMessage) {
        assertFalse(actual==null, errorMessage);
    }
    
    public static  <T> int assertWithRetries(Supplier<T> actual) {
        return assertNotEqualsASync(null, actual);
    }
    
    public static <T> CompletableFuture<Void> assertNotNullAsync(T str) {
        return CompletableFuture.runAsync(() -> assertNotNull(str));
    }
    
    
    // -------------------------------------
    // ------- Valid Class       -----------
    // -------------------------------------

    /** Predicate to evaluate if  string content is not empty. */
    private static final Predicate<Class<?>> VALID_CLASS = clazz -> (clazz != null) && (clazz != NullType.class);
    
    /**
     * ASSERT hasLength
     */
    public static void assertValidClass(Class<?> clazz) {
        assertTrue(VALID_CLASS.test(clazz));
    }
    
    // -------------------------------------
    // ------- ASSERT HAS LENGTH -----------
    // -------------------------------------
    
    /** Predicate to evaluate if  string content is not empty. */
    private static Predicate<String> HAS_LENGTH = str -> (str!=null) && !str.isEmpty();
    
    /** default error message. */
    private static final String MSG_HAS_LENGTH = "'%s' should not be null nor empty.";

    /**
     * Assess if a string is not empty.
     *
     * @param str
     *      target string
     */
    public static void assertHasLength(String str) {
        assertTrue(HAS_LENGTH.test(str));
    }

    /**
     * Assess if a string is not empty.
     *
     * @param str
     *      target string
     * @param objectName
     *      parameter name
     */
    public static void assertHasLength(String str, String objectName) {
        assertTrue(HAS_LENGTH.test(str), String.format(MSG_HAS_LENGTH, objectName));
    }
    
    public static CompletableFuture<Void> assertHasLengthAsync(String str) {
        return CompletableFuture.runAsync(() -> assertHasLength(str));
    }
    
    // -------------------------------------
    // ------- ASSERT INSTANCE_OF ----------
    // -------------------------------------
    
    /** default error message. */
    private static final String MSG_INSTANCE_OF = "Invalid class expected %s but was %s";
    
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

    private static final String DEFAULT_MESSAGE_ASSERT = "Expected <%s> but was <%s>";

    private static final String DEFAULT_MESSAGE_ASSERT_LIST =  "Expected <%s> but was <%s> in %s";

    /**
     * Check that object is not null.
     * 
     * @param collection
     *            target object
     */
    public static <T> void assertNotEmpty(Collection<T> collection) {
        assertNotNull(collection);
        assertFalse(collection.isEmpty());
    }
    
    /**
     * Check that string is not null
     *
     * @param collection
     *            target object
     */
    public static <T> void assertNotEmpty(T[] collection) {
        assertNotNull(collection);
        assertTrue(collection.length > 0);
    }    
    
    /**
     * Test assertion several times.
     *
     * @param timeout
     *      if the assertion is still not true at end , fail.
     * @param pPollingInterval
     *      time between 2 tries of the assertion
     * @param assertion
     *      value to test
     * @param param
     *      parameter
     * @param errorMessage
     *      target error message
     */
    private static <T> int assertWithRetries(boolean expected, Predicate<T> assertion, T param, long timeout, long pPollingInterval, String errorMessage) {
        long start = System.currentTimeMillis();
        boolean result = assertion.test(param);
        try {
            int assessCount = 1;
            // As long as the expected condition is not validated (!expected)
            while ((result != expected) && (System.currentTimeMillis() - start) <= timeout) {
                result = assertion.test(param);
                Thread.sleep(pPollingInterval);
                assessCount++;
            }
            assertSync(expected, result, String.format(DEFAULT_MESSAGE_ASSERT_LIST, expected, result, errorMessage));
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
    private static void assertSync(boolean expected, boolean result) {
        assertSync(expected, result, String.format(DEFAULT_MESSAGE_ASSERT, expected, result));
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
    private static void assertSync(boolean expected, boolean result, String errorMessage) {
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
    public static void assertTrueParam(String paramName, int idx, boolean condition) {
        assertTrueParam(paramName, idx, condition, null);
    }
    
    public static  void assertTrueParam(String paramName, int idx, boolean condition, String errorMessage) {
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
    public static void assertFalseParam(String paramName, int idx, boolean condition) {
        assertFalseParam(paramName, idx, condition, null);
    }
    
    public static void assertFalseParam(String paramName, int idx, boolean condition, String errorMessage) {
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
    public static <T> void assertNotNullParam(String paramName, int idx, T actual) {
        assertNotNullParam(paramName, idx, actual, null);
    }
    
    public static <T> void assertNotNullParam(String paramName, int idx, T actual, String errorMessage) {
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
    public static <T> void assertNullParam(String paramName, int idx, T actual) {
        assertNullParam(paramName, idx, actual, null);
    }
    
    public static <T> void assertNullParam(String paramName, int idx, T actual, String errorMessage) {
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
     * Evaluate method parameters.
     *
     * @param paramName
     *          parameter name
     * @param idx
     *          parameter index
     * @param expected
     *          expected value
     * @param actual
     *          value retrieved
     * @param <T>
     *          predicate type
     */
    public static <T> void assertEqualsParam(String paramName, int idx, T expected, T actual, String errorMessage) {
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
    
    public static void assertInstanceOfParam(String paramName, int idx, Object obj, Class<?> expectedClass) {
        assertInstanceOfParam(paramName, idx, obj, expectedClass, null);
    }
    
    public static void assertInstanceOfParam(String paramName, int idx, Object obj, Class<?> expectedClass, String errorMessage) {
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
    public static void assertHasLengthParam(String paramName, int idx, String expression) {
        assertHasLengthParam(paramName, idx, expression, null);
    }
    
    public static void assertHasLengthParam(String paramName, int idx, String expression, String errorMessage) {
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
        throw new IllegalArgumentException(String.format("Param #%s (%s) %s", idx, paramName, ave.getMessage()), ave);
    }

}
