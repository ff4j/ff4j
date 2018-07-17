package org.ff4j.aop.cglib;

import org.aspectj.lang.ProceedingJoinPoint;
import org.aspectj.lang.annotation.Around;
import org.springframework.stereotype.Component;

@Component
@org.aspectj.lang.annotation.Aspect
public class Aspect {

    @Around("execution(* org.ff4j..*.*(..))")
    public Object aroundRestService(ProceedingJoinPoint proceedingJoinPoint) throws Throwable {
        return proceedingJoinPoint.proceed();
    }
}