/**
 * Project Name:demo8
 * File Name:MyConfiger.java
 * Package Name:com.testin
 * Date:2016年12月16日上午12:45:03
 * Copyright (c) 2016, All Rights Reserved.
 *
 */
package com.testin;

import java.lang.reflect.Method;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.Executor;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy;
import java.util.concurrent.TimeUnit;

import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.AsyncConfigurer;
import org.springframework.scheduling.annotation.EnableAsync;

/**
 * ClassName:MyConfiger <br/>
 * Function: <br/>
 * Date: 2016年12月16日 上午12:45:03 <br/>
 * 
 * @author xushjie
 * @version
 * @since JDK 1.8
 * @see
 */
@EnableAsync
@Configuration
public class MyConfiger implements AsyncConfigurer {
    
    @Override
    public Executor getAsyncExecutor() {
        // ThreadPoolExecutor executor = (ThreadPoolExecutor) Executors.newFixedThreadPool(3);
        ThreadPoolExecutor executor = new ThreadPoolExecutor(2,
                                                             2,
                                                             0L,
                                                             TimeUnit.MILLISECONDS,
                                                             new ArrayBlockingQueue<Runnable>(1));
        executor.setRejectedExecutionHandler(new CallerRunsPolicy());
        // ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
        // executor.setCorePoolSize(2);
        // executor.setMaxPoolSize(2);
        // executor.setQueueCapacity(1);
        // executor.setThreadNamePrefix("MyExecutor-");
        // executor.initialize();
        return executor;
    }
    
    @Override
    public AsyncUncaughtExceptionHandler getAsyncUncaughtExceptionHandler() {
        return new AsyncUncaughtExceptionHandler() {
            
            @Override
            public void handleUncaughtException(Throwable arg0,
                                                Method arg1,
                                                Object... arg2) {
                System.out.println("Uncaught: " + arg1.getName());
            }
        };
    }
    
}
