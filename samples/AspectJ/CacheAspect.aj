package com.blogspot.miguelinlas3.aspectj.cache;

import java.util.Map;
import java.util.WeakHashMap;

import org.aspectj.lang.JoinPoint;

import com.blogspot.miguelinlas3.aspectj.cache.marker.Cachable;

/**
 * This simple aspect simulates the behaviour of a very simple cache
 *  
 * @author migue
 *
 */
public aspect CacheAspect {

	public pointcut cache(Cachable cachable): execution(@Cachable * * (..)) && @annotation(cachable);
	
	Object around(Cachable cachable): cache(cachable){
	
		String evaluatedKey = this.evaluateKey(cachable.scriptKey(), thisJoinPoint);
		
		if(cache.containsKey(evaluatedKey)){
			System.out.println("Cache hit for key " + evaluatedKey);
			return this.cache.get(evaluatedKey);
		}
		
		System.out.println("Cache miss for key " + evaluatedKey);
		Object value = proceed(cachable);
		cache.put(evaluatedKey, value);
		return value;
	}
	
	protected String evaluateKey(String key, JoinPoint joinPoint) {
		// TODO add some smart staff to allow simple scripting in @Cachable annotation
		return key;
	}
	
	protected Map<String, Object> cache = new WeakHashMap<String, Object>();
}
