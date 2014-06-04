package aspects.caching;

import java.util.Map;

/**
 * Cache aspect for optimize recursive functions.
 * 
 * @author Migueli
 * @date 05/11/2013
 * @version 1.0
 *
 */
public abstract aspect OptimizeRecursionCache {
		
	@SuppressWarnings("rawtypes")
	private Map _cache;
	
	public OptimizeRecursionCache() {
		_cache = getCache();
	}
	
	@SuppressWarnings("rawtypes")
	abstract public Map getCache();
	
	abstract public pointcut operation(Object o);

	pointcut topLevelOperation(Object o): operation(o) && !cflowbelow(operation(Object));

	before(Object o) : topLevelOperation(o) {
		System.out.println("Seeking value for " + o);
	}

	Object around(Object o) : operation(o) {
		Object cachedValue = _cache.get(o);
		if (cachedValue != null) {
			System.out.println("Found cached value for " + o + ": " + cachedValue);
			return cachedValue;
		}
		return proceed(o);
	}

	@SuppressWarnings("unchecked")
	after(Object o) returning(Object result) : topLevelOperation(o) {
		_cache.put(o, result);
	}
	
	after(Object o) returning(Object result) : topLevelOperation(o) {
		System.out.println("cache size: " + _cache.size());
	}
}
