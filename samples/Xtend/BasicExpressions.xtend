/*******************************************************************************
 * Copyright (c) 2012 itemis AG (http://www.itemis.eu) and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package example2

import org.junit.Test
import static org.junit.Assert.*


class BasicExpressions {
	
	@Test def void literals() {
		// string literals work with single or double quotes
		assertEquals('Hello', "Hello")
		
		// number literals (big decimals in this case)
		assertEquals(42, 20 + 20 + 1 * 2)
		assertEquals(42.00bd, 0.00bd + 42bd)
		
		// boolean literals
		assertEquals(true, !false)
		
		// class literals
		assertEquals(getClass(), typeof(BasicExpressions))
	}
	
	@Test def void collections() {
		// There are various static methods to create collections
		// and numerous extension methods which make working with them
		// convenient.
		val list = newArrayList('Hello', 'World')
		assertEquals('HELLO', list.map[toUpperCase].head)
		
		val set  = newHashSet(1, 3, 5)
		assertEquals(2, set.filter[ it >= 3].size)
		
		val map  = newHashMap('one' -> 1, 'two' -> 2, 'three' -> 3)
		assertEquals( 2 , map.get('two'))
	}
	
	@Test def void controlStructures() {
		// 'if' looks like in Java
		if ('text'.length == 4) {
			// but it's an expression so it can be used in more flexible ways:
			assertEquals( 42 , if ('foo' != 'bar') 42 else -24 )
		} else {
			fail('Never happens!')
		}
		
		// in a switch the first match wins
		switch (t : 'text') {
			// use predicates
			case t.length > 8 :
				fail('Never happens!')
			// use equals
			case 'text' :
				assertTrue(true)
			default :
				fail('never happens!')
		}
		
		// switch also supports type guards, which provide a safe 
		// and convenient alternative to Java's 'instanceof'-cascades.
		val Object someValue = 'a string typed to Object'
		assertEquals('string', 
			switch someValue {
				Number : 'number'
				String : 'string' 
			})
	}
	
	@Test def void loops() {
		// for loop
		var counter = 1
		for (i : 1 .. 10) {
			assertEquals(counter, i)
			counter = counter + 1
		}
		
		// while loop
		val iterator = newArrayList(1,2,3,4,5).iterator
		counter = 1
		while(iterator.hasNext) {
			val i = iterator.next
			assertEquals(counter, i)
			counter = counter + 1
		}
	}
}
