/* ============================================================
 * This code is part of the "apex-lang" open source project avaiable at:
 * 
 *      http://code.google.com/p/apex-lang/
 *
 * This code is licensed under the Apache License, Version 2.0.  You may obtain a 
 * copy of the License at:
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * ============================================================
 */
global class ArrayUtils {   
    
    global static String[] EMPTY_STRING_ARRAY = new String[]{};
    global static Integer MAX_NUMBER_OF_ELEMENTS_IN_LIST {get{return 1000;}}
    
    global static List<String> objectToString(List<Object> objects){
        List<String> strings = null;
        if(objects != null){
        	strings = new List<String>();
        	if(objects.size() > 0){
	            for(Object obj : objects){
	                if(obj instanceof String){
	                    strings.add((String)obj);
	                }
	            }
        	}
        }
        return strings;
    }

    global static Object[] reverse(Object[] anArray) {
        if (anArray == null) {
            return null;
        }
        Integer i = 0;
        Integer j = anArray.size() - 1;
        Object tmp;
        while (j > i) {
            tmp = anArray[j];
            anArray[j] = anArray[i];
            anArray[i] = tmp;
            j--;
            i++;
        }
        return anArray;
    }
    
    global static SObject[] reverse(SObject[] anArray) {
        if (anArray == null) {
            return null;
        }
        Integer i = 0;
        Integer j = anArray.size() - 1;
        SObject tmp;
        while (j > i) {
            tmp = anArray[j];
            anArray[j] = anArray[i];
            anArray[i] = tmp;
            j--;
            i++;
        }
        return anArray;
    }
    
    global static List<String> lowerCase(List<String> strs){
        List<String> returnValue = null;
        if(strs != null){
            returnValue = new List<String>();
        	if(strs.size() > 0){
	            for(String str : strs){
	                returnValue.add(str == null ? null : str.toLowerCase());
	            }
            }
        }
        return returnValue;
    }
    
    global static List<String> upperCase(List<String> strs){
        List<String> returnValue = null;
        if(strs != null){
            returnValue = new List<String>();
            if(strs.size() > 0){
	            for(String str : strs){
	                returnValue.add(str == null ? null : str.toUpperCase());
	            }
            }
        }
        return returnValue;
    }
    
    global static List<String> trim(List<String> strs){
        List<String> returnValue = null;
        if(strs != null){
            returnValue = new List<String>();
            if(strs.size() > 0){
	            for(String str : strs){
	                returnValue.add(str == null ? null : str.trim());
	            }
            }
        }
        return returnValue;
    }
    
    global static Object[] mergex(Object[] array1, Object[] array2){
        if(array1 == null){ return array2; }
        if(array2 == null){ return array1; }
        Object[] merged = new Object[array1.size() + array2.size()];
        for(Integer i = 0; i < array1.size(); i++){
            merged[i] = array1[i];
        }
        for(Integer i = 0; i < array2.size(); i++){
            merged[i+array1.size()] = array2[i];
        }
        return merged;
    }   
     
    global static SObject[] mergex(SObject[] array1, SObject[] array2){
        if(array1 == null){ return array2; }
        if(array2 == null){ return array1; }
        if(array1.size() <= 0){ return array2; }
        List<SObject> merged = new List<SObject>();
        for(SObject sObj : array1){ merged.add(sObj); }
        for(SObject sObj : array2){ merged.add(sObj); }
        return merged;
    }   
    
    global static Boolean isEmpty(Object[] objectArray){
        if(objectArray == null){
            return true;
        }
        return objectArray.size() == 0;
    }
    
    global static Boolean isEmpty(SObject[] objectArray){
        if(objectArray == null){
            return true;
        }
        return objectArray.size() == 0;
    }
    
    global static Boolean isNotEmpty(Object[] objectArray){
        return !isEmpty(objectArray);
    }
    
    global static Boolean isNotEmpty(SObject[] objectArray){
        return !isEmpty(objectArray);
    }
    
    global static Object[] pluck(SObject[] objectArray, String fieldName){
        if(isEmpty(objectArray) || fieldName == null || fieldName.trim() == null || fieldName.trim().length() == 0){
            return new Object[]{};
        }
        Object[] plucked = new Object[objectArray.size()];
        for(Integer i = 0; i < objectArray.size(); i++){
            plucked[i] = objectArray[i].get(fieldName);
        }
        return plucked;
    }
    
    
    global static String toString(Object[] objectArray){
        if(objectArray == null){
            return 'null';    
        }
        String returnValue = '{';
        for(Integer i = 0; i < objectArray.size(); i++){
            if(i!=0){ returnValue += ','; }
            returnValue += '\'' + objectArray[i] + '\'';
        }
        returnValue += '}';
        return returnValue; 
    }
    
    global static String toString(SObject[] objectArray){
        if(objectArray == null){
            return 'null';    
        }
        String returnValue = '{';
        for(Integer i = 0; i < objectArray.size(); i++){
            if(i!=0){ returnValue += ','; }
            returnValue += '\'' + objectArray[i] + '\'';
        }
        returnValue += '}';
        return returnValue; 
    }
    
    global static void assertArraysAreEqual(Object[] expected, Object[] actual){
        //check to see if one param is null but the other is not
        System.assert((expected == null && actual == null)|| (expected != null && actual != null),
            'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
        if(expected != null && actual != null){
            System.assert(expected.size() == actual.size(), 'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
            for(Integer i = 0; i < expected.size(); i++){
                System.assert(expected[i] == actual[i], 'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
            }
        }
    }
    
    global static void assertArraysAreEqual(SObject[] expected, SObject[] actual){
        //check to see if one param is null but the other is not
        System.assert((expected == null && actual == null)|| (expected != null && actual != null),
            'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
        if(expected != null && actual != null){
            System.assert(expected.size() == actual.size(), 'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
            for(Integer i = 0; i < expected.size(); i++){
                System.assert(expected[i] == actual[i], 'Assertion failed, the following two arrays are not equal.  Expected: ' 
                    + ArrayUtils.toString(expected) + ', Actual: ' + ArrayUtils.toString(actual));
            }
        }
    }
    
    global static List<Object> merg(List<Object> list1, List<Object> list2) {
        List<Object> returnList = new List<Object>();
        if(list1 != null && list2 != null && (list1.size()+list2.size()) > MAX_NUMBER_OF_ELEMENTS_IN_LIST){
            throw new IllegalArgumentException('Lists cannot be merged because new list would be greater than maximum number of elements in a list: ' + MAX_NUMBER_OF_ELEMENTS_IN_LIST);
        }
        if(isNotEmpty(list1)){
            for(Object elmt : list1){
                returnList.add(elmt);
            }
        }
        if(isNotEmpty(list2)){
            for(Object elmt : list2){
                returnList.add(elmt);
            }
        }
        return returnList;
    }

    
    global static List<SObject> merg(List<SObject> list1, List<SObject> list2) {
        if(list1 != null && list2 != null && (list1.size()+list2.size()) > MAX_NUMBER_OF_ELEMENTS_IN_LIST){
            throw new IllegalArgumentException('Lists cannot be merged because new list would be greater than maximum number of elements in a list: ' + MAX_NUMBER_OF_ELEMENTS_IN_LIST);
        }
        if(isEmpty(list1) && isEmpty(list2)){
            return null;
        }
        List<SObject> returnList = new List<SObject>();
        if(list1 != null){
            for(SObject elmt : list1){
                returnList.add(elmt);
            }
        }
        if(list2 != null){
            for(SObject elmt : list2){
                returnList.add(elmt);
            }
        }
        return returnList;
    }
    
    global static List<Object> subset(List<Object> aList, Integer count) {
        return subset(aList,0,count);
    }

    global static List<Object> subset(List<Object> list1, Integer startIndex, Integer count) {
        List<Object> returnList = new List<Object>();
        if(list1 != null && list1.size() > 0 && startIndex >= 0 && startIndex <= list1.size()-1 && count > 0){
            for(Integer i = startIndex; i < list1.size() && i - startIndex < count; i++){
                returnList.add(list1.get(i));
            }
        }
        return returnList;
    }

    
    global static List<SObject> subset(List<SObject> aList, Integer count) {
        return subset(aList,0,count);
    }

    global static List<SObject> subset(List<SObject> list1, Integer startIndex, Integer count) {
        List<SObject> returnList = null;
        if(list1 != null && list1.size() > 0 && startIndex <= list1.size()-1 && count > 0){
            returnList = new List<SObject>();
            for(Integer i = startIndex; i < list1.size() && i - startIndex < count; i++){
                returnList.add(list1.get(i));
            }
        }
        return returnList;
    }
    
    //===============================================
    //LIST/ARRAY SORTING
    //===============================================

    //FOR FORCE.COM PRIMITIVES (Double,Integer,ID,etc.):
    global static List<Object> qsort(List<Object> theList) {
        return qsort(theList,new PrimitiveComparator());
    }

    global static List<Object> qsort(List<Object> theList, Boolean sortAsc) {
        return qsort(theList,new PrimitiveComparator(),sortAsc);
    }
    
    global static List<Object> qsort(List<Object> theList, ObjectComparator comparator) {
        return qsort(theList,comparator,true);
    }
    
    global static List<Object> qsort(List<Object> theList, ObjectComparator comparator, Boolean sortAsc) {
        return qsort(theList, 0, (theList == null ? 0 : theList.size()-1),comparator,sortAsc);
    }
    

    
    //FOR SALESFORCE OBJECTS (sObjects):
    global static List<SObject> qsort(List<SObject> theList, ISObjectComparator comparator) {
        return qsort(theList,comparator,true);
    }
    
    global static List<SObject> qsort(List<SObject> theList, ISObjectComparator comparator,Boolean sortAsc ) {
        return qsort(theList,  0, (theList == null ? 0 : theList.size()-1),comparator,sortAsc);
    }

    private static List<Object> qsort(List<Object> theList,
                                Integer lo0, 
                                Integer hi0, 
                                ObjectComparator comparator,
                                Boolean sortAsc){
        Integer lo = lo0;
        Integer hi = hi0;
     
        if (lo >= hi) {
            return theList;
        } else if( lo == hi - 1 ) {
        
            if (( comparator.compare(theList[lo],theList[hi])>0 && sortAsc) || 
                  (comparator.compare(theList[lo],theList[hi])<0 && !sortAsc)    
                ) {
                 Object prs = theList[lo];
                 theList[lo]         = theList[hi];
                 theList[hi]         = prs;
            }
            return theList;
        }

        Object pivot = theList[(lo + hi) / 2];
        theList[(lo + hi) / 2] = theList[hi];
        theList[hi] = pivot;

        while( lo < hi ) {
            while ((comparator.compare(theList[lo], pivot)<=0 && lo < hi && sortAsc) || 
                   (comparator.compare(theList[lo], pivot)>=0 && lo < hi && !sortAsc)
                  ) { lo++; }
            while (( comparator.compare(pivot,theList[hi])<=0 && lo < hi && sortAsc) ||
                   ( comparator.compare(pivot,theList[hi])>=0 && lo < hi && !sortAsc)
                  ) { hi--; }
            
            if( lo < hi ){
                 Object prs = theList[lo];
                 theList[lo]   = theList[hi];
                 theList[hi]    = prs;
            }
        }

        theList[hi0] = theList[hi];
        theList[hi] = pivot;
        
        qsort(theList, lo0, lo-1,comparator,sortAsc);
        qsort(theList, hi+1, hi0,comparator,sortAsc);
        return theList;
    }    
        
    
    private static List<SObject> qsort(List<SObject> theList,
                                Integer lo0, 
                                Integer hi0, 
                                ISObjectComparator comparator,
                                Boolean sortAsc){
        Integer lo = lo0;
        Integer hi = hi0;
     
        if (lo >= hi) {
            return theList;
        } else if( lo == hi - 1 ) {
        
            if (( comparator.compare(theList[lo],theList[hi])>0 && sortAsc) || 
                  (comparator.compare(theList[lo],theList[hi])<0 && !sortAsc)    
                ) {
                 SObject prs = theList[lo];
                 theList[lo]         = theList[hi];
                 theList[hi]         = prs;
            }
            return theList;
        }

        SObject pivot = theList[(lo + hi) / 2];
        theList[(lo + hi) / 2] = theList[hi];
        theList[hi] = pivot;

        while( lo < hi ) {
            while ((comparator.compare(theList[lo], pivot)<=0 && lo < hi && sortAsc) || 
                   (comparator.compare(theList[lo], pivot)>=0 && lo < hi && !sortAsc)
                  ) { lo++; }
            while (( comparator.compare(pivot,theList[hi])<=0 && lo < hi && sortAsc) ||
                   ( comparator.compare(pivot,theList[hi])>=0 && lo < hi && !sortAsc)
                  ) { hi--; }
            
            if( lo < hi ){
                 SObject prs = theList[lo];
                 theList[lo]   = theList[hi];
                 theList[hi]    = prs;
            }
        }

        theList[hi0] = theList[hi];
        theList[hi] = pivot;
        
        qsort(theList, lo0, lo-1,comparator,sortAsc);
        qsort(theList, hi+1, hi0,comparator,sortAsc);
        return theList;
    }
/*
    global static List<Object> unique(List<Object> theList) {
        List<Object> uniques = new List<Object>();
        Set<Object> keys = new Set<Object>(); 
        if(theList != null && theList.size() > 0){
            for(Object obj : theList){
                if(keys.contains(obj)){
                    continue;
                } else {
                    keys.add(obj);
                    uniques.add(obj);
                }
            }
        }
        return uniques;
    }

    global static List<SObject> unique(List<SObject> theList) {
        if(theList == null){
            return null;
        }
        List<SObject> uniques = createEmptySObjectList(theList.get(0));
        Set<String> keys = new Set<String>(); 
        if(theList != null && theList.size() > 0){
            String key = null;
            for(SObject obj : theList){
                key = obj == null ? null : ''+obj;
                if(keys.contains(key)){
                    continue;
                } else {
                    keys.add(key);
                    uniques.add(obj);
                }
            }
        }
        return uniques;
    }
*/


}