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
global class UrlUtils {
    
    global static String getBase(String url){
        String[] split = StringUtils.split(url, '?');
        if(split == null || split.size() == 0){
            return null;
        }
        return split[0];
    }

    global static Map<String,String> getParams(String url){
        //url -> http://google.com?api=x&xyz=123
        Map<String,String> returnMap = new Map<String,String>();
        String[] split = StringUtils.split(url, '?');
        //split -> ['http://google.com','api=x&xyz=123']
        if(split == null || split.size() != 2 || split[1] == null){
            return returnMap;
        }
        split = StringUtils.split(split[1],'&');
        //split -> ['api=x','xyz=123']
        if(split != null && split.size() > 0){
            String[] split2 = null;
            for(String keyValuePair : split){
                //keyValuePair -> 'api=x'
                split2 = StringUtils.split(keyValuePair,'=');
                String key = '';
                if(split2 != null && split2.size() > 0) key = split2[0];
                String value = '';
                if(split2 != null && split2.size() > 1) value = split2[1];
                returnMap.put(key,EncodingUtil.urlDecode(value, 'UTF-8'));
            }
        }
        return returnMap;
    }

    global static String ensureUrlBeginsWithHttp(String url){
        if(StringUtils.isNotEmpty(url)){
            final String lowerCaseUrl = StringUtils.lowerCase(url);
            if(    !StringUtils.startsWith(lowerCaseUrl, 'http://') 
              &&   !StringUtils.startsWith(lowerCaseUrl, 'https://')){
              	if(StringUtils.contains(lowerCaseUrl, '//')) url = StringUtils.substringAfter(url, '//');
                url = 'http://'+ StringUtils.stripStart(url,'/:');
            }
        }
        return url;
    }

}