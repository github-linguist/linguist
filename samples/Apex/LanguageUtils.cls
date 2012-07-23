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
global class LanguageUtils {

    global static final String HTTP_LANGUAGE_CODE_PARAMETER_KEY = 'l';
	global static final String DEFAULT_LANGUAGE_CODE = 'en_us';

    global static Set<String> SUPPORTED_LANGUAGE_CODES = new Set<String>{
        'zh-cn'         //Chinese (Simplified)
        ,'zh-tw'        //Chinese (Traditional)
        ,'nl-nl'        //Dutch
        ,'en-us'        //English
        ,'fi'           //Finnish
        ,'fr'           //French
        ,'de'           //German
        ,'it'           //Italian
        ,'ja'           //Japanese
        ,'ko'           //Korean
        ,'pl'           //Polish
        ,'pt-br'        //Portuguese (Brazilian)
        ,'ru'           //Russian
        ,'es'           //Spanish
        ,'sv'           //Swedish
        ,'th'           //Thai
        ,'cs'           //Czech
        ,'da'           //Danish
        ,'hu'           //Hungarian
        ,'in'           //Indonesian
        ,'tr'           //Turkish
    };
    
    private static Map<String,String> DEFAULTS = new Map<String,String>{
        'en'=>'en-us'
        ,'zh'=>'zh-cn'
        ,'nl'=>'nl-nl'
        ,'pt'=>'pt-br'
    };
    

    global static String getLangCodeByHttpParam(){
    	String returnValue = null;
        final Set<String> LANGUAGE_CODE_SET = getSuppLangCodeSet();
        if(ApexPages.currentPage() != null && ApexPages.currentPage().getParameters() != null){
	        String LANGUAGE_HTTP_PARAMETER = 
	            StringUtils.lowerCase(
	                StringUtils.replaceChars( 
	                    ApexPages.currentPage().getParameters().get(HTTP_LANGUAGE_CODE_PARAMETER_KEY)
	                    , '_' //underscore
	                    , '-' //dash
	                )
	            );
	        if(DEFAULTS.containsKey(LANGUAGE_HTTP_PARAMETER)){
	            LANGUAGE_HTTP_PARAMETER = DEFAULTS.get(LANGUAGE_HTTP_PARAMETER);
	        }
	        if(StringUtils.isNotBlank(LANGUAGE_HTTP_PARAMETER)
	            && SUPPORTED_LANGUAGE_CODES.contains(LANGUAGE_HTTP_PARAMETER)){
	            returnValue = LANGUAGE_HTTP_PARAMETER;
	        }        
        }
        return returnValue;
    }

    global static String getLangCodeByBrowser(){
        final String LANGUAGES_FROM_BROWSER_AS_STRING = ApexPages.currentPage().getHeaders().get('Accept-Language');
        final List<String> LANGUAGES_FROM_BROWSER_AS_LIST = splitAndFilterAcceptLanguageHeader(LANGUAGES_FROM_BROWSER_AS_STRING);
        if(LANGUAGES_FROM_BROWSER_AS_LIST != null && LANGUAGES_FROM_BROWSER_AS_LIST.size() > 0){
            for(String languageFromBrowser : LANGUAGES_FROM_BROWSER_AS_LIST){
                if(DEFAULTS.containsKey(languageFromBrowser)){
                    languageFromBrowser = DEFAULTS.get(languageFromBrowser);
                }
                if(SUPPORTED_LANGUAGE_CODES.contains(languageFromBrowser)){
                    return languageFromBrowser;
                }
            }               
        }
        return null;
    }
    
    global static String getLangCodeByUser(){
        return UserInfo.getLanguage();
    }
    
    global static String getLangCodeByHttpParamOrIfNullThenBrowser(){
        return StringUtils.defaultString(getLangCodeByHttpParam(),getLangCodeByBrowser());
    }

    global static String getLangCodeByHttpParamOrIfNullThenUser(){
        return StringUtils.defaultString(getLangCodeByHttpParam(),getLangCodeByUser());
    }
    
    global static String getLangCodeByBrowserOrIfNullThenHttpParam(){
        return StringUtils.defaultString(getLangCodeByBrowser(),getLangCodeByHttpParam());
    }
    
    global static String getLangCodeByBrowserOrIfNullThenUser(){
        return StringUtils.defaultString(getLangCodeByBrowser(),getLangCodeByUser());
    }
    
    private static List<String> splitAndFilterAcceptLanguageHeader(String header){
        List<String> returnList = new List<String>();
        String[] tokens = StringUtils.split(header,',');
        if(tokens != null){
            for(String token : tokens){
                if(token != null ){
                    if(token.contains(';')){
                        token = token.substring(0,token.indexOf(';',0));
                    }
                    returnList.add(token);
                    if(StringUtils.length(token) > 2){
                        returnList.add(StringUtils.substring(token,0,2));
                    }
                }
            }       
        }
        return returnList;
    }
    
    private static Set<String> getSuppLangCodeSet(){
        Set<String> langCodes = new Set<String>();
        for(String langCode : SUPPORTED_LANGUAGE_CODES){
            if(langCode != null){ 
                langCodes.add(StringUtils.lowerCase(langCode));
            }
        }
        return langCodes;
    }

	
	global static String getLanguageName(String displayLanguageCode, String languageCode){
		return translatedLanguageNames.get(filterLanguageCode(displayLanguageCode)).get(filterLanguageCode(languageCode));
	}
	
	global static Map<String,String> getAllLanguages(){
		return getAllLanguages(DEFAULT_LANGUAGE_CODE);
	}
	
	global static Map<String,String> getAllLanguages(String displayLanguageCode){
		return translatedLanguageNames.get(filterLanguageCode(displayLanguageCode));
	}
	
	private static String filterLanguageCode(String displayLanguageCode){
		displayLanguageCode = StringUtils.lowerCase(displayLanguageCode);
		if(DEFAULTS.containsKey(displayLanguageCode)){
			displayLanguageCode = StringUtils.replaceChars(DEFAULTS.get(displayLanguageCode),'-','_');
		}
		if(!translatedLanguageNames.containsKey(displayLanguageCode)){
			displayLanguageCode = DEFAULT_LANGUAGE_CODE; 
		}
		return displayLanguageCode;
	}

	private static final Map<String,Map<String,String>> translatedLanguageNames = new Map<String,Map<String,String>>{
		'cs'=> new Map<String,String>{
'cs'=>'Čeština'
,'da'=>'Dánština'
,'de'=>'Němčina'
,'en_us'=>'Angličtina (Spojené státy)'
,'es'=>'Španělština'
,'es_mx'=>'Mexická španělština'
,'fi'=>'Finština'
,'fr'=>'Francouzština'
,'hu'=>'Maďarština'
,'in'=>'Indonéština'
,'it'=>'Italština'
,'ja'=>'Japonština'
,'ko'=>'Korejština'
,'nl_nl'=>'Nizozemština'
,'pl'=>'Polština'
,'pt_br'=>'Portugalština (Brazílie)'
,'ro'=>'Rumunština'
,'ru'=>'Ruština'
,'sv'=>'Švédština'
,'th'=>'Thajská'
,'tr'=>'Turečtina'
,'zh_cn'=>'Čínština (zjednodušená)'
,'zh_tw'=>'Čínština (tradiční)'
}
,'da'=> new Map<String,String>{
'cs'=>'Tjekkisk'
,'da'=>'Dansk'
,'de'=>'Tysk'
,'en_us'=>'Engelsk (USA)'
,'es'=>'Spansk'
,'es_mx'=>'Mexicansk spansk'
,'fi'=>'Finsk'
,'fr'=>'Fransk'
,'hu'=>'Ungarsk'
,'in'=>'Indonesisk'
,'it'=>'Italiensk'
,'ja'=>'Japansk'
,'ko'=>'Koreansk'
,'nl_nl'=>'Hollandsk'
,'pl'=>'Polsk'
,'pt_br'=>'Portugisisk (Brasilien)'
,'ro'=>'Rumænsk'
,'ru'=>'Russisk'
,'sv'=>'Svensk'
,'th'=>'Thai'
,'tr'=>'Tyrkisk'
,'zh_cn'=>'Kinesisk (forenklet)'
,'zh_tw'=>'Kinesisk (traditionelt)'
}
,'de'=> new Map<String,String>{
'cs'=>'Tschechisch'
,'da'=>'Dänisch'
,'de'=>'Deutsch'
,'en_us'=>'Englisch (Vereinigte Staaten)'
,'es'=>'Spanisch'
,'es_mx'=>'Mexican Spanish'
,'fi'=>'Finnisch'
,'fr'=>'Französisch'
,'hu'=>'Ungarisch'
,'in'=>'Indonesisch'
,'it'=>'Italienisch'
,'ja'=>'Japanisch'
,'ko'=>'Koreanisch'
,'nl_nl'=>'Niederländisch'
,'pl'=>'Polnisch'
,'pt_br'=>'Portugiesisch (Brasilien)'
,'ro'=>'Rumänisch'
,'ru'=>'Russisch'
,'sv'=>'Schwedisch'
,'th'=>'Thai'
,'tr'=>'Türkisch'
,'zh_cn'=>'Chinesisch (Taiwan)'
,'zh_tw'=>'Chinesisch (traditionell)'
}
,'en_us'=> new Map<String,String>{
'cs'=>'Czech'
,'da'=>'Danish'
,'de'=>'German'
,'en_us'=>'English (United States)'
,'es'=>'Spanish'
,'es_mx'=>'Mexican Spanish'
,'fi'=>'Finnish'
,'fr'=>'French'
,'hu'=>'Hungarian'
,'in'=>'Indonesian'
,'it'=>'Italian'
,'ja'=>'Japanese'
,'ko'=>'Korean'
,'nl_nl'=>'Dutch'
,'pl'=>'Polish'
,'pt_br'=>'Portuguese (Brazilian)'
,'ro'=>'Romanian'
,'ru'=>'Russian'
,'sv'=>'Swedish'
,'th'=>'Thai'
,'tr'=>'Turkish'
,'zh_cn'=>'Chinese (Simplified)'
,'zh_tw'=>'Chinese (Traditional)'
}
,'es'=> new Map<String,String>{
'cs'=>'Checa'
,'da'=>'Danés'
,'de'=>'Alemán'
,'en_us'=>'Inglés (Estados Unidos)'
,'es'=>'Español'
,'es_mx'=>'El español de México'
,'fi'=>'Finlandés'
,'fr'=>'Francés'
,'hu'=>'Húngaro'
,'in'=>'Indonesia'
,'it'=>'Italiano'
,'ja'=>'Japonés'
,'ko'=>'Corea'
,'nl_nl'=>'Neerlandés'
,'pl'=>'Polaco'
,'pt_br'=>'Portugués (brasileño)'
,'ro'=>'Rumano'
,'ru'=>'Rusia'
,'sv'=>'Sueco'
,'th'=>'Tailandia'
,'tr'=>'Turquía'
,'zh_cn'=>'Chino (simplificado)'
,'zh_tw'=>'Chino (tradicional)'
}
,'es_mx'=> new Map<String,String>{
'cs'=>'Checa'
,'da'=>'Danés'
,'de'=>'Alemán'
,'en_us'=>'Inglés (Estados Unidos)'
,'es'=>'Español'
,'es_mx'=>'El español de México'
,'fi'=>'Finlandés'
,'fr'=>'Francés'
,'hu'=>'Húngaro'
,'in'=>'Indonesia'
,'it'=>'Italiano'
,'ja'=>'Japonés'
,'ko'=>'Corea'
,'nl_nl'=>'Neerlandés'
,'pl'=>'Polaco'
,'pt_br'=>'Portugués (brasileño)'
,'ro'=>'Rumano'
,'ru'=>'Rusia'
,'sv'=>'Sueco'
,'th'=>'Tailandia'
,'tr'=>'Turquía'
,'zh_cn'=>'Chino (simplificado)'
,'zh_tw'=>'Chino (tradicional)'
}
,'fi'=> new Map<String,String>{
'cs'=>'Tšekki'
,'da'=>'Tanska'
,'de'=>'Saksa'
,'en_us'=>'Englanti (Yhdysvallat)'
,'es'=>'Espanja'
,'es_mx'=>'Meksikon espanja'
,'fi'=>'Suomen'
,'fr'=>'Ranska'
,'hu'=>'Unkari'
,'in'=>'Indonesia'
,'it'=>'Italia'
,'ja'=>'Japani'
,'ko'=>'Korea'
,'nl_nl'=>'Hollanti'
,'pl'=>'Puola'
,'pt_br'=>'Portugali (Brasilia)'
,'ro'=>'Romania'
,'ru'=>'Venäjä'
,'sv'=>'Ruotsi'
,'th'=>'Thaimaalaisen'
,'tr'=>'Turkki'
,'zh_cn'=>'Kiina (yksinkertaistettu)'
,'zh_tw'=>'Kiina (perinteinen)'
}
,'fr'=> new Map<String,String>{
'cs'=>'Tchèque'
,'da'=>'Danois'
,'de'=>'Allemand'
,'en_us'=>'Anglais (Etats Unis)'
,'es'=>'Espagnol'
,'es_mx'=>'Espagnol mexicain'
,'fi'=>'Finnois'
,'fr'=>'Français'
,'hu'=>'Hongrois'
,'in'=>'Indonésien'
,'it'=>'Italien'
,'ja'=>'Japonais'
,'ko'=>'Coréen'
,'nl_nl'=>'Néerlandais'
,'pl'=>'Polonais'
,'pt_br'=>'Portugais (brésilien)'
,'ro'=>'Roumain'
,'ru'=>'Russe'
,'sv'=>'Suédois'
,'th'=>'Thai'
,'tr'=>'Turc'
,'zh_cn'=>'Chinois (simplifié)'
,'zh_tw'=>'Chinois (Traditionnel)'
}
,'hu'=> new Map<String,String>{
'cs'=>'Cseh'
,'da'=>'Dán'
,'de'=>'Német'
,'en_us'=>'Angol (Egyesült Államok)'
,'es'=>'Spanyol'
,'es_mx'=>'Mexikói spanyol'
,'fi'=>'Finn'
,'fr'=>'Francia'
,'hu'=>'Magyar'
,'in'=>'Indonéz'
,'it'=>'Olasz'
,'ja'=>'Japán'
,'ko'=>'Koreai'
,'nl_nl'=>'Holland'
,'pl'=>'Lengyel'
,'pt_br'=>'Portugál (brazíliai)'
,'ro'=>'Román'
,'ru'=>'Orosz'
,'sv'=>'Svéd'
,'th'=>'Thaiföldi'
,'tr'=>'Török'
,'zh_cn'=>'Kínai (egyszerűsített)'
,'zh_tw'=>'Kínai (hagyományos)'
}
,'in'=> new Map<String,String>{
'cs'=>'Ceko'
,'da'=>'Denmark'
,'de'=>'Jerman'
,'en_us'=>'Inggris (Amerika Serikat)'
,'es'=>'Spanyol'
,'es_mx'=>'Meksiko Spanyol'
,'fi'=>'Finlandia'
,'fr'=>'Prancis'
,'hu'=>'Hungaria'
,'in'=>'Indonesia'
,'it'=>'Italia'
,'ja'=>'Jepang'
,'ko'=>'Korea'
,'nl_nl'=>'Belanda'
,'pl'=>'Polish'
,'pt_br'=>'Portugis (Brasil)'
,'ro'=>'Romanian'
,'ru'=>'Russian'
,'sv'=>'Swedia'
,'th'=>'Thai'
,'tr'=>'Turkish'
,'zh_cn'=>'Cina (Sederhana)'
,'zh_tw'=>'Cina (Tradisional)'
}
,'it'=> new Map<String,String>{
'cs'=>'Ceco'
,'da'=>'Danese'
,'de'=>'Tedesco'
,'en_us'=>'Inglese (Stati Uniti)'
,'es'=>'Spagnolo'
,'es_mx'=>'Spagnolo messicano'
,'fi'=>'Finlandese'
,'fr'=>'Francese'
,'hu'=>'Ungherese'
,'in'=>'Indonesiano'
,'it'=>'Italiano'
,'ja'=>'Giapponese'
,'ko'=>'Coreano'
,'nl_nl'=>'Olandese'
,'pl'=>'Polacco'
,'pt_br'=>'Portoghese (brasiliano)'
,'ro'=>'Rumeno'
,'ru'=>'Russo'
,'sv'=>'Svedese'
,'th'=>'Thai'
,'tr'=>'Turco'
,'zh_cn'=>'Cinese (semplificato)'
,'zh_tw'=>'Cinese (tradizionale)'
}
,'ja'=> new Map<String,String>{
'cs'=>'チェコ語'
,'da'=>'デンマーク語'
,'de'=>'ドイツ語'
,'en_us'=>'英語（アメリカ合衆国）'
,'es'=>'スペイン語'
,'es_mx'=>'メキシコのスペイン語'
,'fi'=>'フィンランド語'
,'fr'=>'フランス語'
,'hu'=>'ハンガリー語'
,'in'=>'インドネシア語'
,'it'=>'イタリア語'
,'ja'=>'日本語'
,'ko'=>'韓国語'
,'nl_nl'=>'オランダ語'
,'pl'=>'ポーランド語'
,'pt_br'=>'ポルトガル語（ブラジル）'
,'ro'=>'ルーマニア語'
,'ru'=>'ロシア語'
,'sv'=>'スウェーデン語'
,'th'=>'タイ'
,'tr'=>'トルコ語'
,'zh_cn'=>'中国語（簡体字）'
,'zh_tw'=>'中国語（繁体字）'
}
,'ko'=> new Map<String,String>{
'cs'=>'체코어'
,'da'=>'덴마크어'
,'de'=>'독일어'
,'en_us'=>'영어 (미국)'
,'es'=>'스페인어'
,'es_mx'=>'멕시코 스페인'
,'fi'=>'핀란드어'
,'fr'=>'프랑스어'
,'hu'=>'헝가리어'
,'in'=>'인도네시 아어'
,'it'=>'이탈리아어'
,'ja'=>'일본어'
,'ko'=>'한국어'
,'nl_nl'=>'네덜란드'
,'pl'=>'폴란드어'
,'pt_br'=>'포르투갈어 (브라질)'
,'ro'=>'루마니아어'
,'ru'=>'러시아어'
,'sv'=>'스웨덴어'
,'th'=>'타이어'
,'tr'=>'터키어'
,'zh_cn'=>'중국어 (간체)'
,'zh_tw'=>'중국어 (번체)'
}
,'nl_nl'=> new Map<String,String>{
'cs'=>'Tsjechisch'
,'da'=>'Deens'
,'de'=>'Duits'
,'en_us'=>'Engels (Verenigde Staten)'
,'es'=>'Spaans'
,'es_mx'=>'Mexicaans Spaans'
,'fi'=>'Fins'
,'fr'=>'Frans'
,'hu'=>'Hongaars'
,'in'=>'Indonesisch'
,'it'=>'Italiaans'
,'ja'=>'Japans'
,'ko'=>'Koreaans'
,'nl_nl'=>'Nederlandse'
,'pl'=>'Pools'
,'pt_br'=>'Portugees (Braziliaans)'
,'ro'=>'Roemeens'
,'ru'=>'Russisch'
,'sv'=>'Zweeds'
,'th'=>'Thais'
,'tr'=>'Turks'
,'zh_cn'=>'Chinese (Simplified)'
,'zh_tw'=>'Chinees (traditioneel)'
}
,'pl'=> new Map<String,String>{
'cs'=>'Czeski'
,'da'=>'Duński'
,'de'=>'Niemiecki'
,'en_us'=>'Angielski (Stany Zjednoczone)'
,'es'=>'Hiszpański'
,'es_mx'=>'Mexican hiszpański'
,'fi'=>'Fiński'
,'fr'=>'Francuski'
,'hu'=>'Węgierski'
,'in'=>'Indonezyjski'
,'it'=>'Włoski'
,'ja'=>'Japoński'
,'ko'=>'Koreański'
,'nl_nl'=>'Niderlandzki'
,'pl'=>'Polska'
,'pt_br'=>'Portugalski (Brazylia)'
,'ro'=>'Rumuński'
,'ru'=>'Rosyjski'
,'sv'=>'Szwedzki'
,'th'=>'Taj'
,'tr'=>'Turecki'
,'zh_cn'=>'Chiński (uproszczony)'
,'zh_tw'=>'Chiński (tradycyjny)'
}
,'pt_br'=> new Map<String,String>{
'cs'=>'Tcheco'
,'da'=>'Dinamarquês'
,'de'=>'Alemão'
,'en_us'=>'Inglês (Estados Unidos)'
,'es'=>'Espanhol'
,'es_mx'=>'Espanhol mexicano'
,'fi'=>'Finlandês'
,'fr'=>'Francês'
,'hu'=>'Húngaro'
,'in'=>'Indonésio'
,'it'=>'Italiano'
,'ja'=>'Japonês'
,'ko'=>'Coreano'
,'nl_nl'=>'Holandês'
,'pl'=>'Polonês'
,'pt_br'=>'Português (Brasil)'
,'ro'=>'Romeno'
,'ru'=>'Russo'
,'sv'=>'Sueco'
,'th'=>'Tailandês'
,'tr'=>'Turco'
,'zh_cn'=>'Chinês (simplificado)'
,'zh_tw'=>'Chinês (Tradicional)'
}
,'ro'=> new Map<String,String>{
'cs'=>'Cehă'
,'da'=>'Daneză'
,'de'=>'Germană'
,'en_us'=>'În limba engleză (Statele Unite)'
,'es'=>'Spaniolă'
,'es_mx'=>'Mexicane Spanish'
,'fi'=>'Finlandeză'
,'fr'=>'Franţuzesc'
,'hu'=>'Maghiară'
,'in'=>'Indoneziană'
,'it'=>'Italiană'
,'ja'=>'Japoneză'
,'ko'=>'Coreeană'
,'nl_nl'=>'Olandeză'
,'pl'=>'Poloneză'
,'pt_br'=>'Portuguese (Brazilian)'
,'ro'=>'Român'
,'ru'=>'Rus'
,'sv'=>'Suedez'
,'th'=>'Thai'
,'tr'=>'Turcă'
,'zh_cn'=>'Chineză (simplificată)'
,'zh_tw'=>'Chineză (Tradiţională)'
}
,'ru'=> new Map<String,String>{
'cs'=>'Чешский'
,'da'=>'Датский'
,'de'=>'Немецкий'
,'en_us'=>'Английский (США)'
,'es'=>'Испанский'
,'es_mx'=>'Мексиканские Испанский'
,'fi'=>'Финский'
,'fr'=>'Французский'
,'hu'=>'Венгерский'
,'in'=>'Индонезийский'
,'it'=>'Итальянский'
,'ja'=>'Японский'
,'ko'=>'Корейский'
,'nl_nl'=>'Голландский'
,'pl'=>'Польский'
,'pt_br'=>'Португальский (бразильский)'
,'ro'=>'Румынский'
,'ru'=>'Русский'
,'sv'=>'Шведский'
,'th'=>'Тайский'
,'tr'=>'Турецкий'
,'zh_cn'=>'Китайский (упрощенный)'
,'zh_tw'=>'Китайский (традиционный)'
}
,'sv'=> new Map<String,String>{
'cs'=>'Tjeckiska'
,'da'=>'Danska'
,'de'=>'Tyska'
,'en_us'=>'Engelska (USA)'
,'es'=>'Spanska'
,'es_mx'=>'Mexikansk spanska'
,'fi'=>'Finska'
,'fr'=>'Franska'
,'hu'=>'Ungerska'
,'in'=>'Indonesiska'
,'it'=>'Italienska'
,'ja'=>'Japanska'
,'ko'=>'Koreanska'
,'nl_nl'=>'Nederländska'
,'pl'=>'Polska'
,'pt_br'=>'Portugisiska (Brasilien)'
,'ro'=>'Rumänska'
,'ru'=>'Ryska'
,'sv'=>'Svenska'
,'th'=>'Thai'
,'tr'=>'Turkiska'
,'zh_cn'=>'Kinesiska (förenklad)'
,'zh_tw'=>'Kinesiska (traditionell)'
}
,'th'=> new Map<String,String>{
'cs'=>'สาธารณรัฐ เช็ ก'
,'da'=>'เดนมาร์ก'
,'de'=>'เยอรมัน'
,'en_us'=>'ภาษา อังกฤษ States (United)'
,'es'=>'สเปน'
,'es_mx'=>'สเปน เม็ก ซิ กัน'
,'fi'=>'ฟินแลนด์'
,'fr'=>'ฝรั่งเศส'
,'hu'=>'ฮังการี'
,'in'=>'อินโดนีเซีย'
,'it'=>'อิตาเลียน'
,'ja'=>'ญี่ปุ่น'
,'ko'=>'เกาหลี'
,'nl_nl'=>'ดัตช์'
,'pl'=>'เงา'
,'pt_br'=>'โปรตุเกส (บราซิล)'
,'ro'=>'โรมาเนีย'
,'ru'=>'ภาษา รัสเซีย'
,'sv'=>'สวีเดน'
,'th'=>'ไทย'
,'tr'=>'ภาษา ตุรกี'
,'zh_cn'=>'จีน (ประยุกต์)'
,'zh_tw'=>'ภาษา จีน (ดั้งเดิม)'
}
,'tr'=> new Map<String,String>{
'cs'=>'Çekçe'
,'da'=>'Danca'
,'de'=>'Almanca'
,'en_us'=>'İngilizce (ABD)'
,'es'=>'İspanyolca'
,'es_mx'=>'Mexican İspanyolca'
,'fi'=>'Fince'
,'fr'=>'Fransızca'
,'hu'=>'Macarca'
,'in'=>'Endonezya Dili'
,'it'=>'İtalyanca'
,'ja'=>'Japonca'
,'ko'=>'Korece'
,'nl_nl'=>'Hollanda Dili'
,'pl'=>'Lehçe'
,'pt_br'=>'Portekizce (Brezilya)'
,'ro'=>'Romence'
,'ru'=>'Rusça'
,'sv'=>'İsveççe'
,'th'=>'Tay'
,'tr'=>'Türkçe'
,'zh_cn'=>'Çince (Basitleştirilmiş)'
,'zh_tw'=>'Çince (Geleneksel)'
}
,'zh_cn'=> new Map<String,String>{
'cs'=>'捷克文'
,'da'=>'丹麦文'
,'de'=>'德语'
,'en_us'=>'英语（美国）'
,'es'=>'西班牙语'
,'es_mx'=>'墨西哥西班牙语'
,'fi'=>'芬兰文'
,'fr'=>'法语'
,'hu'=>'匈牙利文'
,'in'=>'印度尼西亚文'
,'it'=>'意大利语'
,'ja'=>'日语'
,'ko'=>'韩文'
,'nl_nl'=>'荷兰文'
,'pl'=>'波兰文'
,'pt_br'=>'葡萄牙语（巴西）'
,'ro'=>'罗马尼亚文'
,'ru'=>'俄文'
,'sv'=>'瑞典文'
,'th'=>'泰国'
,'tr'=>'土耳其文'
,'zh_cn'=>'中文（简体）'
,'zh_tw'=>'中文（繁体）'
}
,'zh_tw'=> new Map<String,String>{
'cs'=>'捷克文'
,'da'=>'丹麥文'
,'de'=>'德語'
,'en_us'=>'英語（美國）'
,'es'=>'西班牙語'
,'es_mx'=>'墨西哥西班牙語'
,'fi'=>'芬蘭文'
,'fr'=>'法語'
,'hu'=>'匈牙利文'
,'in'=>'印度尼西亞文'
,'it'=>'意大利語'
,'ja'=>'日語'
,'ko'=>'韓文'
,'nl_nl'=>'荷蘭文'
,'pl'=>'波蘭文'
,'pt_br'=>'葡萄牙語（巴西）'
,'ro'=>'羅馬尼亞文'
,'ru'=>'俄文'
,'sv'=>'瑞典文'
,'th'=>'泰國'
,'tr'=>'土耳其文'
,'zh_cn'=>'中文（簡體）'
,'zh_tw'=>'中文（繁體）'
}

	};    
    
}