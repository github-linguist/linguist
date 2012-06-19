public class GeoUtils {
	// generate a KML string given a page reference, call getContent() 
	// then cleanup the output.
	public static string generateFromContent(PageReference pr) { 
		string ret = ''; 
		try {  
	        ret = (string) pr.getContent().toString();
			
			ret = ret.replaceAll('"','\'' ); // get content produces quote chars \"  
	        ret = ret.replaceAll( '&','&amp;');// we need to escape these in the node value
        } catch (exception e ) { 
        	system.debug( 'ERROR '+e); 
        }
   		
   		ret = ret.replaceAll('\n',' ');	// must use ALL since many new line may get 
        ret = ret.replaceAll('\r',' ');	// get these also!
      //  system.debug( ret); // dump the KML 
        return ret ;
	}
	
	public static Map<String, String> geo_response = new Map<String, String>{'200'=>'G_GEO_SUCCESS',
    '400'=>'G_GEO_BAD_REQUEST',
    '500'=>'G_GEO_SERVER_ERROR',
    '601'=>'G_GEO_MISSING_ADDRESS',
    '602'=>'G_GEO_UNKNOWN_ADDRESS',
    '603'=>'G_GEO_UNAVAILABLE_ADDRESS',
    '604'=>'G_GEO_UNKNOWN_DIRECTIONS',
    '610'=>'G_GEO_BAD_KEY',
    '620'=>'G_GEO_TOO_MANY_QUERIES'
    };
        
	public static string accountAddressString ( account acct ) {
    	// form an address string given an account object
    	string adr = acct.billingstreet + ',' + acct.billingcity + ',' + acct.billingstate; 
        if ( acct.billingpostalcode != null ) adr += ',' + acct.billingpostalcode; 
        if ( acct.billingcountry != null ) adr += ',' + acct.billingcountry; 
        adr = adr.replaceAll('\"', '' );
        adr = adr.replaceAll('\'', '' );
        adr = adr.replaceAll( '\n', ' ' );    
        adr = adr.replaceAll( '\r', ' ' );    
        system.debug( adr );  
        return adr;	
    }
    
	public static testmethod void t1() { 
		PageReference pageRef =  Page.kmlPreviewTemplate;
        Test.setCurrentPage(pageRef);
        system.assert ( GeoUtils.generateFromContent( pageRef ) != null );
        Account a =  new Account( name='foo', billingstreet='main', billingcity='springfield',billingstate='il',
         billingpostalcode='9',billingcountry='us');
        insert a;
        system.assertEquals( 'main,springfield,il,9,us',accountAddressString( a) );
       
	}
}