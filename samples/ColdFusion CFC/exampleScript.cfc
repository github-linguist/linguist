/**
********************************************************************************
ContentBox - A Modular Content Platform
Copyright 2012 by Luis Majano and Ortus Solutions, Corp
www.gocontentbox.org | www.luismajano.com | www.ortussolutions.com
********************************************************************************
Apache License, Version 2.0

Copyright Since [2012] [Luis Majano and Ortus Solutions,Corp]

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
********************************************************************************
* A generic content service for content objects
*/
component extends="coldbox.system.orm.hibernate.VirtualEntityService" singleton{

	// DI
	property name="settingService"			inject="id:settingService@cb";
	property name="cacheBox"				inject="cachebox";
	property name="log"						inject="logbox:logger:{this}";
	property name="customFieldService" 	 	inject="customFieldService@cb";
	property name="categoryService" 	 	inject="categoryService@cb";
	property name="commentService" 	 		inject="commentService@cb";
	property name="contentVersionService"	inject="contentVersionService@cb";
	property name="authorService"			inject="authorService@cb";
	property name="populator"				inject="wirebox:populator";
	property name="systemUtil"				inject="SystemUtil@cb";
	
	/*
	* Constructor
	* @entityName.hint The content entity name to bind this service to.
	*/
	ContentService function init(entityName="cbContent"){
		// init it
		super.init(entityName=arguments.entityName, useQueryCaching=true);

		// Test scope coloring in pygments
		this.colorTestVar = "Just for testing pygments!";
		cookie.colorTestVar = "";
		client.colorTestVar = ""
		session.colorTestVar = "";
		application.colorTestVar = "";

		return this;
	}

	/**
	* Clear all content caches
	* @async.hint Run it asynchronously or not, defaults to false
	*/
	function clearAllCaches(boolean async=false){
		var settings = settingService.getAllSettings(asStruct=true);
		// Get appropriate cache provider
		var cache = cacheBox.getCache( settings.cb_content_cacheName );
		cache.clearByKeySnippet(keySnippet="cb-content",async=arguments.async);
		return this;
	}

	/**
	* Clear all page wrapper caches
	* @async.hint Run it asynchronously or not, defaults to false
	*/
	function clearAllPageWrapperCaches(boolean async=false){
		var settings = settingService.getAllSettings(asStruct=true);
		// Get appropriate cache provider
		var cache = cacheBox.getCache( settings.cb_content_cacheName );
		cache.clearByKeySnippet(keySnippet="cb-content-pagewrapper",async=arguments.async);
		return this;
	}

	/**
	* Clear all page wrapper caches
	* @slug.hint The slug partial to clean on
	* @async.hint Run it asynchronously or not, defaults to false
	*/
	function clearPageWrapperCaches(required any slug, boolean async=false){
		var settings = settingService.getAllSettings(asStruct=true);
		// Get appropriate cache provider
		var cache = cacheBox.getCache( settings.cb_content_cacheName );
		cache.clearByKeySnippet(keySnippet="cb-content-pagewrapper-#arguments.slug#",async=arguments.async);
		return this;
	}

	/**
	* Clear a page wrapper cache
	* @slug.hint The slug to clean
	* @async.hint Run it asynchronously or not, defaults to false
	*/
	function clearPageWrapper(required any slug, boolean async=false){
		var settings = settingService.getAllSettings(asStruct=true);
		// Get appropriate cache provider
		var cache = cacheBox.getCache( settings.cb_content_cacheName );
		cache.clear("cb-content-pagewrapper-#arguments.slug#/");
		return this;
	}

	/**
	* Searches published content with cool paramters, remember published content only
	* @searchTerm.hint The search term to search
	* @max.hint The maximum number of records to paginate
	* @offset.hint The offset in the pagination
	* @asQuery.hint Return as query or array of objects, defaults to array of objects
	* @sortOrder.hint The sorting of the search results, defaults to publishedDate DESC
	* @isPublished.hint Search for published, non-published or both content objects [true, false, 'all']
	* @searchActiveContent.hint Search only content titles or both title and active content. Defaults to both.
	*/
	function searchContent(
		any searchTerm="", 
		numeric max=0, 
		numeric offset=0, 
		boolean asQuery=false, 
		any sortOrder="publishedDate DESC", 
		any isPublished=true, 
		boolean searchActiveContent=true){

		var results = {};
		var c = newCriteria();

		// only published content
		if( isBoolean( arguments.isPublished ) ){
			// Published bit
			c.isEq( "isPublished", javaCast( "Boolean", arguments.isPublished ) );
			// Published eq true evaluate other params
			if( arguments.isPublished ){
				c.isLt("publishedDate", now() )
				.$or( c.restrictions.isNull("expireDate"), c.restrictions.isGT("expireDate", now() ) )
				.isEq("passwordProtection","");
			}
		}

		// Search Criteria
		if( len( arguments.searchTerm ) ){
			// like disjunctions
			c.createAlias("activeContent","ac");
			// Do we search title and active content or just title?
			if( arguments.searchActiveContent ){
				c.$or( c.restrictions.like("title","%#arguments.searchTerm#%"),
				  	  c.restrictions.like("ac.content", "%#arguments.searchTerm#%") );
			}
			else{
				c.like( "title", "%#arguments.searchTerm#%" ); 
			}
		}

		// run criteria query and projections count
		results.count = c.count( "contentID" );
		results.content = c.resultTransformer( c.DISTINCT_ROOT_ENTITY )
							.list(offset=arguments.offset, max=arguments.max, sortOrder=arguments.sortOrder, asQuery=arguments.asQuery);
	
		return results;
	}

/********************************************* PRIVATE *********************************************/
	

	/**
	* Update the content hits
	* @contentID.hint The content id to update
	*/
	private function syncUpdateHits(required contentID){
		var q = new Query(sql="UPDATE cb_content SET hits = hits + 1 WHERE contentID = #arguments.contentID#").execute();
		return this;
	}
	
	
	private function closureTest(){
		methodCall(
			param1,
			function( arg1, required arg2 ){
				var settings = settingService.getAllSettings(asStruct=true);
				// Get appropriate cache provider
				var cache = cacheBox.getCache( settings.cb_content_cacheName );
				cache.clear("cb-content-pagewrapper-#arguments.slug#/");
				return this;
			},
			param1
		);
	}
	
	private function StructliteralTest(){
		return {
			foo = bar,
			brad = 'Wood',
			func = function( arg1, required arg2 ){
				var settings = settingService.getAllSettings(asStruct=true);
				// Get appropriate cache provider
				var cache = cacheBox.getCache( settings.cb_content_cacheName );
				cache.clear("cb-content-pagewrapper-#arguments.slug#/");
				return this;
			},
			array = [
				1,
				2,
				3,
				4,
				5,
				'test',
				'testing',
				'testerton',
				{
					foo = true,
					brad = false,
					wood = null
				}
			],
			last = "final"
		};
	}
	
	private function arrayliteralTest(){
		return [
			1,
			2,
			3,
			4,
			5,
			'test',
			'testing',
			'testerton',
			{
				foo = true,
				brad = false,
				wood = null
			},
			'testy-von-testavich'
		];
	}
	
}