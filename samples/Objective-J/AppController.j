//
// AppController.j
// FlickrPhoto
//
// Created by Ross Boucher.
// Copyright 2008 - 2010, 280 North, Inc. All rights reserved.

@import <Foundation/Foundation.j>
@import <AppKit/AppKit.j>

var SliderToolbarItemIdentifier = "SliderToolbarItemIdentifier",
    AddToolbarItemIdentifier = "AddToolbarItemIdentifier",
    RemoveToolbarItemIdentifier = "RemoveToolbarItemIdentifier";

/*
    Important note about CPJSONPConnection: CPJSONPConnection is ONLY for JSONP APIs.
    If aren't sure you NEED JSONP (see http://ajaxian.com/archives/jsonp-json-with-padding ),
    you most likely don't want to use CPJSONPConnection, but rather the more standard
    CPURLConnection. CPJSONPConnection is designed for cross-domain
    connections, and if you are making requests to the same domain (as most web
    applications do), you do not need it.
*/

@implementation AppController : CPObject
{
    CPString                lastIdentifier;
    CPDictionary            photosets;

    CPCollectionView        listCollectionView;
    CPCollectionView        photosCollectionView;
}

- (void)applicationDidFinishLaunching:(CPNotification)aNotification
{
    //the first thing we need to do is create a window to take up the full screen
    //we'll also create a toolbar to go with it, and grab its size for future reference

    var theWindow = [[CPWindow alloc] initWithContentRect:CGRectMakeZero() styleMask:CPBorderlessBridgeWindowMask],
        contentView = [theWindow contentView],
        toolbar = [[CPToolbar alloc] initWithIdentifier:"Photos"],
        bounds = [contentView bounds];

    //we tell the toolbar that we want to be its delegate and attach it to theWindow
    [toolbar setDelegate:self];
    [toolbar setVisible:true];
    [theWindow setToolbar:toolbar];

    photosets = [CPDictionary dictionary]; //storage for our sets of photos return from Flickr

    //now we create a scroll view to contain the list of collections of photos (photosets)
    //inside the scroll view, we'll place our collection view, which manages a collection of "cells"
    //each cell will represent one photo collection, and choosing cells will select that collection

    var listScrollView = [[CPScrollView alloc] initWithFrame:CGRectMake(0, 0, 200, CGRectGetHeight(bounds) - 58)];
    [listScrollView setAutohidesScrollers:YES];
    [listScrollView setAutoresizingMask:CPViewHeightSizable];
    [[listScrollView contentView] setBackgroundColor:[CPColor colorWithRed:213.0 / 255.0 green:221.0 / 255.0 blue:230.0 / 255.0 alpha:1.0]];

    //we create the collection view cells by creating a single prototype (CPCollectionViewItem) and setting its view.
    //the CPCollectionView class will then duplicate this item as many times as it needs

    var photosListItem = [[CPCollectionViewItem alloc] init];
    [photosListItem setView:[[PhotosListCell alloc] initWithFrame:CGRectMakeZero()]];

    listCollectionView = [[CPCollectionView alloc] initWithFrame:CGRectMake(0, 0, 200, 0)];

    [listCollectionView setDelegate:self]; //we want delegate methods
    [listCollectionView setItemPrototype:photosListItem]; //set the item prototype

    [listCollectionView setMinItemSize:CGSizeMake(20.0, 45.0)];
    [listCollectionView setMaxItemSize:CGSizeMake(1000.0, 45.0)];
    [listCollectionView setMaxNumberOfColumns:1]; //setting a single column will make this appear as a vertical list

    [listCollectionView setVerticalMargin:0.0];
    [listCollectionView setAutoresizingMask:CPViewWidthSizable];

    //finally, we put our collection view inside the scroll view as it's document view, so it can be scrolled
    [listScrollView setDocumentView:listCollectionView];

    //and we add it to the window's content view, so it will show up on the screen
    [contentView addSubview:listScrollView];

    //repeat the process with another collection view for the actual photos
    //this time we'll use a different view for the prototype (PhotoCell)

    var photoItem = [[CPCollectionViewItem alloc] init];
    [photoItem setView:[[PhotoCell alloc] initWithFrame:CGRectMake(0, 0, 150, 150)]];

    var scrollView = [[CPScrollView alloc] initWithFrame:CGRectMake(200, 0, CGRectGetWidth(bounds) - 200, CGRectGetHeight(bounds) - 58)];

    photosCollectionView = [[CPCollectionView alloc] initWithFrame:CGRectMake(0, 0, CGRectGetWidth(bounds) - 200, 0)];

    [photosCollectionView setDelegate:self];
    [photosCollectionView setItemPrototype:photoItem];

    [photosCollectionView setMinItemSize:CGSizeMake(150, 150)];
    [photosCollectionView setMaxItemSize:CGSizeMake(150, 150)];
    [photosCollectionView setAutoresizingMask:CPViewWidthSizable];

    [scrollView setAutoresizingMask:CPViewHeightSizable | CPViewWidthSizable];
    [scrollView setDocumentView:photosCollectionView];
    [scrollView setAutohidesScrollers:YES];

    [[scrollView contentView] setBackgroundColor:[CPColor colorWithCalibratedWhite:0.25 alpha:1.0]];

    [contentView addSubview:scrollView];

    //bring forward the window to display it
    [theWindow orderFront:self];

    //get the most interesting photos on flickr
    var request = [CPURLRequest requestWithURL:"http://www.flickr.com/services/rest/?method=flickr.interestingness.getList&per_page=20&format=json&api_key=ca4dd89d3dfaeaf075144c3fdec76756"];

    // see important note about CPJSONPConnection above
    var connection = [CPJSONPConnection sendRequest:request callback:"jsoncallback" delegate:self];

    lastIdentifier = "Interesting Photos";
}

- (void)add:(id)sender
{
    var string = prompt("Enter a tag to search Flickr for photos.");

    if (string)
    {
        //create a new request for the photos with the tag returned from the javascript prompt
        var request = [CPURLRequest requestWithURL:"http://www.flickr.com/services/rest/?"+
                                                    "method=flickr.photos.search&tags="+encodeURIComponent(string)+
                                                    "&media=photos&machine_tag_mode=any&per_page=20&format=json&api_key=ca4dd89d3dfaeaf075144c3fdec76756"];

        // see important note about CPJSONPConnection above
        [CPJSONPConnection sendRequest:request callback:"jsoncallback" delegate:self];

        lastIdentifier = string;
    }
}

- (void)remove:(id)sender
{
    //remove this photo
    [self removeImageListWithIdentifier:[[photosets allKeys] objectAtIndex:[[listCollectionView selectionIndexes] firstIndex]]];
}

- (void)addImageList:(CPArray)images withIdentifier:(CPString)aString
{
    [photosets setObject:images forKey:aString];

    [listCollectionView setContent:[[photosets allKeys] copy]];
    [listCollectionView setSelectionIndexes:[CPIndexSet indexSetWithIndex:[[photosets allKeys] indexOfObject:aString]]];
}

- (void)removeImageListWithIdentifier:(CPString)aString
{
    var nextIndex = MAX([[listCollectionView content] indexOfObject:aString] - 1, 0);

    [photosets removeObjectForKey:aString];

    [listCollectionView setContent:[[photosets allKeys] copy]];
    [listCollectionView setSelectionIndexes:[CPIndexSet indexSetWithIndex:nextIndex]];
}

- (void)adjustImageSize:(id)sender
{
    var newSize = [sender value];

    [photosCollectionView setMinItemSize:CGSizeMake(newSize, newSize)];
    [photosCollectionView setMaxItemSize:CGSizeMake(newSize, newSize)];
}

- (void)collectionViewDidChangeSelection:(CPCollectionView)aCollectionView
{
    if (aCollectionView == listCollectionView)
    {
        var listIndex = [[listCollectionView selectionIndexes] firstIndex];

        if (listIndex === CPNotFound)
            return;

        var key = [listCollectionView content][listIndex];

        [photosCollectionView setContent:[photosets objectForKey:key]];
        [photosCollectionView setSelectionIndexes:[CPIndexSet indexSet]];
    }
}

- (void)connection:(CPJSONPConnection)aConnection didReceiveData:(CPString)data
{
    //this method is called when the network request returns. the data is the returned
    //information from flickr. we set the array of photo urls as the data to our collection view

    [self addImageList:data.photos.photo withIdentifier:lastIdentifier];
}

- (void)connection:(CPJSONPConnection)aConnection didFailWithError:(CPString)error
{
    alert(error); //a network error occurred
}

//these two methods are the toolbar delegate methods, and tell the toolbar what it should display to the user

- (CPArray)toolbarAllowedItemIdentifiers:(CPToolbar)aToolbar
{
   return [self toolbarDefaultItemIdentifiers:aToolbar];
}

- (CPArray)toolbarDefaultItemIdentifiers:(CPToolbar)aToolbar
{
   return [AddToolbarItemIdentifier, RemoveToolbarItemIdentifier, CPToolbarFlexibleSpaceItemIdentifier, SliderToolbarItemIdentifier];
}

//this delegate method returns the actual toolbar item for the given identifier

- (CPToolbarItem)toolbar:(CPToolbar)aToolbar itemForItemIdentifier:(CPString)anItemIdentifier willBeInsertedIntoToolbar:(BOOL)aFlag
{
    var toolbarItem = [[CPToolbarItem alloc] initWithItemIdentifier:anItemIdentifier];

    if (anItemIdentifier == SliderToolbarItemIdentifier)
    {
        [toolbarItem setView:[[PhotoResizeView alloc] initWithFrame:CGRectMake(0, 0, 180, 32)]];
        [toolbarItem setMinSize:CGSizeMake(180, 32)];
        [toolbarItem setMaxSize:CGSizeMake(180, 32)];
        [toolbarItem setLabel:"Scale"];
    }
    else if (anItemIdentifier == AddToolbarItemIdentifier)
    {
        var image = [[CPImage alloc] initWithContentsOfFile:[[CPBundle mainBundle] pathForResource:"add.png"] size:CPSizeMake(30, 25)],
            highlighted = [[CPImage alloc] initWithContentsOfFile:[[CPBundle mainBundle] pathForResource:"addHighlighted.png"] size:CPSizeMake(30, 25)];

        [toolbarItem setImage:image];
        [toolbarItem setAlternateImage:highlighted];

        [toolbarItem setTarget:self];
        [toolbarItem setAction:@selector(add:)];
        [toolbarItem setLabel:"Add Photo List"];

        [toolbarItem setMinSize:CGSizeMake(32, 32)];
        [toolbarItem setMaxSize:CGSizeMake(32, 32)];
    }
    else if (anItemIdentifier == RemoveToolbarItemIdentifier)
    {
        var image = [[CPImage alloc] initWithContentsOfFile:[[CPBundle mainBundle] pathForResource:"remove.png"] size:CPSizeMake(30, 25)],
            highlighted = [[CPImage alloc] initWithContentsOfFile:[[CPBundle mainBundle] pathForResource:"removeHighlighted.png"] size:CPSizeMake(30, 25)];

        [toolbarItem setImage:image];
        [toolbarItem setAlternateImage:highlighted];

        [toolbarItem setTarget:self];
        [toolbarItem setAction:@selector(remove:)];
        [toolbarItem setLabel:"Remove Photo List"];

        [toolbarItem setMinSize:CGSizeMake(32, 32)];
        [toolbarItem setMaxSize:CGSizeMake(32, 32)];
    }

    return toolbarItem;
}

@end

/*
    This code demonstrates how to add a category to an existing class.
    In this case, we are adding the class method +flickr_labelWithText: to
    the CPTextField class. Later on, we can call [CPTextField flickr_labelWithText:"foo"]
    to return a new text field with the string foo.
    Best practices suggest prefixing category methods with your unique prefix, to prevent collisions.
*/

@implementation CPTextField (CreateLabel)

+ (CPTextField)flickr_labelWithText:(CPString)aString
{
    var label = [[CPTextField alloc] initWithFrame:CGRectMakeZero()];

    [label setStringValue:aString];
    [label sizeToFit];
    [label setTextShadowColor:[CPColor whiteColor]];
    [label setTextShadowOffset:CGSizeMake(0, 1)];

    return label;
}

@end

// This class wraps our slider + labels combo

@implementation PhotoResizeView : CPView
{
}

- (id)initWithFrame:(CGRect)aFrame
{
    self = [super initWithFrame:aFrame];

    var slider = [[CPSlider alloc] initWithFrame:CGRectMake(30, CGRectGetHeight(aFrame) / 2.0 - 8, CGRectGetWidth(aFrame) - 65, 24)];

    [slider setMinValue:50.0];
    [slider setMaxValue:250.0];
    [slider setIntValue:150.0];
    [slider setAction:@selector(adjustImageSize:)];

    [self addSubview:slider];

    var label = [CPTextField flickr_labelWithText:"50"];
    [label setFrameOrigin:CGPointMake(0, CGRectGetHeight(aFrame) / 2.0 - 4.0)];
    [self addSubview:label];

    label = [CPTextField flickr_labelWithText:"250"];
    [label setFrameOrigin:CGPointMake(CGRectGetWidth(aFrame) - CGRectGetWidth([label frame]), CGRectGetHeight(aFrame) / 2.0 - 4.0)];
    [self addSubview:label];

    return self;
}

@end

// This class displays a single photo collection inside our list of photo collecitions

@implementation PhotosListCell : CPView
{
    CPTextField     label;
    CPView          highlightView;
}

- (void)setRepresentedObject:(JSObject)anObject
{
    if (!label)
    {
        label = [[CPTextField alloc] initWithFrame:CGRectInset([self bounds], 4, 4)];

        [label setFont:[CPFont systemFontOfSize:16.0]];
        [label setTextShadowColor:[CPColor whiteColor]];
        [label setTextShadowOffset:CGSizeMake(0, 1)];

        [self addSubview:label];
    }

    [label setStringValue:anObject];
    [label sizeToFit];

    [label setFrameOrigin:CGPointMake(10,CGRectGetHeight([label bounds]) / 2.0)];
}

- (void)setSelected:(BOOL)flag
{
    if (!highlightView)
    {
        highlightView = [[CPView alloc] initWithFrame:CGRectCreateCopy([self bounds])];
        [highlightView setBackgroundColor:[CPColor blueColor]];
    }

    if (flag)
    {
        [self addSubview:highlightView positioned:CPWindowBelow relativeTo:label];
        [label setTextColor:[CPColor whiteColor]];
        [label setTextShadowColor:[CPColor blackColor]];
    }
    else
    {
        [highlightView removeFromSuperview];
        [label setTextColor:[CPColor blackColor]];
        [label setTextShadowColor:[CPColor whiteColor]];
    }
}

@end

// This class displays a single photo from our collection

@implementation PhotoCell : CPView
{
    CPImage         image;
    CPImageView     imageView;
    CPView          highlightView;
}

- (void)setRepresentedObject:(JSObject)anObject
{
    if (!imageView)
    {
        imageView = [[CPImageView alloc] initWithFrame:CGRectMakeCopy([self bounds])];
        [imageView setAutoresizingMask:CPViewWidthSizable | CPViewHeightSizable];
        [imageView setImageScaling:CPScaleProportionally];
        [imageView setHasShadow:YES];
        [self addSubview:imageView];
    }

    [image setDelegate:nil];

    image = [[CPImage alloc] initWithContentsOfFile:thumbForFlickrPhoto(anObject)];

    [image setDelegate:self];

    if ([image loadStatus] == CPImageLoadStatusCompleted)
        [imageView setImage:image];
    else
        [imageView setImage:nil];
}

- (void)imageDidLoad:(CPImage)anImage
{
    [imageView setImage:anImage];
}

- (void)setSelected:(BOOL)flag
{
    if (!highlightView)
    {
        highlightView = [[CPView alloc] initWithFrame:[self bounds]];
        [highlightView setBackgroundColor:[CPColor colorWithCalibratedWhite:0.8 alpha:0.6]];
        [highlightView setAutoresizingMask:CPViewWidthSizable | CPViewHeightSizable];
    }

    if (flag)
    {
        [highlightView setFrame:[self bounds]];
        [self addSubview:highlightView positioned:CPWindowBelow relativeTo:imageView];
    }
    else
        [highlightView removeFromSuperview];
}

@end

// helper javascript functions for turning a Flickr photo object into a URL for getting the image

function urlForFlickrPhoto(photo)
{
    return "http://farm" + photo.farm + ".static.flickr.com/" + photo.server + "/" + photo.id+"_" + photo.secret + ".jpg";
}

function thumbForFlickrPhoto(photo)
{
    return "http://farm" + photo.farm + ".static.flickr.com/" + photo.server + "/" + photo.id + "_" + photo.secret + "_m.jpg";
}