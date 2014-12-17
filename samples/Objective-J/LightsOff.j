@import <Foundation/CPObject.j>
@import <AppKit/CPView.j>
@import <AppKit/CPButton.j>
@import <AppKit/CPWebView.j>
@import "LOBoard.j"

@implementation LOInfoView : CPView
{
}

- (void)drawRect:(CGRect)r
{
    [[CPColor whiteColor] setFill]
    var path = [CPBezierPath bezierPath];
    [path appendBezierPathWithRoundedRect:CGRectMake(5, 0, CGRectGetWidth([self bounds]) - 10.0, CGRectGetHeight([self bounds])) xRadius:10 yRadius:10];
    [path fill];
}

@end

@implementation AppController : CPObject
{
}

- (CPPanel)initInfoWindow
{
    var infoWindow = [[CPPanel alloc] initWithContentRect:CGRectMake(400, 50, 320, 480) styleMask:CPHUDBackgroundWindowMask | CPResizableWindowMask];
    [infoWindow setFloatingPanel:YES];

    var _infoContent = [infoWindow contentView],
        _iconImage = [[CPImage alloc] initWithContentsOfFile:"Resources/icon.png" size:CPSizeMake(59, 60)],
        _iconView = [[CPImageView alloc] initWithFrame:CGRectMake(125, 0, 59, 60)];

    [_iconView setImage:_iconImage];
    [_infoContent addSubview:_iconView];

    var _infoView = [[LOInfoView alloc] initWithFrame:CGRectMake(0, 65, 320, 395)],
        _webView = [[CPWebView alloc] initWithFrame:CGRectMake(20, 0, 270, 370)];

    [_webView loadHTMLString:@"<center><h3>Lights Off</h3></center> <p>Lights Off is a fantastic game exclusively for iPhone and iPod touch and inspired by Tiger Electronic's 'Lights Out'.</p> <p>The goal of the game is simply to switch all of the lights off, but it's harder than it looks! Give the first few levels a try in the playable demo to the left.</p><center><img src='Resources/avail_on_app_store.png'></center>"];

    [_infoView addSubview:_webView];

    [_infoContent addSubview:_infoView];

    return infoWindow;
}

- (void)applicationDidFinishLaunching:(CPNotification)aNotification
{
    /* Enable Logging (DEBUG) */
    // CPLogRegister(CPLogPopup);

    var rootWindow = [[CPWindow alloc] initWithContentRect:CGRectMakeZero() styleMask:CPBorderlessBridgeWindowMask];
    [rootWindow setBackgroundColor:[CPColor grayColor]];
    [rootWindow orderFront:self];

    var infoWindow = [self initInfoWindow],
        gameWindow = [[CPPanel alloc] initWithContentRect:CGRectMake(50, 50, 324, 482) styleMask:CPHUDBackgroundWindowMask];
    [gameWindow setFloatingPanel:YES];
    [gameWindow setTitle:@"Lights Off"];

    contentView = [gameWindow contentView];

    var _board = [[LOBoard alloc] initWithFrame:CGRectMake(2, 0, 320, 480)],
        _bgImage = [[CPImage alloc] initWithContentsOfFile:"Resources/lo-background.png" size:CPSizeMake(320, 480)];
    [_board setImage:_bgImage];
    [_board resetBoard];

    var _buttonImage = [[CPImage alloc] initWithContentsOfFile:"Resources/button-reset.png" size:CPSizeMake(90, 28)],
        _buttonPressImage = [[CPImage alloc] initWithContentsOfFile:"Resources/button-reset-press.png" size:CPSizeMake(90, 28)],
        _resetButton = [[CPButton alloc] initWithFrame:CGRectMake(195, 422, 90, 28)];

    [_resetButton setImage:_buttonImage];
    [_resetButton setAlternateImage:_buttonPressImage];
    [_resetButton setBordered:NO];

    [contentView addSubview:_board];
    [contentView addSubview:_resetButton];

    [_resetButton setTarget:_board];
    [_resetButton setAction:@selector(resetBoard)];

    [gameWindow orderFront:self];
    [infoWindow orderFront:self];
}

@end