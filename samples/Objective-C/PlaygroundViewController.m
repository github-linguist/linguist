//
// Copyright 2009-2011 Facebook
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#import "PlaygroundViewController.h"

#import <Three20Core/NSDataAdditions.h>

static const CGFloat kFramePadding    = 10;
static const CGFloat kElementSpacing  = 5;
static const CGFloat kGroupSpacing    = 10;


///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
@implementation PlaygroundViewController


///////////////////////////////////////////////////////////////////////////////////////////////////
- (CGFloat) addHeader:(NSString*)text yOffset:(CGFloat)yOffset {
  UILabel* label = [[UILabel alloc] initWithFrame:CGRectZero];
  label.text = text;
  label.font = [UIFont systemFontOfSize:20];
  label.numberOfLines = 0;

  CGRect frame = label.frame;
  frame.origin.x = kFramePadding;
  frame.origin.y = yOffset;
  frame.size.width = 320 - kFramePadding * 2;
  frame.size.height = [text sizeWithFont:label.font
                       constrainedToSize:CGSizeMake(frame.size.width, 10000)].height;
  label.frame = frame;

  [_scrollView addSubview:label];

  yOffset += label.frame.size.height + kElementSpacing;

  TT_RELEASE_SAFELY(label);

  return yOffset;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (CGFloat) addText:(NSString*)text yOffset:(CGFloat)yOffset {
  UILabel* label = [[UILabel alloc] initWithFrame:CGRectZero];
  label.text = text;
  label.numberOfLines = 0;

  CGRect frame = label.frame;
  frame.origin.x = kFramePadding;
  frame.origin.y = yOffset;
  frame.size.width = 320 - kFramePadding * 2;
  frame.size.height = [text sizeWithFont:label.font
                       constrainedToSize:CGSizeMake(frame.size.width, 10000)].height;
  label.frame = frame;

  [_scrollView addSubview:label];

  yOffset += label.frame.size.height + kElementSpacing;

  TT_RELEASE_SAFELY(label);

  return yOffset;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void) loadView {
  [super loadView];

  _scrollView = [[UIScrollView alloc] initWithFrame:self.view.bounds];
  _scrollView.autoresizingMask =
    UIViewAutoresizingFlexibleWidth
    | UIViewAutoresizingFlexibleHeight;

  [self.view addSubview:_scrollView];

  CGFloat yOffset = kFramePadding;

  yOffset = [self addHeader:NSLocalizedString(@"TTDebug", @"") yOffset:yOffset];

  {
    UIButton* button = [UIButton buttonWithType:UIButtonTypeRoundedRect];
    [button setTitle:NSLocalizedString(@"Debug test", @"") forState:UIControlStateNormal];
    [button addTarget: self
               action: @selector(debugTestAction)
     forControlEvents: UIControlEventTouchUpInside];
    [button sizeToFit];

    CGRect frame = button.frame;
    frame.origin.x = kFramePadding;
    frame.origin.y = yOffset;
    button.frame = frame;

    [_scrollView addSubview:button];

    yOffset += frame.size.height;
  }

  yOffset += kGroupSpacing;

  yOffset = [self addHeader:NSLocalizedString(@"TTGlobalCoreLocale", @"") yOffset:yOffset];
  yOffset = [self addText:[NSString stringWithFormat:NSLocalizedString(@"Current locale: %@", @""),
                           [TTCurrentLocale()
                            displayNameForKey:NSLocaleIdentifier
                                        value:[TTCurrentLocale() localeIdentifier]]]
                  yOffset:yOffset];
  yOffset += kGroupSpacing;

  yOffset = [self addHeader:NSLocalizedString(@"TTGlobalCorePaths", @"") yOffset:yOffset];
  yOffset = [self addText:[NSString stringWithFormat:NSLocalizedString(@"Bundle path: %@", @""),
                           TTPathForBundleResource(@"Icon.png")]
                  yOffset:yOffset];
  yOffset = [self addText:[NSString stringWithFormat:NSLocalizedString(@"Document path: %@", @""),
                           TTPathForDocumentsResource(@"document.pdf")]
                  yOffset:yOffset];
  yOffset += kGroupSpacing;

  yOffset = [self addHeader:NSLocalizedString(@"NSDataAdditions", @"") yOffset:yOffset];
  yOffset = [self addText:[NSString stringWithFormat:NSLocalizedString(@"MD5 Hash of \"Three20\": %@", @""),
                           [[@"Three20" dataUsingEncoding:NSUTF8StringEncoding] md5Hash]]
                  yOffset:yOffset];
  yOffset += kGroupSpacing;

  [_scrollView setContentSize:CGSizeMake(320, yOffset)];
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void) viewDidUnload {
  [super viewDidUnload];

  TT_RELEASE_SAFELY(_scrollView);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void) viewDidAppear:(BOOL)animated {
  [super viewDidAppear:animated];

  [_scrollView flashScrollIndicators];
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void) debugTestAction {
#ifdef DEBUG
  NSLog(@"Three20 debug logging is currently...ON");
#else
  NSLog(@"Three20 debug logging is currently...OFF");
#endif

  // This will print the current method name.
  TTDPRINTMETHODNAME();

  TTDPRINT(@"Showing TTDPRINT.");
  TTDPRINT(@"-----------------");
  TTDPRINT(@"Showing TTD log levels <= %d", TTMAXLOGLEVEL);
  TTDERROR(@"This is TTDERROR, level %d.", TTLOGLEVEL_ERROR);
  TTDWARNING(@"This is TTDWARNING, level %d.", TTLOGLEVEL_WARNING);
  TTDINFO(@"This is TTDINFO, level %d.", TTLOGLEVEL_INFO);

  TTDPRINT(@"");
  TTDPRINT(@"Showing TTDCONDITIONLOG.");
  TTDPRINT(@"------------------------");
  TTDCONDITIONLOG(true, @"This will always display because the condition is \"true\"");
  TTDCONDITIONLOG(false, @"This will never display because the condition is \"false\"");
  TTDCONDITIONLOG(rand()%2, @"This will randomly display because the condition is \"rand()%2\"");

  TTDPRINT(@"");
  TTDPRINT(@"Showing TTDASSERT.");
  TTDPRINT(@"------------------");
  // Should do nothing at all.
  TTDASSERT(true);

  // This will jump you into the debugger in the simulator.
  // Note that this isn't a crash! Simply the equivalent of setting
  // a breakpoint in the debugger, but programmatically. These TTDASSERTs
  // will be completely stripped away from your final product, assuming
  // you don't declare the DEBUG preprocessor macro (and you shouldn't in
  // your final product).
  TTDASSERT(false);
}


@end
