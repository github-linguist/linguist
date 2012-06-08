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

#import "StyleViewController.h"

#import "StyleView.h"

NSString* kTextStyleType  = @"text";
NSString* kViewStyleType  = @"view";
NSString* kImageStyleType = @"image";


///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
@implementation StyleViewController


///////////////////////////////////////////////////////////////////////////////////////////////////
- (id)initWithStyleName:(NSString*)name styleType:(NSString*)styleType {
  if (self = [super initWithNibName:nil bundle:nil]) {
    self.title = name;

    _style = [[[TTStyleSheet globalStyleSheet] styleWithSelector:name] retain];
    _styleHighlight = [[[TTStyleSheet globalStyleSheet]
                        styleWithSelector: name
                        forState: UIControlStateHighlighted] retain];
    _styleDisabled = [[[TTStyleSheet globalStyleSheet]
                      styleWithSelector: name
                       forState: UIControlStateDisabled] retain];
    _styleSelected = [[[TTStyleSheet globalStyleSheet]
                       styleWithSelector: name
                       forState: UIControlStateSelected] retain];

    _styleType = [styleType copy];
  }

  return self;
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void)dealloc {
  TT_RELEASE_SAFELY(_style);
  TT_RELEASE_SAFELY(_styleHighlight);
  TT_RELEASE_SAFELY(_styleDisabled);
  TT_RELEASE_SAFELY(_styleSelected);
  TT_RELEASE_SAFELY(_styleType);

  [super dealloc];
}


///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
#pragma mark -
#pragma mark UIViewController


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void)addTextView:(NSString*)title frame:(CGRect)frame style:(TTStyle*)style {
  CGRect textFrame = TTRectInset(frame, UIEdgeInsetsMake(20, 20, 20, 20));
  StyleView* text = [[StyleView alloc]
                     initWithFrame:textFrame];
  text.text = title;
  TTStyleContext* context = [[TTStyleContext alloc] init];
  context.frame = frame;
  context.delegate = text;
  context.font = [UIFont systemFontOfSize:[UIFont systemFontSize]];
  CGSize size = [style addToSize:CGSizeZero context:context];
  TT_RELEASE_SAFELY(context);

  size.width += 20;
  size.height += 20;
  textFrame.size = size;
  text.frame = textFrame;

  text.style = style;
  text.backgroundColor = [UIColor colorWithRed:0.9 green:0.9 blue:1 alpha:1];
  text.autoresizingMask =
  UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleBottomMargin;

  [self.view addSubview:text];
  TT_RELEASE_SAFELY(text);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void)addView:(CGRect)frame style:(TTStyle*)style {
  CGRect viewFrame = TTRectInset(frame, UIEdgeInsetsMake(20, 20, 20, 20));
  StyleView* view = [[StyleView alloc]
                     initWithFrame:viewFrame];

  view.style = style;
  view.backgroundColor = [UIColor colorWithRed:0.9 green:0.9 blue:1 alpha:1];
  view.autoresizingMask =
  UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleBottomMargin;

  [self.view addSubview:view];
  TT_RELEASE_SAFELY(view);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void)addImageView:(CGRect)frame style:(TTStyle*)style {
  CGRect viewFrame = TTRectInset(frame, UIEdgeInsetsMake(20, 20, 20, 20));
  TTImageView* view = [[TTImageView alloc]
                       initWithFrame:viewFrame];

  view.urlPath = @"bundle://Icon.png";
  view.style = style;
  view.backgroundColor = [UIColor colorWithRed:0.9 green:0.9 blue:1 alpha:1];
  view.autoresizingMask =
  UIViewAutoresizingFlexibleWidth | UIViewAutoresizingFlexibleBottomMargin;
  CGRect imageFrame = view.frame;
  imageFrame.size = view.image.size;
  view.frame = imageFrame;

  [self.view addSubview:view];
  TT_RELEASE_SAFELY(view);
}


///////////////////////////////////////////////////////////////////////////////////////////////////
- (void)loadView {
  [super loadView];

  CGRect frame = self.view.bounds;
  frame.size.height /= 4;

  if ([_styleType isEqualToString:kTextStyleType]) {
    [self addTextView:@"UIControlStateNormal" frame:frame style:_style];

    frame.origin.y += frame.size.height;
    [self addTextView:@"UIControlStateHighlighted" frame:frame style:_styleHighlight];

    frame.origin.y += frame.size.height;
    [self addTextView:@"UIControlStateDisabled" frame:frame style:_styleDisabled];

    frame.origin.y += frame.size.height;
    [self addTextView:@"UIControlStateSelected" frame:frame style:_styleSelected];

  } else if ([_styleType isEqualToString:kViewStyleType]) {
    [self addView:frame style:_style];

    frame.origin.y += frame.size.height;
    [self addView:frame style:_styleHighlight];

    frame.origin.y += frame.size.height;
    [self addView:frame style:_styleDisabled];

    frame.origin.y += frame.size.height;
    [self addView:frame style:_styleSelected];

  } else if ([_styleType isEqualToString:kImageStyleType]) {
    [self addImageView:frame style:_style];

    frame.origin.y += frame.size.height;
    [self addImageView:frame style:_styleHighlight];

    frame.origin.y += frame.size.height;
    [self addImageView:frame style:_styleDisabled];

    frame.origin.y += frame.size.height;
    [self addImageView:frame style:_styleSelected];

  }
}


@end

