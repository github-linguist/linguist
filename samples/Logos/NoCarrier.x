//
//  NoCarrier.x
//  NoCarrier
//
//  Created by Jonas Gessner on 27.01.2014.
//  Copyright (c) 2014 Jonas Gessner. All rights reserved.
//

#import <CoreGraphics/CoreGraphics.h>

#include <substrate.h>

%group main

%hook UIStatusBarServiceItemView

- (id)_serviceContentsImage {
    return nil;
}

- (CGFloat)extraLeftPadding {
    return 0.0f;
}

- (CGFloat)extraRightPadding {
    return 0.0f;
}

- (CGFloat)standardPadding {
    return 2.0f;
}

%end

%end


%ctor {
	@autoreleasepool {
		%init(main);
	}
}
