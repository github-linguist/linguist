#import <UIKit/UIKit.h>
#import "substrate.h"

@interface SBPowerDownController

+ (SBPowerDownController*)sharedInstance;

@end

@interface SBPowerDownView : UIView

@end

@interface _UIActionSlider : UIView

@property (readwrite) NSString *trackText;
@property (readwrite) UIImage *knobImage;

- (UIView*)_knobView;
- (UIImageView*)knobImageView;
- (void)setNewKnobImage:(UIImage*)image;
- (void)knobTapped;

@end

@interface UIApplication (PowerOptions)

- (void)reboot;
- (void)terminateWithSuccess;
- (void)nonExistentMethod;

@end