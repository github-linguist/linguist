ARCHS = armv7 arm64
ADDITIONAL_OBJCFLAGS = -fobjc-arc

include theos/makefiles/common.mk

TWEAK_NAME = PowerTap
PowerTap_FILES = Tweak.xm PTPreferences.m
PowerTap_FRAMEWORKS = UIKit CoreFoundation
PowerTap_PRIVATE_FRAMEWORKS = TelephonyUI

include $(THEOS_MAKE_PATH)/tweak.mk

after-install::
	install.exec "killall -9 SpringBoard"
SUBPROJECTS += powertapprefs
include $(THEOS_MAKE_PATH)/aggregate.mk
