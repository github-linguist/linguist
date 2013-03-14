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

#import "MainMenuViewController.h"


///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////
@implementation MainMenuViewController


///////////////////////////////////////////////////////////////////////////////////////////////////
- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil {
  if (self = [super initWithNibName:nil bundle:nil]) {
    self.title = @"Style Catalog";
    //self.variableHeightRows = YES;
    self.tableViewStyle = UITableViewStyleGrouped;

    self.dataSource =
      [TTSectionedDataSource dataSourceWithObjects:
       @"Text Styles",
       [TTTableTextItem itemWithText:@"Link Text"
                                 URL:@"tt://styles/linkText:/text"],
       [TTTableTextItem itemWithText:@"Mini Badge"
                                 URL:@"tt://styles/miniBadge/text"],
       [TTTableTextItem itemWithText:@"Badge"
                                 URL:@"tt://styles/badge/text"],
       [TTTableTextItem itemWithText:@"Large Badge"
                                 URL:@"tt://styles/largeBadge/text"],

       @"Views",
       [TTTableTextItem itemWithText:@"Post Text Editor"
                                 URL:@"tt://styles/postTextEditor/view"],
       [TTTableTextItem itemWithText:@"Photo Caption"
                                 URL:@"tt://styles/photoCaption/view"],
       [TTTableTextItem itemWithText:@"Photo Status Label"
                                 URL:@"tt://styles/photoStatusLabel/view"],
       [TTTableTextItem itemWithText:@"Page Dot"
                                 URL:@"tt://styles/pageDot:/view"],
       [TTTableTextItem itemWithText:@"Highlighted Link"
                                 URL:@"tt://styles/linkHighlighted/view"],
       [TTTableTextItem itemWithText:@"Table Header"
                                 URL:@"tt://styles/tableHeader/view"],
       [TTTableTextItem itemWithText:@"Picker Cell"
                                 URL:@"tt://styles/pickerCell:/view"],
       [TTTableTextItem itemWithText:@"Search Table Shadow"
                                 URL:@"tt://styles/searchTableShadow/view"],
       [TTTableTextItem itemWithText:@"Black Bezel"
                                 URL:@"tt://styles/blackBezel/view"],
       [TTTableTextItem itemWithText:@"White Bezel"
                                 URL:@"tt://styles/whiteBezel/view"],
       [TTTableTextItem itemWithText:@"Black Banner"
                                 URL:@"tt://styles/blackBanner/view"],
       [TTTableTextItem itemWithText:@"Tab Bar"
                                 URL:@"tt://styles/tabBar/view"],
       [TTTableTextItem itemWithText:@"Tab Strip"
                                 URL:@"tt://styles/tabStrip/view"],

       @"Tab Grid",
       [TTTableTextItem itemWithText:@"Tab Grid"
                                 URL:@"tt://styles/tabGrid/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Top Left"
                                 URL:@"tt://styles/tabGridTabTopLeft:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Top Right"
                                 URL:@"tt://styles/tabGridTabTopRight:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Bottom Right"
                                 URL:@"tt://styles/tabGridTabBottomRight:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Bottom Left"
                                 URL:@"tt://styles/tabGridTabBottomLeft:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Left"
                                 URL:@"tt://styles/tabGridTabLeft:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Right"
                                 URL:@"tt://styles/tabGridTabRight:/view"],
       [TTTableTextItem itemWithText:@"Tab Grid Center"
                                 URL:@"tt://styles/tabGridTabCenter:/view"],

       @"Tabs",
       [TTTableTextItem itemWithText:@"Tab"
                                 URL:@"tt://styles/tab:/view"],
       [TTTableTextItem itemWithText:@"Round Tab"
                                 URL:@"tt://styles/tabRound:/view"],
       [TTTableTextItem itemWithText:@"Tab Left Overflow"
                                 URL:@"tt://styles/tabOverflowLeft/view"],
       [TTTableTextItem itemWithText:@"Tab Right Overflow"
                                 URL:@"tt://styles/tabOverflowRight/view"],

       @"Images",
       [TTTableTextItem itemWithText:@"Thumb View"
                                 URL:@"tt://styles/thumbView:/image"],

       @"Launcher",
       [TTTableTextItem itemWithText:@"Launcher Button"
                                 URL:@"tt://styles/launcherButton:/image"],
       [TTTableTextItem itemWithText:@"Launcher Close Button"
                                 URL:@"tt://styles/launcherCloseButton:/view"],

       @"Text Bar",
       [TTTableTextItem itemWithText:@"Text Bar"
                                 URL:@"tt://styles/textBar/view"],
       [TTTableTextItem itemWithText:@"Text Bar Footer"
                                 URL:@"tt://styles/textBarFooter/view"],
       [TTTableTextItem itemWithText:@"Text Bar Text Field"
                                 URL:@"tt://styles/textBarTextField/view"],
       [TTTableTextItem itemWithText:@"Text Bar Post Button"
                                 URL:@"tt://styles/textBarPostButton:/text"],

       @"Toolbars",
       [TTTableTextItem itemWithText:@"Toolbar Button"
                                 URL:@"tt://styles/toolbarButton:/view"],
       [TTTableTextItem itemWithText:@"Toolbar Back Button"
                                 URL:@"tt://styles/toolbarBackButton:/view"],
       [TTTableTextItem itemWithText:@"Toolbar Forward Button"
                                 URL:@"tt://styles/toolbarForwardButton:/view"],
       [TTTableTextItem itemWithText:@"Toolbar Round Button"
                                 URL:@"tt://styles/toolbarRoundButton:/view"],
       [TTTableTextItem itemWithText:@"Black Toolbar Button"
                                 URL:@"tt://styles/blackToolbarButton:/view"],
       [TTTableTextItem itemWithText:@"Gray Toolbar Button"
                                 URL:@"tt://styles/grayToolbarButton:/view"],
       [TTTableTextItem itemWithText:@"Black Toolbar Forward Button"
                                 URL:@"tt://styles/blackToolbarForwardButton:/view"],
       [TTTableTextItem itemWithText:@"Black Toolbar Round Button"
                                 URL:@"tt://styles/blackToolbarRoundButton:/view"],

       @"Search",
       [TTTableTextItem itemWithText:@"Search Text Field"
                                 URL:@"tt://styles/searchTextField/view"],
       [TTTableTextItem itemWithText:@"Search Bar"
                                 URL:@"tt://styles/searchBar/view"],
       [TTTableTextItem itemWithText:@"Search Bar Bottom"
                                 URL:@"tt://styles/searchBarBottom/view"],
       [TTTableTextItem itemWithText:@"Black Search Bar"
                                 URL:@"tt://styles/blackSearchBar/view"],

       nil];
  }

  return self;
}


@end

