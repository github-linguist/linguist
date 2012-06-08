/*
 Copyright 2011 Twitter, Inc.
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this work except in compliance with the License.
 You may obtain a copy of the License in the LICENSE file, or at:
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 */

#import "TUITableView.h"
#import "TUITableView+Cell.h"
#import "TUITableViewSectionHeader.h"
#import "TUINSView.h"

// header views need to be above the cells at all times
#define HEADER_Z_POSITION 1000 

typedef struct {
	CGFloat offset; // from beginning of section
	CGFloat height;
} TUITableViewRowInfo;

@interface TUITableViewSection : NSObject
{
	__unsafe_unretained TUITableView  *_tableView;   // weak
	TUIView              *_headerView;  // Not reusable (similar to UITableView)
	NSInteger             sectionIndex;
	NSUInteger            numberOfRows;
	CGFloat               sectionHeight;
	CGFloat               sectionOffset;
	TUITableViewRowInfo  *rowInfo;
}

@property (strong, readonly) TUIView           *headerView;
@property (nonatomic, assign) CGFloat   sectionOffset;
@property (readonly) NSInteger          sectionIndex;

@end

@implementation TUITableViewSection

@synthesize sectionOffset;
@synthesize sectionIndex;

- (id)initWithNumberOfRows:(NSUInteger)n sectionIndex:(NSInteger)s tableView:(TUITableView *)t
{
	if((self = [super init])){
		_tableView = t;
		sectionIndex = s;
		numberOfRows = n;
		rowInfo = calloc(n, sizeof(TUITableViewRowInfo));
	}
	return self;
}

- (void)dealloc
{
	if(rowInfo) free(rowInfo);
}

- (NSUInteger)numberOfRows
{
	return numberOfRows;
}

- (void)_setupRowHeights
{
	sectionHeight = 0.0;
	
	TUIView *header;
	if((header = self.headerView) != nil) {
		sectionHeight += roundf(header.frame.size.height);
	}
  
	for(int i = 0; i < numberOfRows; ++i) {
		CGFloat h = roundf([_tableView.delegate tableView:_tableView heightForRowAtIndexPath:[TUIFastIndexPath indexPathForRow:i inSection:sectionIndex]]);
		rowInfo[i].offset = sectionHeight;
		rowInfo[i].height = h;
		sectionHeight += h;
	}
	
}

- (CGFloat)rowHeight:(NSInteger)i
{
	if(i >= 0 && i < numberOfRows) {
		return rowInfo[i].height;
	}
	return 0.0;
}

- (CGFloat)sectionRowOffset:(NSInteger)i
{
	if(i >= 0 && i < numberOfRows){
		return rowInfo[i].offset;
	}
	return 0.0;
}

- (CGFloat)tableRowOffset:(NSInteger)i
{
	return sectionOffset + [self sectionRowOffset:i];
}

- (CGFloat)sectionHeight
{
	return sectionHeight;
}

- (CGFloat)headerHeight
{
	return (self.headerView != nil) ? self.headerView.frame.size.height : 0;
}

/**
 * @brief Obtain the section header view.
 * 
 * The section header view is created lazily via the data source when this
 * method is first called.
 * 
 * @return section header view
 */
- (TUIView *)headerView
{
	if(_headerView == nil) {
		if(_tableView.dataSource != nil && [_tableView.dataSource respondsToSelector:@selector(tableView:headerViewForSection:)]){
			_headerView = [_tableView.dataSource tableView:_tableView headerViewForSection:sectionIndex];
			_headerView.autoresizingMask = TUIViewAutoresizingFlexibleWidth;
			_headerView.layer.zPosition = HEADER_Z_POSITION;
		}
	}
	return _headerView;
}

@end

@interface TUITableView (Private)
- (void)_updateSectionInfo;
- (void)_updateDerepeaterViews;
@end

@implementation TUITableView

@synthesize pullDownView=_pullDownView;
@synthesize headerView=_headerView;

- (id)initWithFrame:(CGRect)frame style:(TUITableViewStyle)style
{
	if((self = [super initWithFrame:frame])) {
		_style = style;
		_reusableTableCells = [[NSMutableDictionary alloc] init];
		_visibleSectionHeaders = [[NSMutableIndexSet alloc] init];
		_visibleItems = [[NSMutableDictionary alloc] init];
		_tableFlags.animateSelectionChanges = 1;
	}
	return self;
}

- (id)initWithFrame:(CGRect)frame
{
	return [self initWithFrame:frame style:TUITableViewStylePlain];
}


- (id<TUITableViewDelegate>)delegate
{
	return (id<TUITableViewDelegate>)[super delegate];
}

- (void)setDelegate:(id<TUITableViewDelegate>)d
{
	_tableFlags.delegateTableViewWillDisplayCellForRowAtIndexPath = [d respondsToSelector:@selector(tableView:willDisplayCell:forRowAtIndexPath:)];
	[super setDelegate:d]; // must call super
}

- (id<TUITableViewDataSource>)dataSource
{
	return _dataSource;
}

- (void)setDataSource:(id<TUITableViewDataSource>)d
{
	_dataSource = d;
	_tableFlags.dataSourceNumberOfSectionsInTableView = [_dataSource respondsToSelector:@selector(numberOfSectionsInTableView:)];
}

- (BOOL)animateSelectionChanges
{
	return _tableFlags.animateSelectionChanges;
}

- (void)setAnimateSelectionChanges:(BOOL)a
{
	_tableFlags.animateSelectionChanges = a;
}

- (NSInteger)numberOfSections
{
	return [_sectionInfo count];
}

- (NSInteger)numberOfRowsInSection:(NSInteger)section
{
	return [[_sectionInfo objectAtIndex:section] numberOfRows];
}

- (CGRect)rectForHeaderOfSection:(NSInteger)section {
	if(section >= 0 && section < [_sectionInfo count]){
		TUITableViewSection *s = [_sectionInfo objectAtIndex:section];
		CGFloat offset = [s sectionOffset];
		CGFloat height = [s headerHeight];
		CGFloat y = _contentHeight - offset - height;
		return CGRectMake(0, y, self.bounds.size.width, height);
	}
	return CGRectZero;
}

- (CGRect)rectForSection:(NSInteger)section
{
	if(section >= 0 && section < [_sectionInfo count]){
		TUITableViewSection *s = [_sectionInfo objectAtIndex:section];
		CGFloat offset = [s sectionOffset];
		CGFloat height = [s sectionHeight];
		CGFloat y = _contentHeight - offset - height;
		return CGRectMake(0, y, self.bounds.size.width, height);
	}
	return CGRectZero;
}

- (CGRect)rectForRowAtIndexPath:(TUIFastIndexPath *)indexPath
{
	NSInteger section = indexPath.section;
	NSInteger row = indexPath.row;
	if(section >= 0 && section < [_sectionInfo count]) {
		TUITableViewSection *s = [_sectionInfo objectAtIndex:section];
		CGFloat offset = [s tableRowOffset:row];
		CGFloat height = [s rowHeight:row];
		CGFloat y = _contentHeight - offset - height;
		return CGRectMake(0, y, self.bounds.size.width, height);
	}
	return CGRectZero;
}

/**
 * @brief Update section info
 * 
 * The previous section info is released and new section info is created.
 */
- (void)_updateSectionInfo {
  
  if(_sectionInfo != nil){
    
    // remove any visible headers, they should be re-added when the table is laid out
    for(TUITableViewSection *section in _sectionInfo){
      TUIView *headerView;
      if((headerView = [section headerView]) != nil){
        [headerView removeFromSuperview];
      }
    }
    
    // clear visible section headers
    [_visibleSectionHeaders removeAllIndexes];
    // clear the section info array
	_sectionInfo = nil;
  }
  
	NSInteger numberOfSections = 1;
	if(_tableFlags.dataSourceNumberOfSectionsInTableView){
		numberOfSections = [_dataSource numberOfSectionsInTableView:self];
	}
	
	NSMutableArray *sections = [[NSMutableArray alloc] initWithCapacity:numberOfSections];
	
	CGFloat offset = [_headerView bounds].size.height - self.contentInset.top*2;
	for(int s = 0; s < numberOfSections; ++s) {
		TUITableViewSection *section = [[TUITableViewSection alloc] initWithNumberOfRows:[_dataSource tableView:self numberOfRowsInSection:s] sectionIndex:s tableView:self];
		[section _setupRowHeights];
		section.sectionOffset = offset;
		offset += [section sectionHeight];
		[sections addObject:section];
	}
	
	_contentHeight = offset - self.contentInset.bottom;
	_sectionInfo = sections;
	
}

- (void)_enqueueReusableCell:(TUITableViewCell *)cell
{
	NSString *identifier = cell.reuseIdentifier;
	
	if(!identifier)
		return;
	
	NSMutableArray *array = [_reusableTableCells objectForKey:identifier];
	if(!array) {
		array = [[NSMutableArray alloc] init];
		[_reusableTableCells setObject:array forKey:identifier];
	}
	[array addObject:cell];
}

- (TUITableViewCell *)dequeueReusableCellWithIdentifier:(NSString *)identifier
{
	if(!identifier)
		return nil;
	
	NSMutableArray *array = [_reusableTableCells objectForKey:identifier];
	if(array) {
		TUITableViewCell *c = [array lastObject];
		if(c) {
			[array removeLastObject];
			[c prepareForReuse];
			return c;
		}
	}
	return nil;
}

/**
 * @brief Obtain the header view for the specified section
 * 
 * If the section has no header, nil is returned.
 * 
 * @param section the section
 * @return section header
 */
- (TUIView *)headerViewForSection:(NSInteger)section {
  if(section >= 0 && section < [_sectionInfo count]){
    return [(TUITableViewSection *)[_sectionInfo objectAtIndex:section] headerView];
  }else{
    return nil;
  }
}

- (TUITableViewCell *)cellForRowAtIndexPath:(TUIFastIndexPath *)indexPath // returns nil if cell is not visible or index path is out of range
{
	return [_visibleItems objectForKey:indexPath];
}

- (NSArray *)visibleCells
{
	return [_visibleItems allValues];
}

static NSInteger SortCells(TUITableViewCell *a, TUITableViewCell *b, void *ctx)
{
	if(a.frame.origin.y > b.frame.origin.y)
		return NSOrderedAscending;
	return NSOrderedDescending;
}

- (NSArray *)sortedVisibleCells
{
	NSArray *v = [self visibleCells];
	return [v sortedArrayUsingComparator:(NSComparator)^NSComparisonResult(TUITableViewCell *a, TUITableViewCell *b) {
		if(a.frame.origin.y > b.frame.origin.y)
			return NSOrderedAscending;
		return NSOrderedDescending;
	}];
}

#define INDEX_PATHS_FOR_VISIBLE_ROWS [_visibleItems allKeys]

- (NSArray *)indexPathsForVisibleRows
{
	return INDEX_PATHS_FOR_VISIBLE_ROWS;
}

- (TUIFastIndexPath *)indexPathForCell:(TUITableViewCell *)c
{
	for(TUIFastIndexPath *i in _visibleItems) {
		TUITableViewCell *cell = [_visibleItems objectForKey:i];
		if(cell == c)
			return i;
	}
	return nil;
}

/**
 * @brief Obtain the indexes of sections which intersect @p rect.
 * 
 * @param rect the rect
 * @return intersecting sections
 */
- (NSIndexSet *)indexesOfSectionsInRect:(CGRect)rect
{
	NSMutableIndexSet *indexes = [[NSMutableIndexSet alloc] init];
	
	for(int i = 0; i < [_sectionInfo count]; i++) {
		if(CGRectIntersectsRect([self rectForSection:i], rect)){
			[indexes addIndex:i];
		}
	}
	
	return indexes;
}

/**
 * @brief Obtain the indexes of sections whose header views intersect @p rect.
 * 
 * @param rect the rect
 * @return intersecting sections
 */
- (NSIndexSet *)indexesOfSectionHeadersInRect:(CGRect)rect
{
	NSMutableIndexSet *indexes = [[NSMutableIndexSet alloc] init];
	
	for(int i = 0; i < [_sectionInfo count]; i++) {
		if(CGRectIntersectsRect([self rectForHeaderOfSection:i], rect)){
			[indexes addIndex:i];
		}
	}
	
	return indexes;
}

- (NSArray *)indexPathsForRowsInRect:(CGRect)rect
{
	NSMutableArray *indexPaths = [NSMutableArray arrayWithCapacity:50];
	NSInteger sectionIndex = 0;
	for(TUITableViewSection *section in _sectionInfo) {
		NSInteger numberOfRows = [section numberOfRows];
		for(NSInteger row = 0; row < numberOfRows; ++row) {
			TUIFastIndexPath *indexPath = [TUIFastIndexPath indexPathForRow:row inSection:sectionIndex];
			CGRect cellRect = [self rectForRowAtIndexPath:indexPath];
			if(CGRectIntersectsRect(cellRect, rect)) {
				[indexPaths addObject:indexPath];
			} else {
				// not visible
			}
		}
		++sectionIndex;
	}
	return indexPaths;
}

/**
 * @brief Obtain the index path of the row at the specified point
 * 
 * If the point is not valid or no row exists at that point, nil is
 * returned.
 * 
 * @param point location in the table view
 * @return index path of the row at @p point
 */
- (TUIFastIndexPath *)indexPathForRowAtPoint:(CGPoint)point {
  
	NSInteger sectionIndex = 0;
  for(TUITableViewSection *section in _sectionInfo){
    for(NSInteger row = 0; row < [section numberOfRows]; row++){
      TUIFastIndexPath *indexPath = [TUIFastIndexPath indexPathForRow:row inSection:sectionIndex];
      CGRect cellRect = [self rectForRowAtIndexPath:indexPath];
      if(CGRectContainsPoint(cellRect, point)){
        return indexPath;
      }
    }
		++sectionIndex;
  }
	
	return nil;
}

/**
 * @brief Obtain the index path of the row at the specified y-coordinate offset
 * 
 * Unlike #indexPathForRowAtPoint:, this method does not consider the x-coordinate.
 * If the offset is not valid or no row exists at that offset, nil is returned.
 * 
 * @param offset y-coordinate offset in the table view
 * @return index path of the row at @p offset
 */
- (TUIFastIndexPath *)indexPathForRowAtVerticalOffset:(CGFloat)offset {
  
	NSInteger sectionIndex = 0;
  for(TUITableViewSection *section in _sectionInfo){
    for(NSInteger row = 0; row < [section numberOfRows]; row++){
      TUIFastIndexPath *indexPath = [TUIFastIndexPath indexPathForRow:row inSection:sectionIndex];
      CGRect cellRect = [self rectForRowAtIndexPath:indexPath];
      if(offset >= cellRect.origin.y && offset <= (cellRect.origin.y + cellRect.size.height)){
        return indexPath;
      }
    }
		++sectionIndex;
  }
	
	return nil;
}

/**
 * @brief Obtain the index of a section whose header is at the specified point
 * 
 * If the point is not valid or no header exists at that point, a negative value
 * is returned.
 * 
 * @param point location in the table view
 * @return index of the section whose header is at @p point
 */
- (NSInteger)indexOfSectionWithHeaderAtPoint:(CGPoint)point {
  
	NSInteger sectionIndex = 0;
  for(TUITableViewSection *section in _sectionInfo){
    TUIView *headerView;
    if((headerView = section.headerView) != nil){
      CGFloat offset = [section sectionOffset];
      CGFloat height = [section headerHeight];
      CGFloat y = _contentHeight - offset - height;
      CGRect frame = CGRectMake(0, y, self.bounds.size.width, height);
      if(point.y > frame.origin.y && point.y < (frame.origin.y + frame.size.height)){
        return sectionIndex;
      }
    }
    sectionIndex++;
  }
	
	return -1;
}

/**
 * @brief Obtain the index of a section whose header is at the specified y-coordinate offset
 * 
 * Unlike #indexOfSectionWithHeaderAtPoint:, this method does not consider the x-coordinate.
 * If the offset is not valid or no header exists at that offset, a negative value
 * is returned.
 * 
 * @param offset y-coordinate offset in the table view
 * @return index of the section whose header is at @p offset
 */
- (NSInteger)indexOfSectionWithHeaderAtVerticalOffset:(CGFloat)offset {
  
	NSInteger sectionIndex = 0;
  for(TUITableViewSection *section in _sectionInfo){
    TUIView *headerView;
    if((headerView = section.headerView) != nil){
      CGFloat offset = [section sectionOffset];
      CGFloat height = [section headerHeight];
      CGFloat y = _contentHeight - offset - height;
      CGRect frame = CGRectMake(0, y, self.bounds.size.width, height);
      if(offset >= frame.origin.y && offset <= (frame.origin.y + frame.size.height)){
        return sectionIndex;
      }
    }
    sectionIndex++;
  }
	
	return -1;
}

/**
 * @brief Enumerate index paths
 * @see #enumerateIndexPathsFromIndexPath:toIndexPath:withOptions:usingBlock:
 */
- (void)enumerateIndexPathsUsingBlock:(void (^)(TUIFastIndexPath *indexPath, BOOL *stop))block {
  [self enumerateIndexPathsFromIndexPath:nil toIndexPath:nil withOptions:0 usingBlock:block];
}

/**
 * @brief Enumerate index paths
 * @see #enumerateIndexPathsFromIndexPath:toIndexPath:withOptions:usingBlock:
 */
- (void)enumerateIndexPathsWithOptions:(NSEnumerationOptions)options usingBlock:(void (^)(TUIFastIndexPath *indexPath, BOOL *stop))block {
  [self enumerateIndexPathsFromIndexPath:nil toIndexPath:nil withOptions:options usingBlock:block];
}

/**
 * @brief Enumerate index paths
 * 
 * The provided block is repeatedly invoked with each valid index path between
 * the specified bounds.  Both bounding index paths are inclusive.
 * 
 * @param fromIndexPath the index path to begin enumerating at or nil to begin at the first index path
 * @param toIndexPath the index path to stop enumerating at or nil to stop at the last index path
 * @param options enumeration options (not currently supported; pass 0)
 * @param block the block to enumerate with
 */
- (void)enumerateIndexPathsFromIndexPath:(TUIFastIndexPath *)fromIndexPath toIndexPath:(TUIFastIndexPath *)toIndexPath withOptions:(NSEnumerationOptions)options usingBlock:(void (^)(TUIFastIndexPath *indexPath, BOOL *stop))block {
  NSInteger sectionLowerBound = (fromIndexPath != nil) ? fromIndexPath.section : 0;
  NSInteger sectionUpperBound = (toIndexPath != nil) ? toIndexPath.section : [self numberOfSections] - 1;
  NSInteger rowLowerBound = (fromIndexPath != nil) ? fromIndexPath.row : 0;
  NSInteger rowUpperBound = (toIndexPath != nil) ? toIndexPath.row : -1;
  
  NSInteger irow = rowLowerBound; // start at the lower bound row for the first iteration...
  for(NSInteger i = sectionLowerBound; i < [self numberOfSections] && i <= sectionUpperBound /* inclusive */; i++){
    NSInteger rowCount = [self numberOfRowsInSection:i];
    for(NSInteger j = irow; j < rowCount && j <= ((rowUpperBound < 0 || i < sectionUpperBound) ? rowCount - 1 : rowUpperBound) /* inclusive */; j++){
      BOOL stop = FALSE;
      block([TUIFastIndexPath indexPathForRow:j inSection:i], &stop);
      if(stop) return;
    }
    irow = 0; // ...then use zero for subsequent iterations
  }
  
}

- (TUIFastIndexPath *)_topVisibleIndexPath
{
	TUIFastIndexPath *topVisibleIndex = nil;
	NSArray *v = [INDEX_PATHS_FOR_VISIBLE_ROWS sortedArrayUsingSelector:@selector(compare:)];
	if([v count])
		topVisibleIndex = [v objectAtIndex:0];
	return topVisibleIndex;
}

- (void)setFrame:(CGRect)f
{
	_tableFlags.forceSaveScrollPosition = 1;
	[super setFrame:f];
}

- (void)setContentOffset:(CGPoint)p
{
	_tableFlags.didFirstLayout = 1; // prevent the auto-scroll-to-top during the first layout
	[super setContentOffset:p];
	
	// if we're currently dragging we need to update the drag operation since the content under
	// the mouse has moved; we just call update again with the last mouse location
  if([self __isDraggingCell]){
    [self __updateDraggingCell:_dragToReorderCell offset:_currentDragToReorderMouseOffset location:_currentDragToReorderLocation];
  }
  
}

- (void)setPullDownView:(TUIView *)p
{
	[_pullDownView removeFromSuperview];
	
	_pullDownView = p;
	
	[self addSubview:_pullDownView];
	_pullDownView.hidden = YES;
}

- (void)setHeaderView:(TUIView *)h
{
	[_headerView removeFromSuperview];
	
	_headerView = h;
	
	[self addSubview:_headerView];
	_headerView.hidden = YES;
}

- (BOOL)_preLayoutCells
{
	CGRect bounds = self.bounds;

	if(!_sectionInfo || !CGSizeEqualToSize(bounds.size, _lastSize)) {
	  
		// save scroll position
		CGFloat previousOffset = 0.0f;
		TUIFastIndexPath *savedIndexPath = nil;
		CGFloat relativeOffset = 0.0;
		if(_tableFlags.maintainContentOffsetAfterReload) {
			previousOffset = self.contentSize.height + self.contentOffset.y;
		} else {
			if(_tableFlags.forceSaveScrollPosition || [self.nsView inLiveResize]) {
				_tableFlags.forceSaveScrollPosition = 0;
				NSArray *a = [INDEX_PATHS_FOR_VISIBLE_ROWS sortedArrayUsingSelector:@selector(compare:)];
				if([a count]) {
					savedIndexPath = [a objectAtIndex:0];
					CGRect v = [self visibleRect];
					CGRect r = [self rectForRowAtIndexPath:savedIndexPath];
					relativeOffset = ((v.origin.y + v.size.height) - (r.origin.y + r.size.height));
					relativeOffset += (_lastSize.height - bounds.size.height);
				}
			} else if(_keepVisibleIndexPathForReload) {
				savedIndexPath = _keepVisibleIndexPathForReload;
				relativeOffset = _relativeOffsetForReload;
				_keepVisibleIndexPathForReload = nil;
			}
		}
		
		[self _updateSectionInfo]; // clean up any previous section info and recreate it
		self.contentSize = CGSizeMake(self.bounds.size.width, _contentHeight);
		
		_lastSize = bounds.size;
		
		if(!_tableFlags.didFirstLayout) {
			_tableFlags.didFirstLayout = 1;
			[self scrollToTopAnimated:NO];
		}
		
		// restore scroll position
		if(_tableFlags.maintainContentOffsetAfterReload) {
			CGFloat newOffset = previousOffset - self.contentSize.height;
			self.contentOffset = CGPointMake(self.contentOffset.x, newOffset);
		} else {
			if(savedIndexPath) {
				CGRect v = [self visibleRect];
				CGRect r = [self rectForRowAtIndexPath:savedIndexPath];
				r.origin.y -= (v.size.height - r.size.height);
				r.size.height += (v.size.height - r.size.height);
				
				r.origin.y += relativeOffset;
				
				[self scrollRectToVisible:r animated:NO];
			}
		}
		
		return YES; // needs visible cells to be redisplayed
	}
	
	return NO; // just need to do the recycling
}

/**
 * @brief Layout header views for sections which have one.
 */
- (void)_layoutSectionHeaders:(BOOL)visibleHeadersNeedRelayout
{
	CGRect visible = [self visibleRect];
	NSIndexSet *oldIndexes = _visibleSectionHeaders;
	NSIndexSet *newIndexes = [self indexesOfSectionsInRect:visible];
	
	NSMutableIndexSet *toRemove = [oldIndexes mutableCopy];
	[toRemove removeIndexes:newIndexes];
	NSMutableIndexSet *toAdd = [newIndexes mutableCopy];
	[toAdd removeIndexes:oldIndexes];
	
	// update the placement of all visible headers
	__block TUIView *pinnedHeader = nil;
	[newIndexes enumerateIndexesUsingBlock:^(NSUInteger index, BOOL *stop) {
		if(index < [_sectionInfo count]) {
			TUITableViewSection *section = [_sectionInfo objectAtIndex:index];
			if(section.headerView != nil) {
				CGRect headerFrame = [self rectForHeaderOfSection:index];
				
				if(_style == TUITableViewStyleGrouped) {
					// check if this header needs to be pinned
					if(CGRectGetMaxY(headerFrame) > CGRectGetMaxY(visible)) {
						headerFrame.origin.y = CGRectGetMaxY(visible) - headerFrame.size.height;
						pinnedHeader = section.headerView;
						// if the header is a TUITableViewSectionHeader notify it of it's pinned state
						if([section.headerView isKindOfClass:[TUITableViewSectionHeader class]]){
							((TUITableViewSectionHeader *)section.headerView).pinnedToViewport = TRUE;
						}
					}else if((pinnedHeader != nil) && (CGRectGetMaxY(headerFrame) > pinnedHeader.frame.origin.y)) {
						// this header is intersecting with the pinned header, so we push the pinned header upwards.
						CGRect pinnedHeaderFrame = pinnedHeader.frame;
						pinnedHeaderFrame.origin.y = CGRectGetMaxY(headerFrame);
						pinnedHeader.frame = pinnedHeaderFrame;
						// if the header is a TUITableViewSectionHeader notify it of it's pinned state
						if([section.headerView isKindOfClass:[TUITableViewSectionHeader class]]){
							((TUITableViewSectionHeader *)section.headerView).pinnedToViewport = FALSE;
						}
					}else{
						// if the header is a TUITableViewSectionHeader notify it of it's pinned state
						if([section.headerView isKindOfClass:[TUITableViewSectionHeader class]]){
							((TUITableViewSectionHeader *)section.headerView).pinnedToViewport = FALSE;
						}
					}
				}
				
				section.headerView.frame = headerFrame;
				[section.headerView setNeedsLayout];
				
				if(section.headerView.superview == nil){
					[self addSubview:section.headerView];
				}
				
			}
		}
		[_visibleSectionHeaders addIndex:index];
	}];
	
	// remove offscreen headers
	[toRemove enumerateIndexesUsingBlock:^(NSUInteger index, BOOL *stop) {
		if(index < [_sectionInfo count]) {
			TUITableViewSection *section = [_sectionInfo objectAtIndex:index];
			if(section.headerView != nil) {
				[section.headerView removeFromSuperview];
			}
		}
		[_visibleSectionHeaders removeIndex:index];
	}];
	
}

- (void)_layoutCells:(BOOL)visibleCellsNeedRelayout
{
  
	if(visibleCellsNeedRelayout) {
		// update remaining visible cells if needed
		for(TUIFastIndexPath *i in _visibleItems) {
			TUITableViewCell *cell = [_visibleItems objectForKey:i];
			cell.frame = [self rectForRowAtIndexPath:i];
			cell.layer.zPosition = 0;
			[cell setNeedsLayout];
		}
	}
	
	CGRect visible = [self visibleRect];
	
	// Example:
	// old:            0 1 2 3 4 5 6 7
	// new:                2 3 4 5 6 7 8 9
	// to remove:      0 1
	// to add:                         8 9
	
	NSArray *oldVisibleIndexPaths = INDEX_PATHS_FOR_VISIBLE_ROWS;
	NSArray *newVisibleIndexPaths = [self indexPathsForRowsInRect:visible];
	
	NSMutableArray *indexPathsToRemove = [oldVisibleIndexPaths mutableCopy];
	[indexPathsToRemove removeObjectsInArray:newVisibleIndexPaths];
	
	NSMutableArray *indexPathsToAdd = [newVisibleIndexPaths mutableCopy];
	[indexPathsToAdd removeObjectsInArray:oldVisibleIndexPaths];
	
	// remove offscreen cells
	for(TUIFastIndexPath *i in indexPathsToRemove) {
		TUITableViewCell *cell = [self cellForRowAtIndexPath:i];
		// don't reuse the dragged cell
		if(_dragToReorderCell == nil || ![cell isEqual:_dragToReorderCell]){
      [self _enqueueReusableCell:cell];
      [cell removeFromSuperview];
      [_visibleItems removeObjectForKey:i];
    }
	}
	
	// add new cells
	for(TUIFastIndexPath *i in indexPathsToAdd) {
		if([_visibleItems objectForKey:i]) {
			NSLog(@"!!! Warning: already have a cell in place for index path %@\n\n\n", i);
		} else {
			TUITableViewCell *cell = [_dataSource tableView:self cellForRowAtIndexPath:i];
			[self.nsView invalidateHoverForView:cell];
			
			cell.frame = [self rectForRowAtIndexPath:i];
			cell.layer.zPosition = 0;
			
			[cell setNeedsLayout];
			[cell prepareForDisplay];
			
			if([i isEqual:_selectedIndexPath]) {
				[cell setSelected:YES animated:NO];
			} else {
				[cell setSelected:NO animated:NO];
			}
			
			if(_tableFlags.delegateTableViewWillDisplayCellForRowAtIndexPath) {
				[_delegate tableView:self willDisplayCell:cell forRowAtIndexPath:i];
			}
			
			[self addSubview:cell];
			
			if([_indexPathShouldBeFirstResponder isEqual:i]) {
			  // only make cells first responder if they accept it
			  if([cell acceptsFirstResponder]){
			    [self.nsWindow makeFirstResponderIfNotAlreadyInResponderChain:cell withFutureRequestToken:_futureMakeFirstResponderToken];
			  }
				_indexPathShouldBeFirstResponder = nil;
			}
			
			[_visibleItems setObject:cell forKey:i];
		}
	}
	
  // if we have a dragged cell, make sure it's on top of the newly added cells
  if([indexPathsToAdd count] > 0 && _dragToReorderCell != nil){
    [[_dragToReorderCell superview] bringSubviewToFront:_dragToReorderCell];
  }
  
	if(_headerView) {
		CGSize s = self.contentSize;
		CGRect headerViewRect = CGRectMake(0, s.height - _headerView.frame.size.height, visible.size.width, _headerView.frame.size.height);
		if(CGRectIntersectsRect(headerViewRect, visible)) {
			_headerView.frame = headerViewRect;			
			[_headerView setNeedsLayout];
			
			if(_headerView.hidden) {
				// show
				_headerView.hidden = NO;
			}
		} else {
			if(!_headerView.hidden) {
				_headerView.hidden = YES;
			}
		}
	}
	
	if(_pullDownView) {
		CGSize s = self.contentSize;
		CGRect pullDownRect = CGRectMake(0, s.height, visible.size.width, _pullDownView.frame.size.height);
		if([self pullDownViewIsVisible]) {
			if(_pullDownView.hidden) {
				// show
				_pullDownView.frame = pullDownRect;
				_pullDownView.hidden = NO;
			}
		} else {
			if(!_pullDownView.hidden) {
				_pullDownView.hidden = YES;
			}
		}
	}
}

- (BOOL)pullDownViewIsVisible
{
	if(_pullDownView) {
		CGSize s = self.contentSize;
		CGRect visible = [self visibleRect];
		CGRect pullDownRect = CGRectMake(0, s.height, visible.size.width, _pullDownView.frame.size.height);
		return CGRectIntersectsRect(pullDownRect, visible);
	}
	return NO;
}

- (void)reloadDataMaintainingVisibleIndexPath:(TUIFastIndexPath *)indexPath relativeOffset:(CGFloat)relativeOffset
{
	_keepVisibleIndexPathForReload = indexPath;
	_relativeOffsetForReload = relativeOffset;
	[self reloadData];
}

- (void)reloadData
{
  
  // notify our delegate we're about to reload the table
  if(self.delegate != nil && [self.delegate respondsToSelector:@selector(tableViewWillReloadData:)]){
    [self.delegate tableViewWillReloadData:self];
  }
	
	_selectedIndexPath = nil;
  
	// need to recycle all visible cells, have them be regenerated on layoutSubviews
	// because the same cells might have different content
	for(TUIFastIndexPath *i in _visibleItems) {
		TUITableViewCell *cell = [_visibleItems objectForKey:i];
		[self _enqueueReusableCell:cell];
		[cell removeFromSuperview];
	}
	
	// if we have a dragged cell, clear it
	_dragToReorderCell = nil;
	
	// clear visible cells
	[_visibleItems removeAllObjects];
	
	// remove any visible headers, they should be re-added when the table is laid out
	for(TUITableViewSection *section in _sectionInfo){
	  TUIView *headerView;
	  if((headerView = [section headerView]) != nil){
	    [headerView removeFromSuperview];
	  }
	}
	
	// clear visible section headers
	[_visibleSectionHeaders removeAllIndexes];
	
	_sectionInfo = nil; // will be regenerated on next layout
	
	[self layoutSubviews];
	
  // notify our delegate the table view has been reloaded
  if(self.delegate != nil && [self.delegate respondsToSelector:@selector(tableViewDidReloadData:)]){
    [self.delegate tableViewDidReloadData:self];
  }
  
}

- (void)layoutSubviews
{
	if(!_tableFlags.layoutSubviewsReentrancyGuard) {
		_tableFlags.layoutSubviewsReentrancyGuard = 1;
		
		[TUIView setAnimationsEnabled:NO block:^{
			[CATransaction begin];
			[CATransaction setDisableActions:YES];
			
			BOOL visibleCellsNeedRelayout = [self _preLayoutCells];
			[super layoutSubviews]; // this will munge with the contentOffset
			[self _layoutSectionHeaders:visibleCellsNeedRelayout];
			[self _layoutCells:visibleCellsNeedRelayout];
			
			if(_tableFlags.derepeaterEnabled)
				[self _updateDerepeaterViews];
			
			[CATransaction commit];
		}];
		
		_tableFlags.layoutSubviewsReentrancyGuard = 0;
	} else {
//		NSLog(@"trying to nest...");
	}
}

- (void)reloadLayout
{
	_sectionInfo = nil; // will be regenerated on next layout
	
	[self _preLayoutCells];
	[super layoutSubviews]; // this will munge with the contentOffset
	[self _layoutSectionHeaders:YES];
	[self _layoutCells:YES];
}

- (void)scrollToRowAtIndexPath:(TUIFastIndexPath *)indexPath atScrollPosition:(TUITableViewScrollPosition)scrollPosition animated:(BOOL)animated
{
	CGRect v = [self visibleRect];
	CGRect r = [self rectForRowAtIndexPath:indexPath];
	
	// when the target index path section has a header view, add its height to
	// the height of our row to prevent the selected row from being overlapped
	// by the pinned header
  TUIView *headerView;
  if((headerView = [self headerViewForSection:indexPath.section]) != nil){
    CGRect headerFrame = [self rectForHeaderOfSection:indexPath.section];
    r.size.height += headerFrame.size.height;
  }
	
	switch(scrollPosition) {
		case TUITableViewScrollPositionNone:
			// do nothing
			break;
		case TUITableViewScrollPositionTop:
			r.origin.y -= (v.size.height - r.size.height);
			r.size.height += (v.size.height - r.size.height);
			[self scrollRectToVisible:r animated:animated];
			break;
		case TUITableViewScrollPositionToVisible:
		default:
			[self scrollRectToVisible:r animated:animated];
			break;
	}
	
}

- (TUIFastIndexPath *)indexPathForSelectedRow
{
	return _selectedIndexPath;
}

- (TUIFastIndexPath *)indexPathForFirstRow
{
	return [TUIFastIndexPath indexPathForRow:0 inSection:0];
}

- (TUIFastIndexPath *)indexPathForLastRow
{
	NSInteger sec = [self numberOfSections] - 1;
	NSInteger row = [self numberOfRowsInSection:sec] - 1;
	return [TUIFastIndexPath indexPathForRow:row inSection:sec];
}

- (void)_makeRowAtIndexPathFirstResponder:(TUIFastIndexPath *)indexPath
{
	TUITableViewCell *cell = [self cellForRowAtIndexPath:indexPath];
	// only cells that accept first responder should be made first responder
	if(cell && [cell acceptsFirstResponder]) {
		[self.nsWindow makeFirstResponderIfNotAlreadyInResponderChain:cell];
	} else {
		_indexPathShouldBeFirstResponder = indexPath;
		_futureMakeFirstResponderToken = [self.nsWindow futureMakeFirstResponderRequestToken];
	}
}

- (void)selectRowAtIndexPath:(TUIFastIndexPath *)indexPath animated:(BOOL)animated scrollPosition:(TUITableViewScrollPosition)scrollPosition
{
	TUIFastIndexPath *oldIndexPath = [self indexPathForSelectedRow];
//	if([indexPath isEqual:oldIndexPath]) {
//		// just scroll to visible
//	} else {
		[self deselectRowAtIndexPath:[self indexPathForSelectedRow] animated:animated];
		
		TUITableViewCell *cell = [self cellForRowAtIndexPath:indexPath]; // may be nil
		[cell setSelected:YES animated:animated];
		 // should already be nil
		_selectedIndexPath = indexPath;
		[cell setNeedsDisplay];
		
		// only notify when the selection actually changes
		if([self.delegate respondsToSelector:@selector(tableView:didSelectRowAtIndexPath:)]){
			[self.delegate tableView:self didSelectRowAtIndexPath:indexPath];
		}
//	}

  NSResponder *firstResponder = [self.nsWindow firstResponder];
  if(firstResponder == self || firstResponder == [self cellForRowAtIndexPath:oldIndexPath]) {
    // only make cell first responder if the table view already is first responder
    [self _makeRowAtIndexPathFirstResponder:indexPath];
  }
	[self scrollToRowAtIndexPath:indexPath atScrollPosition:scrollPosition animated:animated];
}

- (void)deselectRowAtIndexPath:(TUIFastIndexPath *)indexPath animated:(BOOL)animated
{
  
	if([indexPath isEqual:_selectedIndexPath]) {
		TUITableViewCell *cell = [self cellForRowAtIndexPath:indexPath]; // may be nil
		
		[cell setSelected:NO animated:animated];
		_selectedIndexPath = nil;
		[cell setNeedsDisplay];
		
		// only notify when the selection actually changes
    if([self.delegate respondsToSelector:@selector(tableView:didDeselectRowAtIndexPath:)]){
      [self.delegate tableView:self didDeselectRowAtIndexPath:indexPath];
    }
    
	}
	
}

- (TUIFastIndexPath *)indexPathForFirstVisibleRow 
{
	TUIFastIndexPath *firstIndexPath = nil;
	for(TUIFastIndexPath *indexPath in _visibleItems) {
		if(firstIndexPath == nil || [indexPath compare:firstIndexPath] == NSOrderedAscending) {
			firstIndexPath = indexPath;
		}
	}
	return firstIndexPath;
}

- (TUIFastIndexPath *)indexPathForLastVisibleRow 
{
	TUIFastIndexPath *lastIndexPath = nil;
	for(TUIFastIndexPath *indexPath in _visibleItems) {
		if(lastIndexPath == nil || [indexPath compare:lastIndexPath] == NSOrderedDescending) {
			lastIndexPath = indexPath;
		}
	}
	return lastIndexPath;
}

- (BOOL)performKeyAction:(NSEvent *)event
{
	// no selection or selected cell not visible and this is not repeative key press
	BOOL noCurrentSelection = (_selectedIndexPath == nil || ([self cellForRowAtIndexPath:_selectedIndexPath] == nil && ![event isARepeat]));;
	
	typedef TUIFastIndexPath * (^TUITableViewCalculateNextIndexPathBlock)(TUIFastIndexPath *lastIndexPath);
	void (^selectValidIndexPath)(TUIFastIndexPath *startForNoSelection, TUITableViewCalculateNextIndexPathBlock calculateNextIndexPath) = ^(TUIFastIndexPath *startForNoSelection, TUITableViewCalculateNextIndexPathBlock calculateNextIndexPath) {
		NSParameterAssert(calculateNextIndexPath != nil);
		
		BOOL foundValidNextRow = NO;
		TUIFastIndexPath *lastIndexPath = _selectedIndexPath;
		while(!foundValidNextRow) {
			TUIFastIndexPath *newIndexPath;
			if(noCurrentSelection && lastIndexPath == nil) {
				newIndexPath = startForNoSelection;
			} else {
				newIndexPath = calculateNextIndexPath(lastIndexPath);
			}
			
			if(![_delegate respondsToSelector:@selector(tableView:shouldSelectRowAtIndexPath:forEvent:)] || [_delegate tableView:self shouldSelectRowAtIndexPath:newIndexPath forEvent:event]){
				[self selectRowAtIndexPath:newIndexPath animated:self.animateSelectionChanges scrollPosition:TUITableViewScrollPositionToVisible];
				foundValidNextRow = YES;
			}
			
			if([lastIndexPath isEqual:newIndexPath]) foundValidNextRow = YES;
			
			lastIndexPath = newIndexPath;
		}
	};
	
	switch([[event charactersIgnoringModifiers] characterAtIndex:0]) {
		case NSUpArrowFunctionKey: {
			selectValidIndexPath([self indexPathForLastVisibleRow], ^(TUIFastIndexPath *lastIndexPath) {
				NSUInteger section = lastIndexPath.section;
				NSUInteger row = lastIndexPath.row;
				if(row > 0) {
					row--;
				} else {
					while(section > 0) {
						section--;
						NSUInteger rowsInSection = [self numberOfRowsInSection:section];
						if(rowsInSection > 0) {
							row = rowsInSection - 1;
							break;
						}
					}
				}
				
				return [TUIFastIndexPath indexPathForRow:row inSection:section];
			});
			
			return YES;
		}
	
		case NSDownArrowFunctionKey:  {
			selectValidIndexPath([self indexPathForFirstVisibleRow], ^(TUIFastIndexPath *lastIndexPath) {
				NSUInteger section = lastIndexPath.section;
				NSUInteger row = lastIndexPath.row;
				NSUInteger rowsInSection = [self numberOfRowsInSection:section];
				if(row + 1 < rowsInSection) {
					row++;
				} else {
					NSUInteger sections = [self numberOfSections];
					while(section + 1 < sections) {
						section++;
						NSUInteger rowsInSection = [self numberOfRowsInSection:section];
						if(rowsInSection > 0) {
							row = 0;
							break;
						}
					}
				}
				
				return [TUIFastIndexPath indexPathForRow:row inSection:section];
			});

			return YES;
		}
	}
	
	return [super performKeyAction:event];
}

- (BOOL)maintainContentOffsetAfterReload
{
	return _tableFlags.maintainContentOffsetAfterReload;
}

- (void)setMaintainContentOffsetAfterReload:(BOOL)newValue
{
	_tableFlags.maintainContentOffsetAfterReload = newValue;
}

@end


@implementation NSIndexPath (TUITableView)

+ (NSIndexPath *)indexPathForRow:(NSUInteger)row inSection:(NSUInteger)section
{
	NSUInteger i[] = {section, row};
	return [NSIndexPath indexPathWithIndexes:i length:2];
}

- (NSUInteger)section
{
	return [self indexAtPosition:0];
}

- (NSUInteger)row
{
	return [self indexAtPosition:1];
}

@end
