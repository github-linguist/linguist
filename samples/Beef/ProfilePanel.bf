using Beefy.widgets;
using Beefy.theme.dark;
using Beefy.gfx;
using System.Collections;
using System;
using System.Diagnostics;
using Beefy.events;
using System.Threading;
using Beefy;

namespace BeefPerf
{
	class ProfilePanel : Widget
	{
		public class ProfileListViewItem : DarkVirtualListViewItem
		{
			public override void MouseDown(float x, float y, int32 btn, int32 btnCount)
			{
				((ProfileListView)mListView).mProfilePanel.ItemClicked(this, btn, btnCount, x, y);
			}

			public override bool Selected
			{
				set
				{
					if (value)
					{
						int32 selectedIdx = mVirtualIdx;
						var profilePanel = ((ProfileListView)mListView).mProfilePanel;
						var result = profilePanel.mProfileCtx.mResults[selectedIdx];
						DeleteAndNullify!(profilePanel.mPerfView.mProfileHiliteZone);
						profilePanel.mPerfView.mProfileHiliteZone = new PerfView.HiliteZone(result.mZoneNameId, result.mUnformattedName);

						//result.mName
					}

					base.Selected = value;
					//int32 selectedIdx = item.mVirtualIdx;
					//var foundEntry = mSearchState.mFoundEntries[selectedIdx];
				}
			}
		}

		public class ProfileListView : DarkVirtualListView
		{
			public ProfilePanel mProfilePanel;

			public override void ChangeSort(DarkListView.SortType sortType)
			{
				base.ChangeSort(sortType);
				mSortType = sortType;
				mProfilePanel.RefreshList();
			}

			protected override ListViewItem CreateListViewItem()
			{
				return new ProfileListViewItem();
			}

			public override void PopulateVirtualItem(DarkVirtualListViewItem listViewItem)
			{
				base.PopulateVirtualItem(listViewItem);

				var client = mProfilePanel.mPerfView.mSession;
				var perfInfo = mProfilePanel.mProfileCtx.mResults[listViewItem.mVirtualIdx];
				listViewItem.Label = perfInfo.mName;

				var subItem = listViewItem.CreateSubItem(1);
				subItem.mLabel = new String();
				subItem.mLabel.AppendF("{0}", perfInfo.mCount);

				subItem = listViewItem.CreateSubItem(2);
				subItem.mLabel = new String();
				client.ElapsedTicksToStr(perfInfo.mTicks, subItem.mLabel);

				subItem = listViewItem.CreateSubItem(3);
				subItem.mLabel = new String();
				client.ElapsedTicksToStr(perfInfo.mTicks - perfInfo.mChildTicks, subItem.mLabel);
			}

			public override void DrawAll(Graphics g)
			{
				base.DrawAll(g);

				if (((mProfilePanel.mProfileCtx != null) && (!mProfilePanel.mProfileCtx.mDone)) || (mProfilePanel.mSorting))
				{
					using (g.PushColor(0xA0595959))
						g.FillRect(0, 20, mWidth - 20, mHeight - 20);
					BPUtils.DrawWait(g, mWidth / 2, mHeight / 2);
				}
			}

			public override void KeyDown(KeyCode keyCode, bool isRepeat)
			{
				base.KeyDown(keyCode, isRepeat);
				if (keyCode == .Escape)
				{
					mProfilePanel.RemoveFocus();
				}
			}

			public void GetSummaryString(String str)
			{
				str.Append("Name____________________________________Count_______Total________Self\n");
				for (var entry in mProfilePanel.mProfileCtx.mResults)
				{
					str.Append(entry.mName);
					str.Append(' ', Math.Max(38 - entry.mName.Length, 1));

					var entryStr = scope String();
					entry.mCount.ToString(entryStr);
					str.Append(' ', Math.Max(7 - entryStr.Length, 1));
					str.Append(entryStr);

					entryStr.Clear();
					mProfilePanel.mProfileCtx.mSession.ElapsedTicksToStr(entry.mTicks, entryStr);
					str.Append(' ', Math.Max(12 - entryStr.Length, 1));
					str.Append(entryStr);

					entryStr.Clear();
					mProfilePanel.mProfileCtx.mSession.ElapsedTicksToStr(entry.mTicks - entry.mChildTicks, entryStr);
					str.Append(' ', Math.Max(12 - entryStr.Length, 1));
					str.Append(entryStr);
					str.Append('\n');
				}

				str.Append("---------------------------------------------------------------------\n");
				str.Append("Total Time: ");

				mProfilePanel.mProfileCtx.mSession.ElapsedTicksToStr(mProfilePanel.mSelection.mTickEnd - mProfilePanel.mSelection.mTickStart, str);
			}

			public void AddStaticMenu(Menu menu)
			{
				var menuItem = menu.AddItem("Copy to Clipboard");
				menuItem.mOnMenuItemSelected.Add(new (item) =>
				    {
						String str = scope String();
						GetSummaryString(str);
						gApp.SetClipboardText(str);
						//Debug.WriteLine(str);
						
				    });
			}

			public override void MouseDown(float x, float y, int32 btn, int32 btnCount)
			{
				base.MouseDown(x, y, btn, btnCount);

				GetRoot().SelectItemExclusively(null);
				if (btn == 1)
				{
					Menu menu = new Menu();
					AddStaticMenu(menu);
					MenuWidget menuWidget = DarkTheme.sDarkTheme.CreateMenuWidget(menu);
					menuWidget.Init(this, x, y);
				}
			}
		}

		DarkCheckBox mFormatCheckbox;
		ProfileListView mListView;
		DarkButton mGetButton;

		PerfView mPerfView;
		BPSelection mSelection;
		bool mSelectionDirty;
		int64 mActiveLastCurTick;

		ProfileContext mProfileCtx ~ delete _;
		public WaitEvent mSortDoneHandle = new WaitEvent() ~ delete _;
		public bool mSorting;
	
		public this()
		{
			mFormatCheckbox = new DarkCheckBox();
			mFormatCheckbox.Checked = true;
			mFormatCheckbox.Label = "Format Strings";
			mFormatCheckbox.mOnMouseClick.Add(new [&] (evt) => { mSelectionDirty = true; } );
			AddWidget(mFormatCheckbox);

			mListView = new ProfileListView();
			mListView.mProfilePanel = this;
			mListView.mOnLostFocus.Add(new (evt) => { RemoveFocus(); });
			mListView.mSortType.mColumn = 2;
			mListView.mSortType.mReverse = true;
			AddWidget(mListView);
	
			mListView.AddColumn(200, "Name");
			mListView.AddColumn(100, "Count");
			mListView.AddColumn(150, "Total");
			mListView.AddColumn(150, "Self");
			mListView.InitScrollbars(false, true);
		}

		public ~this()
		{
			FinishSorting();
		}

		void FinishSorting()
		{
			if (mSorting)
			{
				mSortDoneHandle.WaitFor();
				mSorting = false;
				mSortDoneHandle.Reset();
			}
		}

		public void RemoveFocus()
		{
			mListView.GetRoot().SelectItemExclusively(null);
			if (mPerfView != null)
				DeleteAndNullify!(mPerfView.mProfileHiliteZone);
		}

		public override void Resize(float x, float y, float width, float height)
		{
			base.Resize(x, y, width, height);
	
			mListView.ResizeClamped(0, 20, width, height - 20);
			mFormatCheckbox.Resize(0, 0, 20, 20);
		}
	
		public override void Draw(Graphics g)
		{
			base.Draw(g);

			g.SetFont(DarkTheme.sDarkTheme.mSmallFont);
			if (mProfileCtx != null)
			{
				String str = scope String();
				str.Append("Total Time: ");
				mProfileCtx.mSession.ElapsedTicksToStr(mSelection.mTickEnd - mSelection.mTickStart, str);
                g.DrawString(str, 150, 0);
			}
			//g.DrawBox(DarkTheme.sDarkTheme.GetImage(DarkTheme.ImageIdx.Bkg), 0, 0, mWidth, mHeight);
		}

		public override void DrawAll(Graphics g)
		{
			base.DrawAll(g);
		}

		public void Show(PerfView perfView, BPSelection selection)
		{
			Debug.Assert(perfView != null);
			mPerfView = perfView;
			mSelection = selection;
			mSelectionDirty = true;
		}

		void GetData()
		{
			mListView.GetRoot().Clear();
	
		}

		struct PerfInfo
		{
			public String mName;
			public int32 mCount;
			public int64 mTicks;
			public int64 mChildTicks;
			public int32 mStackCount; // Number of times this entry appears in entryStack

			public int32 mZoneNameId;
			public String mUnformattedName;
		}

		struct BPPerfEntry
		{
			public int32 mZoneNameId;
			public int64 mTickStart;
			public int64 mChildTicks;
			public int32 mParamsReadPos;
			public PerfInfo* mPerfInfo;
		}

		class ProfileContext
		{
			BumpAllocator mAlloc = new BumpAllocator() ~ delete _;
			public BpSession mSession;
			public Dictionary<String, PerfInfo*> mPerfDict = new .() ~ delete _;
			String mTempStr = new String() ~ delete _;
			String mTempDynStr = new String() ~ delete _;
			public List<PerfInfo*> mResults = new List<PerfInfo*>() ~ delete _;
			public List<PerfInfo*> mSortingResults = new List<PerfInfo*>() ~ delete _;
			public int32 mStreamDataIdx;
			public bool mDone;
			public bool mFormatStrings;
			public bool mHasSelectionEndChanged;

			public PerfInfo* GetPerfInfo(BPPerfEntry entry, BPStateContext stateCtx)
			{
				int32 paramsSize;

				String str;
				if (entry.mZoneNameId < 0)
				{
					int32 nameLen = -entry.mZoneNameId;
					str = mTempDynStr;
					str.Reference((char8*)stateCtx.mReadStart + entry.mParamsReadPos - nameLen, nameLen, 0);
					paramsSize = -1;
				}
				else
				{
					let zoneName = mSession.mZoneNames[entry.mZoneNameId];
					str = zoneName.mName;
					paramsSize = zoneName.mParamsSize;
				}

				//bool dbgStr = str == "DeepStack0 %d";

				String unformattedStr = str;

				if ((paramsSize != 0) && (mFormatStrings))
				{
					mTempStr.Clear();
					stateCtx.FormatStr(entry.mParamsReadPos, paramsSize, str, mTempStr);
					str = mTempStr;
				}

				String* keyPtr;
				PerfInfo* perfInfo;
				PerfInfo** perfInfoPtr;
				if (mPerfDict.TryAdd(str, out keyPtr, out perfInfoPtr))
				{
					perfInfo = new:mAlloc PerfInfo();
					*perfInfoPtr = perfInfo;

					if (str == (Object)mTempStr)
					{
						String heapStr = new:mAlloc String(str);
						*keyPtr = heapStr;
						perfInfo.mName = heapStr;
					}
					else
						perfInfo.mName = str;

					if ((unformattedStr == (Object)mTempDynStr) && (unformattedStr != (Object)mTempStr))
					{
						String heapStr = new:mAlloc String(unformattedStr);
						perfInfo.mUnformattedName = heapStr;
					}
					else
						perfInfo.mUnformattedName = unformattedStr;
					perfInfo.mZoneNameId = entry.mZoneNameId;
				}
				else
				{
					perfInfo = *perfInfoPtr;
					if ((entry.mZoneNameId > 0) && (perfInfo.mZoneNameId < 0))
					{
						// Set to a valid zoneNameId if the inserting entry was a dynamic string but this one isn't
						perfInfo.mZoneNameId = entry.mZoneNameId;
					}
				}

				return perfInfo;
			}
		}

		void UpdateProfileCtx()
		{
			Stopwatch stopwatch = scope Stopwatch();
			stopwatch.Start();

			var client = mPerfView.mSession;
			var thread = client.mThreads[mSelection.mThreadIdx];

			bool isRecording = false;
			bool isFirstDrawn = mProfileCtx.mStreamDataIdx == 0;

			bool isManualSelection = mSelection.mDepth == -1;

			StreamLoop: while (mProfileCtx.mStreamDataIdx < thread.mStreamDataList.Count)
			{
				int streamDataListIdx = mProfileCtx.mStreamDataIdx++;
				var streamData = thread.mStreamDataList[streamDataListIdx];

				if ((streamData.mSplitTick > 0) && (streamData.mSplitTick < mSelection.mTickStart))
					continue; // All data is to the left

				BPStateContext stateCtx = scope BPStateContext(client, streamData);

				List<BPPerfEntry> entryStack = scope List<BPPerfEntry>();
				int32 stackDepth = 0;

				CmdLoop: while (true)
				{
					switch (stateCtx.GetNextEvent())
					{
					case let .Enter(startTick, strIdx):
						int stackPos = stackDepth++;

						if (((mSelection.mDepth == -1) && (startTick >= mSelection.mTickStart)) ||
							((startTick == mSelection.mTickStart) && (stackPos == mSelection.mDepth)))
						{
							isRecording = true;
						}

						if (isRecording)
						{
							BPPerfEntry entry;
							entry.mTickStart = startTick;
							entry.mZoneNameId = strIdx;
							entry.mChildTicks = 0;
							//stateCtx.MoveToParamData();
							entry.mParamsReadPos = stateCtx.ReadPos;
							entry.mPerfInfo = null;
							entry.mPerfInfo = mProfileCtx.GetPerfInfo(entry, stateCtx);
							entry.mPerfInfo.mStackCount++;
							entryStack.Add(entry);
						}
					case let .Leave(endTick):
						stackDepth--;
						if (isRecording)
						{
							let entry = entryStack.PopBack();
							entry.mPerfInfo.mStackCount--;

							if ((entry.mTickStart == mSelection.mTickStart) && (stackDepth == mSelection.mDepth))
							{
								if (client.mCurTick == endTick)
								{
									mProfileCtx.mHasSelectionEndChanged = true;
									mSelection.mTickEnd = endTick;
								}
							}

							int64 ticks = endTick - entry.mTickStart;

							if (isManualSelection)
							{
								int64 tickStart = Math.Max(entry.mTickStart, mSelection.mTickStart);
								int64 tickEnd = Math.Min(endTick, mSelection.mTickEnd);
								ticks = tickEnd - tickStart;
								if (ticks <= 0)
									continue;
							}

							//PerfInfo* perfInfo = mProfileCtx.GetPerfInfo(entry, stateCtx);
							PerfInfo* perfInfo = entry.mPerfInfo;

							bool isOld = ((entry.mTickStart <= streamData.mStartTick) && (stackDepth < stateCtx.mSplitCarryoverCount)); 

							// Is this a duplicate spanned entry?  If so, we don't add it's stats but we still
							//  must process it as a parent to keep track of mChildTicks for new children
							if ((isFirstDrawn) || (!isOld))
							{
								perfInfo.mCount++;
								if (perfInfo.mStackCount != 0)
								{
									// Total time is already handled by outer scope
								}
								else
									perfInfo.mTicks += ticks;
							}

							if (entryStack.Count > 0)
							{
								var prevEntry = entryStack[entryStack.Count - 1];

								//bool prevIsOld = ((prevEntry.mTickStart <= streamData.mStartTick) && (stackDepth - 1 < stateCtx.mSplitCarryoverCount)); 
								//if ((isFirstDrawn) || (!prevIsOld))
								if (!isOld)
								{
									//PerfInfo* prevPerfInfo = mProfileCtx.GetPerfInfo(prevEntry, stateCtx);

									PerfInfo* prevPerfInfo = prevEntry.mPerfInfo;
									prevPerfInfo.mChildTicks += ticks;

									if (perfInfo.mStackCount != 0)
									{
										// We have an instance of ourselves on an outer scope, so time we thought was child time actually isn't
										perfInfo.mChildTicks -= ticks;
									}
								}
							}

							if (isManualSelection)
							{
								if ((stackDepth == 0) && (endTick >= mSelection.mTickEnd))
								{
									if (endTick <= streamData.mSplitTick)
										break StreamLoop;
								}
								//isRecording = false;
							}

							if (stackDepth <= mSelection.mDepth)
							{
								if (endTick <= streamData.mSplitTick)
									break StreamLoop;
								isRecording = false;
							}
						}
					case .EndOfStream:
						break CmdLoop;
					default:
					}
				}

				isFirstDrawn = false;

				if (stopwatch.ElapsedMilliseconds > (int)(gApp.mTimePerFrame * 1000))
				{
					return;
				}
			}
			mProfileCtx.mDone = true;

			for (var value in mProfileCtx.mPerfDict.Keys)
			{
				var perfInfo = @value.Value;
				Debug.Assert(perfInfo.mName != null);
				mProfileCtx.mResults.Add(perfInfo);
			}

			RefreshList();
		}

		void RefreshData()
		{
			mListView.GetRoot().Clear();

			if (mPerfView == null)
				return;

			var session = mPerfView.mSession;
			
			delete mProfileCtx;
			mProfileCtx = new ProfileContext();
			mProfileCtx.mFormatStrings = mFormatCheckbox.Checked;
			mProfileCtx.mSession = session;
		}

		public void ItemClicked(ProfileListViewItem clickedItem, int32 btn, int32 btnCount, float x, float y)
		{
			if (clickedItem.mParentItem == null)
				return;
			ProfileListViewItem item = (ProfileListViewItem)clickedItem.GetSubItem(0);
			mListView.GetRoot().SelectItemExclusively(item);
			mListView.SetFocus();

			if (btn == 1)
			{
				Menu menu = new Menu();

				var menuItem = menu.AddItem("Find Instances");
				menuItem.mOnMenuItemSelected.Add(new (selectedItem) =>
				    {
						var find = gApp.mFindPanel;
						var str = scope String();
						var client = mPerfView.mSession;

						var thread = client.mThreads[mSelection.mThreadIdx];

						str.Clear();
						var perfInfo = mProfileCtx.mResults[item.mVirtualIdx];
						str.Append('=');
						str.Append(perfInfo.mName);
						if (str.Contains(' '))
						{
							str.Insert(1, '\"');
							str.Append('"');
						}

						if (mSelection.mDepth > 0)
						{
	                        str.Append(" Depth>");
							mSelection.mDepth.ToString(str);
						}
						find.mEntryEdit.SetText(str);

						str.Clear();
						str.Append('=');
						thread.GetName(str);
						if (str.Contains(' '))
						{
							str.Insert(1, '\"');
							str.Append('"');
						}
						find.mTrackEdit.mEditWidget.SetText(str);

						str.Clear();
						client.TicksToStr(mSelection.mTickStart, str);
						find.mTimeFromEdit.mEditWidget.SetText(str);

						str.Clear();
						client.TicksToStr(mSelection.mTickEnd, str);
						find.mTimeToEdit.mEditWidget.SetText(str);

						find.mFormatCheckbox.Checked = true;
						find.mZonesCheckbox.Checked = true;
						find.mEventsCheckbox.Checked = false;

						find.mNeedsRestartSearch = true;
				    });

				menu.AddItem();
				mListView.AddStaticMenu(menu);

				MenuWidget menuWidget = DarkTheme.sDarkTheme.CreateMenuWidget(menu);
				menuWidget.Init(clickedItem, x, y);
			}
		}

		public void ValueClicked(MouseEvent theEvent)
		{
		    DarkVirtualListViewItem clickedItem = (DarkVirtualListViewItem)theEvent.mSender;
		    DarkVirtualListViewItem item = (DarkVirtualListViewItem)clickedItem.GetSubItem(0);

		    mListView.GetRoot().SelectItemExclusively(item);
		    mListView.SetFocus();

		    if ((theEvent.mBtn == 0) && (theEvent.mBtnCount > 1))
		    {
		        for (int32 childIdx = 1; childIdx < mListView.GetRoot().GetChildCount(); childIdx++)
		        {
		            var checkListViewItem = mListView.GetRoot().GetChildAtIndex(childIdx);
		            checkListViewItem.IconImage = null;
		        }
		        
		        /*int32 selectedIdx = item.mVirtualIdx;
				var foundEntry = mSearchState.mFoundEntries[selectedIdx];
				mPerfView.ZoomTo(foundEntry.mStartTick, foundEntry.mEndTick);
				BPSelection selection;
				selection.mStartTick = foundEntry.mStartTick;
				selection.mEndTick = foundEntry.mEndTick;
				selection.mDepth = foundEntry.mDepth;
				selection.mThreadIdx = foundEntry.mTrackIdx;
				
				mPerfView.mSelection = selection;*/
		    }

			if (theEvent.mBtn == 1)
			{
				Menu menu = new Menu();

#unwarn
				var menuItem = menu.AddItem("Set Track Color ...");
				MenuWidget menuWidget = DarkTheme.sDarkTheme.CreateMenuWidget(menu);
				menuWidget.Init(this, theEvent.mX, theEvent.mY);
			}
		}

		int EntryCompare(PerfInfo* lhs, PerfInfo* rhs)
		{
			int64 result = 0;
			if (mListView.mSortType.mColumn == 0)
			{
				result = String.Compare(lhs.mName, rhs.mName, true);
				if (result == 0)
					result = lhs.mTicks - rhs.mTicks;
			}
			else if (mListView.mSortType.mColumn == 1)
			{
				result = lhs.mCount - rhs.mCount;
			}
			else if (mListView.mSortType.mColumn == 2)
			{
				result = lhs.mTicks - rhs.mTicks;
			}
			else
			{
				result = (lhs.mTicks - lhs.mChildTicks) - (rhs.mTicks - rhs.mChildTicks);
			}
			if (mListView.mSortType.mReverse)
				result = -result;
			return (int)result;
		}

		void SortList()
		{
			mProfileCtx.mSortingResults.Sort(scope => EntryCompare);
			mSortDoneHandle.Set(true);
		}

		void CheckSorting(int32 waitMS = 0)
		{
			if (mSorting)
			{
				if (mSortDoneHandle.WaitFor(waitMS))
				{
					mSorting = false;
					mSortDoneHandle.Reset();

					Debug.Assert(mProfileCtx.mResults.Count == mProfileCtx.mSortingResults.Count);
					Swap!(mProfileCtx.mResults, mProfileCtx.mSortingResults);

					mListView.GetRoot().Clear();

					if (mProfileCtx.mResults.Count > 0)
					{
						var listViewItem = (DarkVirtualListViewItem)mListView.GetRoot().CreateChildItem();
						listViewItem.mVirtualHeadItem = listViewItem;
						listViewItem.mVirtualCount = (int32)mProfileCtx.mResults.Count;
						mListView.PopulateVirtualItem(listViewItem);
					}
				}
			}
		}

		void RefreshList()
		{
			if (mPerfView == null)
			{
				mListView.GetRoot().Clear();
				return;
			}

			FinishSorting();

			mSorting = true;

			mProfileCtx.mSortingResults.Clear();
			mProfileCtx.mSortingResults.GrowUnitialized(mProfileCtx.mResults.Count);
			for (int i < mProfileCtx.mResults.Count)
				mProfileCtx.mSortingResults[i] = mProfileCtx.mResults[i];

			ThreadPool.QueueUserWorkItem(new => SortList);
			CheckSorting(20);
			
			/*for (var strId in iList)
			{
				var childItem = mListView.GetRoot().CreateChildItem();
				childItem.Label = strId;

				var perfInfo = ref perfDict[strId];

				var subItem = childItem.CreateSubItem(1);
				subItem.mLabel = new String();
				subItem.mLabel.FormatInto("{0}", perfInfo.mCount);
				subItem.mMouseDownHandler.Add(new => ValueClicked);

				subItem = childItem.CreateSubItem(2);
				subItem.mLabel = new String();
				client.ElapsedTicksToStr(perfInfo.mTicks, subItem.mLabel);
				subItem.mMouseDownHandler.Add(new => ValueClicked);

				subItem = childItem.CreateSubItem(3);
				subItem.mLabel = new String();
				client.ElapsedTicksToStr(perfInfo.mTicks - perfInfo.mChildTicks, subItem.mLabel);
				subItem.mMouseDownHandler.Add(new => ValueClicked);
			}*/
		}

		public override void Update()
		{
			base.Update();

			if (mPerfView == null)
				return;

			// Were we awaiting more data to refresh
			var client = mPerfView.mSession;
			if ((mActiveLastCurTick != 0) && (mActiveLastCurTick != client.mCurTick))
			{
				mActiveLastCurTick = 0;
				mSelectionDirty = true;
			}

			if (mSorting)
			{
				CheckSorting();
			}
			else
			{
				if ((mSelectionDirty) && (gApp.mIsUpdateBatchStart) &&
	                ((mProfileCtx == null) || (mProfileCtx.mDone)))
				{
					mSelectionDirty = false;
					RefreshData();
				}

				if ((mProfileCtx != null) && (!mProfileCtx.mDone))
				{
					if (gApp.mIsUpdateBatchStart)
						UpdateProfileCtx();

					if (mProfileCtx.mDone)
					{
						if (mProfileCtx.mHasSelectionEndChanged)
						{
							mActiveLastCurTick = client.mCurTick;
						}
						else
							mActiveLastCurTick = 0;
					}
				}
			}
		}

		public void Clear()
		{
			mListView.GetRoot().Clear();
			DeleteAndNullify!(mProfileCtx);
			mActiveLastCurTick = 0;
			mPerfView = null;
		}
	}
}
