:DCS6
"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC"
:


If 1337!=det([[42:Then
	Disp "DOORS CS 7 NOT","ON CALC.GET DCS7","DCS.CEMETECH.NET
Return:End

GridOff:AxesOff
0->Xmin:94->Xmax
~62->Ymin:0->Ymax
ClrDraw:ClrHome
getTmFmt
Ans->theta
setTmFmt(24

Line(0,0,20,0
Line(21,~1,21,~7
Line(20,~8,0,~8
Text(1,1,"Clicks"
Line(78,0,94,0
Line(77,~1,77,~7
Line(78,~8,94,~8
Text(1,79,"Shop"
Line(23,~11,72,~11
Line(73,~12,73,~61
Line(72,~62,23,~62
Line(22,~61,22,~12
Line(39,~12,39,~61
Line(56,~12,56,~61
Line(72,~28,23,~28
Line(23,~45,72,~45

Line(88,~55,93,~55
Line(94,~56,94,~61
Line(93,~62,88,~62
Line(87,~61,87,~56

Line(89,~57,92,~60
Line(89,~60,92,~57


9->dim(|LTIME
3->dim(|LANS
5->dim(|LITEM
11->dim(|LCOW

If |LCOW(3)=0:Then
	Fill(~1,|LTIME
	1->|LCOW(3)
	20->|LITEM(3)
	75->|LITEM(4)
	250->|LITEM(5)
End

Line(1,~54,5,~54
Line(0,~55,0,~61
Line(6,~55,6,~61
Line(1,~62,5,~62
Text(55,2,"?

Goto PS

Lbl A
{45,32,1
While 1
	sum(6,Ans(1),Ans(2
	Ans->|LANS
	
	If |LANS(1)>=0 and |LANS(1)<=6:Then
		If |LANS(2)>=54 and |LANS(2)<=62:Then
			StorePic 1
			ClrDraw
			Text(0,1,"Gameplay:"
			Text(7,1,"Click on cows to recieve
			Text(14,1,"Mooney and Clicks. Use them
			Text(21,1,"to buy items. Items will only"
			Text(28,1,"appear in the shop when you"
			Text(35,1,"can get them.
			Text(42,1,"Controls:
			Text(49,1,"Arrow keys to move cursor,
			Text(56,1,"[2ND] or [ENTER] to click.
			Pause 
			ClrDraw
			RecallPic 1
		End
	End
	
	If |LANS(1)>=23 and |LANS(1)<=38:Then
		If |LANS(2)>=12 and |LANS(2)<=27:Then
			getTime
			If Ans(1)!=|LTIME(1) or |LITEM(2)!=0:Then
				|LCOW(1)+(1+|LITEM(1))->|LCOW(1)
				|LCOW(2)+(1+|LITEM(1))->|LCOW(2)
				getTime
				Ans(1)->|LTIME(1)
			End
		End
	End
	
	If |LANS(1)>=40 and |LANS(1)<=55:Then
		If |LANS(2)>=12 and |LANS(2)<=27:Then
			getTime
			If Ans(1)!=|LTIME(2) or |LITEM(2)!=0:Then
				If |LCOW(4)=1:Then
					|LCOW(2)+1->|LCOW(2)
					|LCOW(1)+1->|LCOW(1)
					getTime
					Ans(1)->|LTIME(2)
				End
			End
		End
	End
	
	If |LANS(1)>=57 and |LANS(1)<=72:Then
		If |LANS(2)>=12 and |LANS(2)<=27:Then
			getTime
			If Ans(1)!=|LTIME(3) or |LITEM(2)!=0:Then
				If |LCOW(5)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(3)
				End
			End
		End
	End
	
	If |LANS(1)>=23 and |LANS(1)<=38:Then
		If |LANS(2)>=29 and |LANS(2)<=44:Then
			getTime
			If Ans(1)!=|LTIME(4) or |LITEM(2)=1:Then
				If |LCOW(6)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(4)
				End
			End
		End
	End
	
	If |LANS(1)>=40 and |LANS(1)<=55:Then
		If |LANS(2)>=29 and |LANS(2)<=44:Then
			getTime
			If Ans(1)!=|LTIME(5) or |LITEM(2)=1:Then
				If |LCOW(7)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(5)
				End
			End
		End
	End
	
	If |LANS(1)>=57 and |LANS(1)<=72:Then
		If |LANS(2)>=29 and |LANS(2)<=44:Then
			getTime
			If Ans(1)!=|LTIME(6) or |LITEM(2)=1:Then
				If |LCOW(8)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(6)
				End
			End
		End
	End
	
	If |LANS(1)>=23 and |LANS(1)>=38:Then
		If |LANS(2)>=46 and |LANS(2)<=61:Then
			getTime
			If Ans(1)!=|LTIME(7) or |LITEM(2)=1:Then
				If |LCOW(9)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(7)
				End
			End
		End
	End
	
	If |LANS(1)>=40 and |LANS(1)<=55:Then
		If |LANS(2)>=46 and |LANS(2)<=61:Then
			getTime
			If Ans(1)!=|LTIME(8) or |LITEM(2)=1:Then
				If |LCOW(10)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(8)
				End
			End
		End
	End
	
	If |LANS(1)>=57 and |LANS(1)<=72:Then
		If |LANS(2)>=44 and |LANS(2)<=61:Then
			getTime
			If Ans(1)!=|LTIME(9) or |LITEM(2)=1:Then
				If |LCOW(11)=1:Then
					|LCOW(1)+1->|LCOW(1)
					|LCOW(2)+1->|LCOW(2)
					getTime
					Ans(1)->|LTIME(9)
				End
			End
		End
	End
	
	|LANS
	
	If |LANS(1)>=87 and |LANS(1)<=94:Then
		If |LANS(2)>=55 and |LANS(2)<=62:Then
			Goto Q
		End
	End
	
	If |LANS(1)>=0 and |LANS(1)<=21:Then
		If |LANS(2)>=0 and |LANS(2)<=8:Then
			StorePic 1
			ClrDraw
			Text(0,0,"You haved clicked"
			Text(7,0,|LCOW(1)," Cow(s)."
			If |LCOW(1)>=100 and |LCOW(1)<=9999:Text(14,0,"You have a bronze cowbell.
			If |LCOW(1)>=10000 and |LCOW(1)<=99999:Text(14,0,"You have a silver cowbell.
			If |LCOW(1)>=100000 and |LCOW(1)<=999999:Text(14,0,"You have a gold cowbell.
			If |LCOW(1)>=1000000:Text(14,0,"You have a diamond cowbell.
			If |LCOW(1)<100:Text(14,0,"You have no cowbell.
			Text(21,0,"Share your score at
			Text(28,0,"www.cemetech.net!
			Pause 
			ClrDraw:RecallPic 1
		End
	End
	If |LANS(1)>=77 and |LANS(1)<=94:Then
		If |LANS(2)>=0 and |LANS(2)<=8:Then
			
			"Shop
			StorePic 1
			ClrDraw
			Lbl S
			Line(94,~7,94,~1
			Line(93,0,1,0
			Line(0,~7,0,~1
			Line(93,~8,1,~8
			
			Line(88,~55,93,~55
			Line(94,~56,94,~61
			Line(93,~62,88,~62
			Line(87,~61,87,~56
			
			Line(89,~57,92,~60
			Line(89,~60,92,~57
			
			Text(1,2,"Shop!                              Mooney:",|LCOW(2)
			
			Line(45,~18,1,~18
			
			Text(11,14,"Items
			
			If |LCOW(2)>=|LITEM(3):Then
				If |LCOW(11)=0:Then
					Line(45,~20,1,~20
					Line(45,~28,1,~28
					Line(46,~21,46,~27
					Line(0,~21,0,~27
					Text(21,2,"Cow:                   ",|LITEM(3)
				End
			End
			
			If |LCOW(2)>=|LITEM(4):Then
				If |LITEM(1)<=100:Then
					Line(45,~30,1,~30
					Line(45,~38,1,~38
					Line(46,~31,46,~37
					Line(0,~31,0,~37
					Text(31,2,"Pointer:      ",|LITEM(4)
				End
			End
			
			If |LCOW(2)>=|LITEM(5):Then
				If |LITEM(2)=0:Then
					Line(45,~40,1,~40
					Line(45,~48,1,~48
					Line(46,~41,46,~47
					Line(0,~41,0,~47
					Text(41,2,"Clock:            ",|LITEM(5)
				End
			End
			
			{45,32,1
			While 1
				sum(6,Ans(1),Ans(2)
				Ans->|LANS
				
				If |LANS(1)>=0 and |LANS(1)<=46:Then
					If |LANS(2)>=20 and |LANS(2)<=28:Then
						If |LCOW(2)>=20 and |LCOW(11)!=1:Then
							For(A,3,11)
								If |LCOW(A)=0:Then
									|LCOW(2)-|LITEM(3)->|LCOW(2)
									1->|LCOW(A)
									|LITEM(3)+iPart(|LITEM(3)/10)->|LITEM(3)
									ClrDraw
									Goto S
								End
							End
						End
					End
				End
				
				If |LANS(1)>=0 and |LANS(1)<=45:Then
					If |LANS(2)>=30 and |LANS(2)<=38:Then
						If |LCOW(2)>=75:Then
							If |LITEM(1)<=100:Then
								|LITEM(1)+1->|LITEM(1)
								|LCOW(2)-|LITEM(4)->|LCOW(2)
								|LITEM(4)+iPart(|LITEM(4)/25)->|LITEM(4)
								ClrDraw
								Goto S
							End
						End
					End
				End
				
				If |LANS(1)>=0 and |LANS(1)<=46:Then
					If |LANS(2)>=40 and |LANS(2)<=48:Then
						If |LCOW(2)>=250 and |LITEM(2)=0:Then
							1->|LITEM(2)
							|LCOW(2)-250->|LCOW(2)
							|LITEM(5)+iPart(|LITEM(5)/10)->|LITEM(5)
							ClrDraw
							Goto S
						End
					End
				End
				
				If |LANS(1)>=87 and |LANS(1)<=94:Then
					If |LANS(2)>=55 and |LANS(2)<=62:Then
						ClrDraw
						RecallPic 1
						
						Lbl PS
						
						If |LCOW(3)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",23,12,2,16,0,0,1)
						If |LCOW(4)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",40,12,2,16,0,0,1)
						If |LCOW(5)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",57,12,2,16,0,0,1)
						If |LCOW(6)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",23,29,2,16,0,0,1)
						If |LCOW(7)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",40,29,2,16,0,0,1)
						If |LCOW(8)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",57,29,2,16,0,0,1)
						If |LCOW(9)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",23,46,2,16,0,0,1)
						If |LCOW(10)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",40,46,2,16,0,0,1)
						If |LCOW(11)=1
						identity(5,"00000FF01C081A481258125810383FFC4002800188119C399C39881140023FFC",57,46,2,16,0,0,1)
						
						Goto A
					End
				End
			End
		End
	End
End
End

Lbl Q
setTmFmt(theta
DelVar thetaDelVar |LANSDelVar Pic1
ClrDraw:ClrHome
