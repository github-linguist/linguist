rendering
renderTitleId: divId on: html
	^ html div
		id: #title , divId;
		class: #aClass;
		with: [ 
					html heading
						level3;
						with: self data title ]