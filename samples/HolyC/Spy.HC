// Pass the task structure you
// wish to spy upon.

U0 SpyStkRep(CTask *task)
{
	CDoc *doc=DocDblBufStart;
	while (!ScanChar && TaskValidate(task)) {
		StkRep(task);
		'\n';
		"$$BK,1$$PRESS A KEY$$BK,0$$\n";
		DocDblBufSwap;
		Refresh;
	}
	if (DocPut==doc) DocDblBufSwap;
	DocDblBufEnd;
}

U0 SpyCallerRep(CTask *task)
{
	CDoc *doc=DocDblBufStart;
	while (!ScanChar && TaskValidate(task)) {
		CallerRep(task->rbp,task);
		'\n';
		"$$BK,1$$PRESS A KEY$$BK,0$$\n";
		DocDblBufSwap;
		Refresh;
	}
	if (DocPut==doc) DocDblBufSwap;
	DocDblBufEnd;
}

U0 Spy()
{
	I64 i;
	CTask *task;

	while (TRUE) {
		task=Fs->next_task;
		while (task!=Fs) {
			"$$MA+X,T=\"Task:%08X:%16ts\",LM=\"0x%08X\n\"$$\n",
			task,task->task_name,task;
			task=task->next_task;
		}
		'\n';
		task=GetI64("Task Addr: ",0);
		if (TaskValidate(task)) {
			"\n\n\t$$BT,\"StkRep\",LM=\"1\n\"$$\n\n\n"
			"\n\n\t$$BT,\"CallerRep\",LM=\"2\n\"$$\n\n\n";
			i=GetI64("",0);
			if (i==1)
				SpyStkRep(task);
			else if (i==2)
				SpyCallerRep(task);
			else
				break;
		} 
		else
			break;
	}
	'\n';
}

Spy;
