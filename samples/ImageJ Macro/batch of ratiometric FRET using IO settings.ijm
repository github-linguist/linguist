/**
*macro to measure the ratio image (FRET/CFP) in 32 bits
*it's necessary to create 3 folders for CFP images and FRET images and one folder for the analysis*
*macro developped by MRI

*/


CH1 = "w1CFP";
CH2 = "w2FRET CY";

dir3 = getDirectory("Choose Destination Directory ");

call("fr.cnrs.mri.macro.io.IOSettings.resetFileLists");
call("fr.cnrs.mri.macro.io.IOSettings.show");
waitForUser("Please select the input files using the IO_Settings dialog and press ok");

fileList = call("fr.cnrs.mri.macro.io.IOSettings.getFileList");
if (fileList=="none") {IJ.log("No files selected! Macro stopped."); return;}
files = split(fileList, ",");

//setBatchMode(true);
for (i=0; i<files.length; i++) {
   	showProgress(i+1, files.length);
    	 open(files[i]);
	nameCFP = getTitle();
	fretFile = replace(files[i], CH1, CH2);
	open(fretFile);
            nameFRET = getTitle();

	run("Clear Results");
	run("Set Measurements...", "  mean redirect=None decimal=3");

	selectWindow(nameCFP);
	setTool("rectangle");
	run("Select None");
	waitForUser("Select areas","Select the area of background and click 'OK'"); 
	roiManager("Add");
	roiManager("Remove Slice Info");

	for (j=1; j<=nSlices; j++) {
   		  showProgress(j, nSlices);
   		  setSlice(j);
			roiManager("Select", 0);
          			selectWindow(nameCFP);
           			run("Measure");
			mean = getResult("Mean");
			selectWindow(nameCFP);
			run("Select None");
			run("Subtract...", "value=mean slice");
          			 run("Clear Results");   
			 }

 

            selectWindow(nameFRET);
	for (k=1; k<=nSlices; k++) {
   		  showProgress(k, nSlices);
   		  setSlice(k);
			roiManager("Select", 0);
          			selectWindow(nameFRET);
           			run("Measure");
			mean = getResult("Mean");
			selectWindow(nameFRET);
			run("Select None");
			run("Subtract...", "value=mean slice");
          			 run("Clear Results");   
			 }


	roiManager("Deselect");
	roiManager("Delete");


            selectWindow(nameFRET);
	run("Duplicate...", "title=mask duplicate range=1-nSlices");
	run("Threshold...");
	waitForUser("Set mask","Threshold set?");  
	run("Make Binary", " ");
	nameMask = getTitle();
	run("Subtract...", "value=254 stack");

	imageCalculator("Multiply stack", nameCFP, nameMask);
	imageCalculator("Multiply stack", nameFRET, nameMask);
	imageCalculator("Divide create 32-bit stack", nameFRET, nameCFP);
	rename("ratio");
	nameRatio = getTitle();
	save(dir3+nameRatio+nameCFP+"-"+i+".tif");
	

run("Close All");
  }
