import arcpy, re, math


class Toolbox(object):
    def __init__(self):
        """Define the toolbox (the name of the toolbox is the name of the
        .pyt file)."""
        self.label = "Aeronautical Batch"
        self.alias = "wsdot_aero"

        # List of tool classes associated with this toolbox
        self.tools = [GenerateSurfaces]


class GenerateSurfaces(object):


    def __init__(self):
        """Define the tool (tool name is the name of the class)."""
        self.label = "Generate Surfaces"
        self.description = "Generates FAA FAR 77 surfaces from runway centerline features"
        self.canRunInBackground = False

    def getParameterInfo(self):
        """Define parameter definitions"""
        centerlinesParam = arcpy.Parameter(
            displayName="Runway Centerline Features",
            name="centerline_features",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Input")
        centerlinesParam.filter.list = ["Polyline"]
        surfaceParam = arcpy.Parameter(
            displayName="Surface Feature Class",
            name="surface_fc",
            datatype="DEFeatureClass",
            parameterType="Required",
            direction="Input")
        approachTypesTableParam = arcpy.Parameter(
            displayName="Approach Types Table",
            name="approach_types_table",
            datatype="GPTableView",
            parameterType="Required",
            direction="Input")
        outputParam = arcpy.Parameter(
            displayName="Output Surface Feature Class",
            name="output_fc",
            datatype="DEFeatureClass",
            parameterType="Derived",
            direction="Output")
        outputParam.parameterDependencies = [surfaceParam.name]
        outputParam.schema.clone = True

        params = [centerlinesParam, surfaceParam, approachTypesTableParam, outputParam]
        return params

    def isLicensed(self):
        """Set whether tool is licensed to execute.
        Returns true if the following conditions are met.
        * Either ArcEditor or ArcInfo license must be available.
        * Aeronautical license must be available.
        """        
        # okRe = re.compile("(Available)|(AlreadyInitialized)", re.IGNORECASE)
        # return ((okRe.match(arcpy.CheckProduct("arceditor")) 
                # or okRe.match(arcpy.CheckProduct("arcinfo"))) 
                # and arcpy.CheckExtension("Aeronautical") == "Available")
        return True

    def updateParameters(self, parameters):
        """Modify the values and properties of parameters before internal
        validation is performed.  This method is called whenever a parameter
        has been changed."""
        # TODO: 
        # Check Approach Types table for LowApproachType and ESRIApproachType fields.
        # Check centerlines FC for these fields: "AirportReferenceElev", "ClearWayLength", "HighApproachType", "LowApproachType"
        return

    def updateMessages(self, parameters):
        """Modify the messages created by internal validation for each tool
        parameter.  This method is called after internal validation."""
        return

    def execute(self, parameters, messages):
        """The source code of the tool."""
        
        CENTERLINES_PARAM_ID = 0
        SURFACE_PARAM_ID = 1
        APPROACH_TYPES_TABLE_PARAM_ID = 2
        OUTPUT_PARAM_ID = 3

        approachTypesTable = parameters[APPROACH_TYPES_TABLE_PARAM_ID].valueAsText
        runwayCenterlinesFC = parameters[CENTERLINES_PARAM_ID].valueAsText
        surfaceFC = parameters[SURFACE_PARAM_ID].valueAsText

        # Create a dictionary of approach type codes.
        approachTypes = {}
        with arcpy.da.SearchCursor(approachTypesTable, ["LowApproachType", "ESRIApproachType"]) as cursor:
            for row in cursor:
                approachTypes[row[0]] = row[1]
        del approachTypesTable

        fields = [
                    "OID@", #OBJECTID
                    #"SHAPE@", #SHAPE
                    #"OWNERSHIP",
                    #"ASSOCIATED",
                    #"DESIGNATIO",
                    #"RUNWAY_",
                    #"ELEVATION",
                    #"LineID",
                    #"ID",
                    #"NAME",
                    #"COUNTY",
                    #"LENGTH",
                    #"WIDTH",
                    #"SURFACE",
                    "AirportReferenceElev",
                    #"HighApproach",
                    #"LowApproach",
                    "ClearWayLength",
                    #"HighID",
                    #"LowID",
                    "HighApproachType",
                    "LowApproachType",
                    #"Z_Min",
                    #"Z_Max",
                    #"Z_Mean",
                    #"Avg_Slope",
                    # "SHAPE_Length"
                ]

        # Create a dictionary of field names and associated IDs.  Keys are names, values are IDs.
        fieldsDict = {}
        for i in range(0,len(fields)):
            fieldsDict[fields[i]] = i

        layer = "TEMP_LAYER"
        arcpy.management.MakeFeatureLayer(runwayCenterlinesFC, layer)
        centerlineCount = arcpy.management.GetCount(runwayCenterlinesFC)
        centerlineCount = int(centerlineCount.getOutput(0))

        def getIncrement(recordCount):
            """Gets an appropriate increment for the number of records.
            Based on code from http://resources.arcgis.com/en/help/main/10.1/index.html#//001500000032000000
            """
            p = math.log10(recordCount)
            if not p:
                p = 1
            inc = int(math.pow(10, p - 1))
            return inc

        increment = getIncrement(centerlineCount)

        # Setup the progress bar in ArcGIS Desktop to show the progress as each centerline is processed.
        arcpy.SetProgressor("step", "Generating surfaces...", 0, centerlineCount, increment)

        try:
            # Loop through the runway centerline features...
            with arcpy.da.SearchCursor(runwayCenterlinesFC, fields) as cursor:
                for i, row in enumerate(cursor, 0):
                    messages.addMessage("\n\n\nProcessing row %s of %s..." % (i, centerlineCount))

                    try:
                        # Get parameters from the search cursor row.
                        oid = row[0]
                        clear_way_length = row[fieldsDict["ClearWayLength"]]
                        high_runway_end_type = approachTypes[row[fieldsDict["HighApproachType"]]]
                        low_runway_end_type = approachTypes[row[fieldsDict["LowApproachType"]]]
                        airport_elevation = row[fieldsDict["AirportReferenceElev"]]

                        # Select the feature with matching OID.
                        arcpy.management.SelectLayerByAttribute(layer, "NEW_SELECTION", '"OBJECTID" = %s' % oid)

                        arcpy.FAAFAR77_aeronautical(layer, surfaceFC, clear_way_length, "#", high_runway_end_type,
                                                    low_runway_end_type, airport_elevation, "PREDEFINED_SPECIFICATION")
                    except IOError as ioEx:
                        messages.addWarningMessage("An IO error occurred processing record with OID %i.\n%s"  % (ioEx, oid))
                        messages.addGPMessages()
                    except arcpy.ExecuteError as ex:
                        messages.addWarningMessage("Error running FAA FAR 77 tool for OID %i..." % oid)
                        messages.addGPMessages()
                    else:
                        messages.addMessage("Messages for row %i, OID %i:" % (i, oid))
                        messages.addGPMessages()
                    finally:
                        # Update the position of the progress meter.
                        if (i % increment) == 0:
                            arcpy.SetProgressorPosition(i)
        finally:
            # Reset the progress meter to its original state.
            arcpy.ResetProgressor()
            arcpy.management.Delete(layer)
            # TODO: Set output parameter
            # parameters[OUTPUT_PARAM_ID].value = parameters[SURFACE_PARAM_ID].value
        return
