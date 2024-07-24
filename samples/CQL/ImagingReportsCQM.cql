library ImagingReportsCQM version '1.0.0'

using FHIR version '4.0.1'

include FHIRHelpers version '4.0.1' called FHIRHelpers
include CQMCommon version '1.0.0' called CQMCommon

parameter MeasurementPeriod default Interval[@2024, @2025)

context Patient

define "Initial Population":
	Patient.gender = 'female' and AgeInYearsAt(start of "MeasurementPeriod") >= 18

define "Denominator":
	"Initial Population"

define "Denominator Exclusion":
	Patient.gender = 'male' // Exclude males

define "Denominator Exception":
	exists ( [Condition: CQMCommon."Pregnancy"] Pregnancy ) // Patient is pregnant

define "Numerator":
	exists (
		[DiagnosticReport: CQMCommon."Imaging Report"] I 
			where I.status = 'final'
	)