library LabTestsCDS version '1.1.0'

using FHIR version '4.0.1'

include FHIRHelpers version '4.0.1' called FHIRHelpers

valueset "Female Administrative Sex": '2.16.840.1.113883.3.560.100.2'
valueset "Diabetes": '2.16.840.1.113883.3.464.1003.103.12.1001'
valueset "Hypertension": '2.16.840.1.113883.3.464.1003.104.12.1001'
valueset "Cholesterol Test": '2.16.840.1.113883.3.464.1003.110.12.1055'
valueset "Blood Pressure Test": '2.16.840.1.113883.3.464.1003.106.12.1001'

context Patient

define "Patient 18 or Older":
    AgeInYears() >= 18

define "Patient is Female":
    Patient.gender in "Female Administrative Sex"

define "Lookback Year":
    Interval[Today() - 12 months - 1 days, Today() - 1 day]

define "Patient Has Diabetes":
    exists ([Condition: "Diabetes"] C)

define "Patient Has Hypertension":
    exists ([Condition: "Hypertension"] C)

define "Patient Had Cholesterol Test in Last Year":
    exists ([Observation: "Cholesterol Test"] R where R.effective during "Lookback Year" and R.value is not null)

define "Patient Had Blood Pressure Test in Last Year":
    exists ([Observation: "Blood Pressure Test"] R where R.effective during "Lookback Year" and R.value is not null)

define "Patient Needs Cholesterol Test":
    "Patient 18 or Older" and "Patient Has Diabetes" and not "Patient Had Cholesterol Test in Last Year"

define "Patient Needs Blood Pressure Test":
    "Patient 18 or Older" and "Patient Has Hypertension" and not "Patient Had Blood Pressure Test in Last Year"