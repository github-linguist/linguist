# Sample : Using the Web Library

Load "weblib.ring"
Load "datalib.ring"
Import System.Web

website = "ex24.ring"

New SalaryController { Routing() }

Class SalaryModel from ModelBase

Class SalaryController From ControllerBase

Class SalaryView From ViewBase

  oLanguage = new SalaryLanguageEnglish

  Func AddFuncScript oPage,oController
        return   oPage.scriptfuncajax("myadd",oController.cMainURL+
                 oController.cOperation+"=add","mysubpage")

  Func FormViewContent oController,oTranslation,oPage
        return [
                        [ oTranslation.aColumnsTitles[2], "textbox", "name",
                          oController.oModel.Name, oPage.stylewidth("100%")    ],
                        [ oTranslation.aColumnsTitles[3], "textbox", "salary",
                          oController.oModel.Salary, oPage.stylewidth("50%") ]
                   ]

Class SalaryLanguageEnglish
  cTitle = "Salary Table"
  cBack = "back"
  aColumnsTitles = ["ID","Name","Salary"]
  cOptions = "Options"
  cSearch = "Search"
  comboitems = ["Select Option...","Edit","Delete"]
  cAddRecord = "Add Record"
  cEditRecord = "Edit Record"
  cRecordDeleted = "Record Deleted!"
  aMovePages = ["First","Prev","Next","Last"]
  cPage = "Page"
  cOf = "of"
  cRecordsCount = "Records Count"
  cSave = "Save"
  temp = new page
  cTextAlign = temp.StyleTextRight()
  cNoRecords = "No records!"
