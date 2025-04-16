Load "guilib.ring"

MyApp = new qApp
{
  $ApplicationObject = "oApp"   # To be used when calling events
  oApp = new App
  exec()
  oApp.CloseDatabase()
}

class App

  cDir = currentdir() + "/"
  oCon
  aIDs = []

  win1 = new qWidget()
  {
        setWindowTitle("Weight History")
        resize(600,600)
        layoutButtons = new qhboxlayout()
        {
          label1 = new qLabel(win1) { setText("Weight") }
          text1 = new qlineedit(win1)
          btnAdd = new qpushbutton(win1) {
                  setText("Add")
                  setClickEvent($ApplicationObject+".AddWeight()")
          }
          btnDelete = new qpushbutton(win1) {
                  setText("Delete")
                  setClickEvent($ApplicationObject+".Deleteweight()")
          }
          addwidget(label1)
          addwidget(text1)
          addwidget(btnAdd)
          addwidget(btnDelete)
        }
        layoutData  = new qhboxlayout()
        {
          Table1 = new qTableWidget(win1) {
                setrowcount(0)
                setcolumncount(3)
                setselectionbehavior(QAbstractItemView_SelectRows)
                setHorizontalHeaderItem(0, new QTableWidgetItem("Date"))
                setHorizontalHeaderItem(1, new QTableWidgetItem("Time"))
                setHorizontalHeaderItem(2, new QTableWidgetItem("Weight"))
                setitemChangedEvent($ApplicationObject+".ItemChanged()")
                                   setAlternatingRowColors(true)
                                   horizontalHeader().setStyleSheet("color: blue")
                                   verticalHeader().setStyleSheet("color: red")
          }
          addWidget(Table1)
        }
        layoutClose = new qhboxlayout()
        {
          btnclose = new qpushbutton(win1) {
            setText("Close")
            setClickEvent("MyApp.Quit()")
          }
          addwidget(btnClose)
        }
        layoutMain = new qvboxlayout()
        {
          addlayout(layoutButtons)
          addLayout(LayoutData)
          addLayout(layoutClose)
        }
        setlayout(layoutMain)
        self.OpenDatabase()
        self.ShowRecords()
        show()
  }

  Func OpenDatabase
        lCreate = False
        if not fexists(cDir + "weighthistory.db")
          lCreate = True
        ok
        new QSqlDatabase() {
          this.oCon = addDatabase("QSQLITE") {
                setDatabaseName("weighthistory.db")
                Open()
          }
        }
        if lCreate
          new QSqlQuery( ) {
                exec("create table weighthistory (id integer primary key,"+
                     " f_date varchar(10),"+
                     " f_time varchar(8), f_weight varchar(8) );")
                delete()
          }
        ok


  Func CloseDatabase
        oCon.Close()

  Func AddWeight
        cWeight = text1.text()
        AddRecord(cWeight)

  Func DeleteWeight
        Table1 {
           nRow = CurrentRow()
          if nRow >= 0
                nID = this.aIDs[nROW+1]
                new QSqlQuery( ) {
                  exec("delete from weighthistory where id = " + nID )
                }
                Del(this.aIDs,nRow+1)
                removerow(nRow)
                selectrow(nRow)
          ok
        }


  Func AddRecord cWeight
        new QSqlQuery( ) {
          cStr = "insert into weighthistory (f_date,f_time,f_weight) values"+
          " ('%f1','%f2','%f3')"
          cDate = Date()
          cTime = Time()
          cStr = substr(cStr,"%f1",cDate)
          cStr = substr(cStr,"%f2",cTime)
          cStr = substr(cStr,"%f3",cWeight)
          exec(cStr)
          delete()
        }
        ShowRecords()
        Table1.selectrow(table1.rowcount()-1)


  Func ShowRecords
        table1.setitemChangedEvent("")
        aIDs = []
        query = new QSqlQuery() {
          exec("select * from weighthistory")
          nRows = 0
          this.Table1.setrowcount(0)
          while movenext()
                this.table1 {
                  insertRow(nRows)
                  this.aIDs + query.value(0).tostring()
                  for x = 1 to 3
                        cStr = query.value(x).tostring()
                        item = new qTableWidgetItem(cStr)
                        setItem(nRows,x-1,item)
                  next
                }
                nRows++
          end
          delete()
        }
        table1.setitemChangedEvent($ApplicationObject+".ItemChanged()")

  Func ItemChanged
        nRow =  table1.currentrow()
        if nRow >= 0
          myitem = Table1.item(table1.currentrow(),0)
          cDate = myitem.text()
          myitem = Table1.item(table1.currentrow(),1)
          cTime = myitem.text()
          myitem = Table1.item(table1.currentrow(),2)
          cWeight = myitem.text()
          new QSqlQuery( ) {
                cStr = "update weighthistory set f_date ='%f1' , f_time = '%f2' , "+
                "f_weight ='%f3' where id = " +  this.aIDs[nROW+1]
                cStr = substr(cStr,"%f1",cDate)
                cStr = substr(cStr,"%f2",cTime)
                cStr = substr(cStr,"%f3",cWeight)
                exec(cStr)
                delete()
          }
        ok
