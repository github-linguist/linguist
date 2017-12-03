/*
The MIT License (MIT)

Copyright (c) 2014 Thiago Brandão Damasceno

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

// based on http://ctrlq.org/code/19053-send-to-google-drive
function sendToGoogleDrive() {

  var gmailLabels           = 'inbox';
  var driveFolder           = 'Itaú Notifications';
  var spreadsheetName       = 'itau';
  var archiveLabel          = 'itau.processed';
  var itauNotificationEmail = 'comunicacaodigital@itau-unibanco.com.br';
  var filter                = "from: " +
                              itauNotificationEmail +
                              " -label:" +
                              archiveLabel +
                              " label:" +
                              gmailLabels;

  // Create label for 'itau.processed' if it doesn't exist
  var moveToLabel =  GmailApp.getUserLabelByName(archiveLabel);

  if (!moveToLabel) {
    moveToLabel = GmailApp.createLabel(archiveLabel);
  }

  // Create folder 'Itaú Notifications' if it doesn't exist
  var folders = DriveApp.getFoldersByName(driveFolder);
  var folder;

  if (folders.hasNext()) {
    folder = folders.next();
  } else {
    folder = DriveApp.createFolder(driveFolder);
  }

  // Create spreadsheet file 'itau' if it doesn't exist
  var files = folder.getFilesByName(spreadsheetName);

  // File is in DriveApp
  // Doc is in SpreadsheetApp
  // They are not interchangeable
  var file, doc;

  // Confusing :\
  // As per: https://code.google.com/p/google-apps-script-issues/issues/detail?id=3578
  if (files.hasNext()){
    file = files.next();
    doc = SpreadsheetApp.openById(file.getId());
  } else {
    doc = SpreadsheetApp.create(spreadsheetName);
    file = DriveApp.getFileById(doc.getId());
    folder.addFile(file);
    DriveApp.removeFile(file);
  }

  var sheet = doc.getSheets()[0];

  // Append header if first line
  if(sheet.getLastRow() == 0){
    sheet.appendRow(['Conta', 'Operação', 'Valor', 'Data', 'Hora', 'Email ID']);
  }

  var message, messages, account, operation, value, date, hour, emailID, plainBody;
  var accountRegex = /Conta: (XXX[0-9\-]+)/;
  var operationRegex = /Tipo de operação: ([A-Z]+)/;
  var paymentRegex = /Pagamento de ([0-9A-Za-z\-]+)\ ?([0-9]+)?/;
  var valueRegex = /Valor: R\$ ([0-9\,\.]+)/;
  var dateRegex = /Data: ([0-9\/]+)/;
  var hourRegex = /Hora: ([0-9\:]+)/;
  var emailIDRegex = /E-mail nº ([0-9]+)/;

  var threads = GmailApp.search(filter, 0, 100);

  for (var x = 0; x < threads.length; x++) {
    messages = threads[x].getMessages();

    for (var i = 0; i < messages.length; i++) {
      account, operation, value, date, hour, emailID = [];

      message = messages[i];

      plainBody = message.getPlainBody();

      if(accountRegex.test(plainBody)) {
        account = RegExp.$1;
      }

      if(operationRegex.test(plainBody)) {
        operation = RegExp.$1;
      }

      if(valueRegex.test(plainBody)) {
        value = RegExp.$1;
      }

      if(dateRegex.test(plainBody)) {
        date = RegExp.$1;
      }

      if(hourRegex.test(plainBody)) {
        hour = RegExp.$1;
      }

      if(emailIDRegex.test(plainBody)){
        emailID = RegExp.$1;
      }

      if(paymentRegex.test(plainBody)){
        operation = RegExp.$1;
        if(RegExp.$2){
          operation += ' ' + RegExp.$2
        }
        date = hour = ' - ';
      }

      if(account && operation && value && date && hour){
        sheet.appendRow([account, operation, value, date, hour, emailID]);
      }

      // Logger.log(account);
      // Logger.log(operation);
      // Logger.log(value);
      // Logger.log(date);
      // Logger.log(hour);
    }

    threads[x].addLabel(moveToLabel);
  }
}
