function add12hours(dateString) {

  // Get the parts of the date string
  var parts = dateString.split(/\s+/);
  var date  = parts[1];
  var month = parts[0];
  var year  = parts[2];
  var time  = parts[3];
  var ampm  = time && time.match(/[a-z]+$/i)[0];
  var hr    = Number(time.split(':')[0]);
  var min   = Number(time.split(':')[1].replace(/\D/g,''));
  var zone  = parts[4].toUpperCase();
  var months = ['January','February','March','April','May','June',
     'July','August','September','October','November','December'];
  var zones = {'EST': 300, 'AEST': -600}; // Minutes to add to zone time to get UTC

  // Convert month name to number, zero indexed
  // Could use indexOf but not supported widely
  for (var i=0, iLen=months.length; i<iLen; i++) {
    if (months[i] == month) {
      month = i;
    }
  }
  if (typeof month != 'number') return; // Invalid month name provided

  // Convert hours to 24hr
  if (ampm && ampm.toLowerCase() == 'pm') {
    hr += 12;
  }

  // Add 12 hours to hours
  hr += 12;

  // Create a date object in local zone
  var d = new Date(year, month, date);
  d.setHours(hr, min, 0, 0);

  // Adjust minutes for the time zones
  d.setMinutes(d.getMinutes() + zones[zone] - d.getTimezoneOffset() );

  // d is now a local date representing the same moment as the
  // source date plus 12 hours
  return d;
}

var inputDateString = 'March 7 2009 7:30pm EST';

alert(
  'Input: ' + inputDateString + '\n' +
  '+12hrs in local time: ' + add12hours(inputDateString)
 );
