var
 now = new Date(),
 fmt1 = now.getFullYear() + '-' + (1 + now.getMonth()) + '-' + now.getDate(),
 weekdays = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'],
 months = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December'],
 fmt2 = weekdays[now.getDay()] + ', ' + months[now.getMonth()] + ' ' + now.getDate() + ', ' + now.getFullYear();
print(fmt1);
print(fmt2);
