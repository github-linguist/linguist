(function(url,callback){//on some browsers you can check certificate information.
xhr=new XMLHttpRequest();
xhr.open('GET',url,true);
xhr.onreadystatechange=function(){if(xhr.readyState==xhr.DONE){callback(xhr)}};
xhr.send();
})('https://sourceforge.net',function(xhr){console.log(xhr.response)})
