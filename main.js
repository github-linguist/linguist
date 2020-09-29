var langauges;

$(document).ready(function () {
    var $langs = $("#langs");

    $.get("https://rawgit.com/github/linguist/master/lib/linguist/languages.yml", function (data) {
        languages = jsyaml.safeLoad(data);
        console.log(languages);

        for (var language in languages) {
            attrs = languages[language];
            var color = attrs.color ? attrs.color : "#ccc";
            var type = attrs.type ? attrs.type : "";
            $langs.append('<div id="' + language + '" class="col-lg-3 col-md-4 col-sm-6 mb-4 lang"><div class="card mb-3 h-100"> <div class="card-body pb-0"> <div class="row"> <div class="col-12"> <h4 class="card-title">' + language + '</h4> </div></div><h6 class="card-subtitle mb-2 text-muted">' + type + '</h6><p class="card-text">' + color + '</p></div><div class="rect ml-3 mr-3 mb-3 mt-0" style="background-color: ' + color + '"></div></div></div>');
        }

        $("#numberOfLanguages").text($("#langs .lang").length + ' langauges').show();
    });
});

$(function () {
    $("#searchTerm").on("keyup", function() { search(); });
});

function search() {
    var value = $("#searchTerm").val().toLowerCase();
    $("#langs .lang").css("display", function () {
        return this.id.toLowerCase().indexOf(value) > -1 ? "" : "none"
    });
}