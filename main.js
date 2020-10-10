var langauges;
var $modalList;

$(document).ready(function () {
    var $langs = $("#langs");
    $modalList = $('#langModal #langExtensions');

    $.get("https://rawgit.com/github/linguist/master/lib/linguist/languages.yml", function (data) {
        languages = jsyaml.safeLoad(data);
        console.log(languages);

        for (var language in languages) {
            attrs = languages[language];
            var color = attrs.color ? attrs.color : "#ccc";
            var type = attrs.type ? attrs.type : "";
            // sanitize language name
            var languageId = language.replace(/(\s|\.|\(|\)|')+/g, "").replace(/\#+/g, "sharp").replace(/\++/g, "plus").replace(/\*+/g, "star");
            $langs.append('<div id="' + languageId + '" datacode="'+language+'" class="col-lg-3 col-md-4 col-sm-6 mb-4 lang" data-ext="'+attrs.extensions+'"><div class="card mb-3 h-100"> <div class="card-body pb-0"> <div class="row"> <div class="col-12"> <h4 class="card-title">' + language + '</h4> </div></div><h6 class="card-subtitle mb-2 text-muted">' + type + '</h6><p class="card-text">' + color + '</p></div><div class="rect ml-3 mr-3 mb-3 mt-0" style="background-color: ' + color + '"></div></div></div>');
        }

        $("#numberOfLanguages").text($("#langs .lang").length + ' langauges').show();
    });
});

$(document).on('click', '.lang', function () {
    $modalList.empty();
    var languageId = "#" + $(this).attr('id');
    var extensions = $(languageId).attr('data-ext').split(",");
    if (extensions != "undefined") {
        extensions.forEach(function(entry) {
            $modalList.append('<li>' + entry + '</li>');
        });
        $('#langModal').modal('show');
    }
});

$(function () {
    $("#searchTerm").on("keyup", function() { search(); });
});

function search() {
    var value = $("#searchTerm").val().toLowerCase();
    $("#langs .lang").css("display", function () {
        return this.attributes.datacode.nodeValue.toLowerCase().indexOf(value) > -1 ? "" : "none"
    });
}