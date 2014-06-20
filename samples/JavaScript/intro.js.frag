(function(window, angular) {

Array.prototype.last = function() {
    return this[this.length-1];
};

var app = angular.module('ConwayGameOfLife', []);
