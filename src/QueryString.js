"use strict";

exports.getRawQueryString = function () {
  var result = window.location.search;
  if (result.startsWith("?"))
    return result.substring(1);
  return "";
};

// Set the query string without triggering a page reload.
exports.setRawQueryString = function (value) {
  return function () {
    var newurl = window.location.protocol + "//" + window.location.host + window.location.pathname + '?' + value;
    window.history.replaceState({ path: newurl }, '', newurl);
  }
};

exports.setQueryParameters = function (params) {
  var encodedParams = Object.keys(params).map(function (key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  exports.setRawQueryString(encodedParams);
};
