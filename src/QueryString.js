"use strict";

exports.getQueryString = function () {
  return decodeURIComponent(exports.getRawQueryString());
};

exports.getRawQueryString = function () {
  var result = window.location.search;
  if (result.startsWith("?"))
    return result.substring(1);
  return "";
};

exports.setQueryString = function (value) {
  return function () {
    setRawQueryString(encodeURIComponent(value));
  }
};

exports.setRawQueryString = function (value) {
  return function () {
    setRawQueryString(value);
  }
};

// Set the query string without triggering a page reload.
var setRawQueryString = function (value) {
  var newurl = window.location.protocol + "//" + window.location.host + window.location.pathname + '?' + value;
  window.history.replaceState({ path: newurl }, '', newurl);
};

exports.setQueryParameters = function (params) {
  var encodedParams = Object.keys(params).map(function (key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  exports.setRawQueryString(encodedParams);
};
