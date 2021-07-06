"use strict";

exports.getLocationString = function () {
  return window.location.href;
};

exports.getLocationBaseString = function () {
  return window.location.origin + window.location.pathname;
};

exports.setLocationString = function (value) {
  return function () {
    _setLocationString(value);
  }
};

var _setLocationString = function (value) {
  var newurl = value;
  window.location = newurl;
  // window.history.pushState({}, '', newurl);
};


exports.getFragmentString = function () {
  var result = window.location.hash;
  if (result.startsWith("#"))
    return result.substring(1);
  return "";
};

exports.setFragmentString = function (value) {
  return function () {
    _setFragmentString(value);
  }
};

var _setFragmentString = function (value) {
  var newurl = window.location.protocol + "//" + window.location.host +
    window.location.pathname + window.location.search + '#' + value;
  // Only changing the fragment string shouldn't trigger a page reload in
  // itself, but we do it like this to be safe and avoid potential sitatuations
  // of changing a query string and then the fragment string still triggering a
  // reload or something.
  window.history.pushState({}, '', newurl);
};


exports.getQueryString = function () {
  var result = window.location.search;
  if (result.startsWith("?"))
    return result.substring(1);
  return "";
};

exports.setQueryString = function (value) {
  return function () {
    _setQueryString(value);
  }
};

var _setQueryString = function (value) {
  var newurl = window.location.protocol + "//" + window.location.host +
    window.location.pathname + '?' + value + window.location.hash;
  // Set the location without triggering a page reload.
  window.history.pushState({}, '', newurl);
};

exports._getQueryParam = function (paramName) {
  return function () {
    const urlParams = new URLSearchParams(window.location.search);
    return urlParams.get(paramName);
  }
}
