"use strict";

exports.getLocationString = function () {
  return window.location.href;
};

exports.getFragmentString = function () {
  return decodeURIComponent(exports.getRawFragmentString());
};

exports.getRawFragmentString = function () {
  var result = window.location.hash;
  if (result.startsWith("#"))
    return result.substring(1);
  return "";
};

exports.setFragmentString = function (value) {
  return function () {
    setRawFragmentString(encodeURIComponent(value));
  }
};

exports.setRawFragmentString = function (value) {
  return function () {
    setRawFragmentString(value);
  }
};

var setRawFragmentString = function (value) {
  var newurl = window.location.protocol + "//" + window.location.host +
    window.location.pathname + window.location.search + '#' + value;
  // Only changing the fragment string shouldn't trigger a page reload in
  // itself, but we do it like this to be safe and avoid potential sitatuations
  // of changing a query string and then the fragment string still triggering a
  // reload or something.
  window.history.replaceState({ path: newurl }, '', newurl);
};

var setRawQueryString = function (value) {
  var newurl = window.location.protocol + "//" + window.location.host +
    window.location.pathname + '?' + value;
  // Set the location without triggering a page reload.
  window.history.replaceState({ path: newurl }, '', newurl);
};

exports.setFragmentParameters = function (params) {
  var encodedParams = Object.keys(params).map(function (key) {
    return key + '=' + encodeURIComponent(params[key].replace('/', ''));
  }).join('&');

  exports.setRawFragmentString(encodedParams);
};
