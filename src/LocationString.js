"use strict";

exports.getLocationString = function () {
  return window.location.href;
};

exports.getLocationBaseString = function () {
  return window.location.origin + window.location.pathname;
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


/*
exports.decodeAndGetFragmentString = function () {
  return decodeURIComponent(exports.getFragmentString());
};

exports.encodeAndSetFragmentString = function (value) {
  return function () {
    _setFragmentString(encodeURIComponent(value));
  }
};
*/


// The popstate event of the Window interface is fired when the active history entry changes while the user navigates the session history. 
// Hack for making sure hashchanges trigger a reload when nagivating history
// TODO: handle this event properly without a reload by executing the Initialize action on this event
window.addEventListener('popstate', () => {
  window.location.reload();
});

// TODO: move to purescript
var root = document.querySelector(':root');
window.addEventListener('resize', () => {
  root.style.setProperty('--app-height', vh() + 'px');
});
function vh() {
  // - 1 because sometimes the innerHeight doesn't seem to be rounded correctly or something
  return Math.max(document.documentElement.clientHeight, window.innerHeight || 0) - 1;
}
