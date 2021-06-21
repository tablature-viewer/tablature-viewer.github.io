'use strict';

exports.setLocalStorage = function (key) {
  return function (value) {
    return function () {
      window.localStorage.setItem(key, value);
    }
  }
}

exports.getLocalStorage = function (key) {
  return function () {
    return window.localStorage.getItem(key);
  }
}
