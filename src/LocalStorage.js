'use strict';

function setLocalStorage(key) {
  return function (value) {
    return function () {
      window.localStorage.setItem(key, value);
    }
  }
}

function getLocalStorage(key) {
  return function () {
    return window.localStorage.getItem(key);
  }
}

export { setLocalStorage, getLocalStorage }
