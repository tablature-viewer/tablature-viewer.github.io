'use strict';

exports.copyToClipboard = function (text) {
  return function () {
    window.prompt("Copy the following link:", text);
  }
}
