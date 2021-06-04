'use strict';

exports.copyToClipboard = function (text) {
  return function() {
    window.prompt("Copy to clipboard: Ctrl+C, Enter", text);
  }
}
