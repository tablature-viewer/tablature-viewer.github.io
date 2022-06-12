'use strict';

function copyToClipboard(text) {
  return function () {
    window.prompt("Copy the following link:", text);
  }
}

export { copyToClipboard }
