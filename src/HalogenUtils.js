'use strict';

exports.scrollBy = function (x) {
  return function (y) {
    return function (elt) {
      return function () {
        elt.scrollBy({
          top: y,
          left: x,
          behavior: 'smooth'
        });
      };
    };
  };
};
