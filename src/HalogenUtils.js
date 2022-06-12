'use strict';

function scrollBy(x) {
  return function (y) {
    return function (elt) {
      return function () {
        elt.scrollBy({
          top: y,
          left: x,
          behavior: 'instant'
        });
      };
    };
  };
};

export { scrollBy }
