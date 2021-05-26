"use strict";
const LZString = require('lz-string');

exports.unsafeCompressToEncodedURIComponent = function (value) {
  return LZString.compressToEncodedURIComponent(value);
};

exports.unsafeDecompressFromEncodedURIComponent = function (value) {
  return LZString.decompressFromEncodedURIComponent(value);
};
