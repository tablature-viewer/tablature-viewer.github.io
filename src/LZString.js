"use strict";
const LZString = require('lz-string');

function unsafeCompressToEncodedURIComponent(value) {
  return LZString.compressToEncodedURIComponent(value);
};

function unsafeDecompressFromEncodedURIComponent(value) {
  return LZString.decompressFromEncodedURIComponent(value);
};

export { unsafeCompressToEncodedURIComponent, unsafeDecompressFromEncodedURIComponent }
