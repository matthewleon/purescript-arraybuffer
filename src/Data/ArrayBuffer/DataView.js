"use strict";

exports.fromArrayBuffer = function(ab) {
  return new DataView(ab);
};

exports.getterImpl = function(just, nothing, getterName, length, endianness, dataView, offset) {
  try {
    return just(dataView[getterName](offset, endianness));
  }
  catch (e) {
    if (e instanceof RangeError) return nothing;
    else throw e;
  }
};

exports.buffer = function(dv) {
  return dv.buffer;
};

exports.byteLength = function(dv) {
  return dv.byteLength;
};

exports.byteOffset = function(dv) {
  return dv.byteLength;
};
