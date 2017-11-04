"use strict";

exports.fromArrayBuffer = function(ab) {
  return new DataView(ab);
};

exports.getterImpl = function(just, nothing, s, l, e, v, o) {
  return ((o + l)>>>0) <= v.byteLength? just(v[s].call(v,o,e)) : nothing;
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
