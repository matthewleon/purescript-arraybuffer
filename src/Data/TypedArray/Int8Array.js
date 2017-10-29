"use strict";

exports.fromArray = function(xs) {
  return new Int8Array(xs);
};

exports.fromArrayBuffer = function(ab) {
  return new Int8Array(ab);
};
