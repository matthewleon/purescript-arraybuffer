"use strict";

exports.fromArrayBuffer = function (arrayBuffer) {
  return function() {
    return new DataView(arrayBuffer.slice());
  };
};
