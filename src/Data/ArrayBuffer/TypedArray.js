"use strict";

var ctor = function (dictIsArrayType) {
  return function (arr) {
    return new dictIsArrayType.constructor(arr);
  }
};

exports.fromArray = ctor;
exports.fromArrayBuffer = ctor;
exports.fromTypedArray = ctor;

exports.buffer = function (av) {
  return av.buffer;
};

exports.byteLength = function (av) {
  return av.byteLength;
};

exports.byteOffset = function (av) {
  return av.byteOffset;
};

exports.length = function (av) {
  return av.length;
};

exports.empty = function(dictIsArrayType) {
  return new dictIsArrayType.constructor();
};

exports.every = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.every(callback);
    };
  };
};

exports.filter = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.filter(callback);
    };
  };
};

exports.map = function(dictIsArrayType) {
  return function(callback) {
    return function(av) {
      return av.map(callback);
    };
  };
};

exports.foldl = function(dictIsArrayType) {
  return function(callback) {
    return function(initialValue) {
      return function(av) {
        function uncurriedCallback(previousValue, currentValue) {
          return callback(previousValue)(currentValue);
        }
        return av.reduce(uncurriedCallback, initialValue);
      };
    };
  };
};

exports.unsafeIndexImpl = function(dictIsArrayType) {
  return function (xs) {
    return function (n) {
      return xs[n];
    };
  };
};

/* TODO
exports.bytesPerElement = function (isArrayType) {
  return isArrayType.constructor.BYTES_PER_ELEMENT;
};
*/

exports.int8ArrayConstructor = Int8Array;
exports.uint8ArrayConstructor = Uint8Array;
exports.float32ArrayConstructor = Float32Array;
