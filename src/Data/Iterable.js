"use strict";

exports.arrayIndexImpl = function (arr) {
  return function (i) {
    return arr[i];
  }
};

exports.arrayLengthImpl = function (arr) {
  return arr.length;
};
