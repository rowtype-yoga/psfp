"use strict";

exports.unsafeReadPropImpl = function (f, s, key, value) {
  return value == null ? f : s(value[key]);
};







  
