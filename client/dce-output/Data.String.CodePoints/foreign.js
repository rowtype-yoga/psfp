"use strict";
/* global Symbol */

var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator =
  typeof Symbol !== "undefined" &&
  Symbol != null &&
  typeof Symbol.iterator !== "undefined" &&
  typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";

exports._unsafeCodePointAt0 = function (fallback) {
  return hasCodePointAt
    ? function (str) { return str.codePointAt(0); }
    : fallback;
};

























































  

exports._singleton = function (fallback) {
  return hasFromCodePoint ? String.fromCodePoint : fallback;
};

exports._take = function (fallback) {
  return function (n) {
    if (hasStringIterator) {
      return function (str) {
        var accum = "";
        var iter = str[Symbol.iterator]();
        for (var i = 0; i < n; ++i) {
          var o = iter.next();
          if (o.done) return accum;
          accum += o.value;
        }
        return accum;
      };
    }
    return fallback(n);
  };
};

exports._toCodePointArray = function (fallback) {
  return function (unsafeCodePointAt0) {
    if (hasArrayFrom) {
      return function (str) {
        return Array.from(str, unsafeCodePointAt0);
      };
    }
    return fallback;
  };
};
