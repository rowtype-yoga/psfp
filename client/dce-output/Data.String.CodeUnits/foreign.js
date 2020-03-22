"use strict";







  

exports.singleton = function (c) {
  return c;
};

















  

exports.length = function (s) {
  return s.length;
};







  

exports._indexOf = function (just) {
  return function (nothing) {
    return function (x) {
      return function (s) {
        var i = s.indexOf(x);
        return i === -1 ? nothing : just(i);
      };
    };
  };
};












































  

exports.drop = function (n) {
  return function (s) {
    return s.substring(n);
  };
};













  
