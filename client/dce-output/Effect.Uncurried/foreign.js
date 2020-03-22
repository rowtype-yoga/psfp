"use strict";

exports.mkEffectFn1 = function mkEffectFn1(fn) {
  return function(x) {
    return fn(x)();
  };
};

exports.mkEffectFn2 = function mkEffectFn2(fn) {
  return function(a, b) {
    return fn(a)(b)();
  };
};















































  

exports.runEffectFn1 = function runEffectFn1(fn) {
  return function(a) {
    return function() {
      return fn(a);
    };
  };
};

exports.runEffectFn2 = function runEffectFn2(fn) {
  return function(a) {
    return function(b) {
      return function() {
        return fn(a, b);
      };
    };
  };
};

exports.runEffectFn3 = function runEffectFn3(fn) {
  return function(a) {
    return function(b) {
      return function(c) {
        return function() {
          return fn(a, b, c);
        };
      };
    };
  };
};











































































































































  
