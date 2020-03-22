"use strict";

exports.map_ = function (f) {
  return function (a) {
    return function () {
      return f(a());
    };
  };
};

exports.pure_ = function (a) {
  return function () {
    return a;
  };
};

exports.bind_ = function (a) {
  return function (f) {
    return function () {
      return f(a())();
    };
  };
};

exports.run = function (f) {
  return f();
};









  

exports["for"] = function (lo) {
  return function (hi) {
    return function (f) {
      return function () {
        for (var i = lo; i < hi; i++) {
          f(i)();
        }
      };
    };
  };
};









  

exports.new = function (val) {
  return function () {
    return { value: val };
  };
};

exports.read = function (ref) {
  return function () {
    return ref.value;
  };
};









  

exports.write = function (a) {
  return function (ref) {
    return function () {
      return ref.value = a; // eslint-disable-line no-return-assign
    };
  };
};
