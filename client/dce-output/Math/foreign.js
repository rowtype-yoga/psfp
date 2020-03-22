"use strict";

// module Math

exports.abs = Math.abs;





                         

exports.atan2 = function (y) {
  return function (x) {
    return Math.atan2(y, x);
  };
};

                         

exports.cos = Math.cos;





















  

exports.pow = function (n) {
  return function (p) {
    return Math.pow(n, p);
  };
};

exports.remainder = function (n) {
  return function (m) {
    return n % m;
  };
};

exports.round = Math.round;

exports.sin = Math.sin;

exports.sqrt = Math.sqrt;











                             

exports.pi = Math.PI;





                           
