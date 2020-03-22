"use strict";

var refEq = function (r1) {
  return function (r2) {
    return r1 === r2;
  };
};

                              
exports.eqIntImpl = refEq;
exports.eqNumberImpl = refEq;
exports.eqCharImpl = refEq;
exports.eqStringImpl = refEq;












  
