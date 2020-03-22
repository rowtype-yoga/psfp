"use strict";

var unsafeCompareImpl = function (lt) {
  return function (eq) {
    return function (gt) {
      return function (x) {
        return function (y) {
          return x < y ? lt : x === y ? eq : gt;
        };
      };
    };
  };
};

                                           
exports.ordIntImpl = unsafeCompareImpl;
exports.ordNumberImpl = unsafeCompareImpl;
                                          
exports.ordCharImpl = unsafeCompareImpl;

























  
