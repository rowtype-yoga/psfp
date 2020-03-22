"use strict";





































  

exports.pushAll = function (as) {
  return function (xs) {
    return function () {
      return xs.push.apply(xs, as);
    };
  };
};





























  

exports.unsafeFreeze = function (xs) {
  return function () {
    return xs;
  };
};





  

function copyImpl(xs) {
  return function () {
    return xs.slice();
  };
}

                          

exports.thaw = copyImpl;


















  
