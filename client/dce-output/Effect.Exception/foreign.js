"use strict";



  

exports.error = function (msg) {
  return new Error(msg);
};

exports.message = function (e) {
  return e.message;
};











  

exports.throwException = function (e) {
  return function () {
    throw e;
  };
};















  
