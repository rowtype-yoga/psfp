"use strict";





  

exports.unsafeGet = function (label) {
  return function (rec) {
    return rec[label];
  };
};


























  
