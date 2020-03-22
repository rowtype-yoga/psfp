"use strict";

exports["null"] = null;

exports.nullable = function (a, r, f) {
  return a == null ? r : f(a);
};



  
