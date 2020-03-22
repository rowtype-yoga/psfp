"use strict";

exports._read = function (nothing, just, value) {
  var tag = Object.prototype.toString.call(value);
  if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
    return just(value);
  } else {
    return nothing;
  }
};
























































































































































































  

// - CSSOM ---------------------------------------------------------------------

exports.getBoundingClientRect = function (el) {
  return function () {
    var rect = el.getBoundingClientRect();
    return {
      top: rect.top,
      right: rect.right,
      bottom: rect.bottom,
      left: rect.left,
      width: rect.width,
      height: rect.height
    };
  };
};





  

exports.offsetTop = function (el) {
  return function () {
    return el.offsetTop;
  };
};

















  
