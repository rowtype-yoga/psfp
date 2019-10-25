'use strict'

var smoothScroll = require("smoothscroll-polyfill")
var smoothScrollingActivated = false;
exports.smoothScrollPolyfill = function() {
  if(smoothScrollingActivated) { return; }
  smoothScroll.polyfill();
}
