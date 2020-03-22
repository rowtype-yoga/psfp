/* globals exports */
"use strict";



                      

exports.infinity = Infinity;









                               

var formatNumber = function (format) {
  return function (fail, succ, digits, n) {
    try {
      return succ(n[format](digits));
    }
    catch (e) {
      return fail(e.message);
    }
  };
};



                                                   

var encdecURI = function (encdec) {
  return function (fail, succ, s) {
    try {
      return succ(encdec(s));
    }
    catch (e) {
      return fail(e.message);
    }
  };
};




                                                            
