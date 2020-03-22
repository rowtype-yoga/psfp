














  

exports.thenImpl = function(promise) {
  return function(errCB) {
    return function(succCB) {
      return function() {
        promise.then(succCB, errCB);
      };
    };
  };
};
