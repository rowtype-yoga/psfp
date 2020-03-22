"use strict";

var React = require("react");





                           

exports.useState_ = function(tuple, initialState) {
  var r = React.useState(initialState);
  var state = r[0];
  var setState = r[1];
  if (!setState.hasOwnProperty("$$reactBasicHooks$$cachedSetState")) {
    setState.$$reactBasicHooks$$cachedSetState = function(update) {
      return function() {
        return setState(update);
      };
    };
  }
  return tuple(state, setState.$$reactBasicHooks$$cachedSetState);
};

exports.useEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useEqCache_(eq, key);
  React.useEffect(effect, [memoizedKey]);
};

exports.useLayoutEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useEqCache_(eq, key);
  React.useLayoutEffect(effect, [memoizedKey]);
};













  

exports.useRef_ = React.useRef;

exports.readRef_ = function(ref) {
  return ref.current;
};























  

exports.unsafeSetDisplayName = function(displayName, component) {
  component.displayName = displayName;
  component.toString = function() {
    return displayName;
  };
  return component;
};





  
