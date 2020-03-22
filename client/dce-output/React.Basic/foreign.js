"use strict";

var React = require("react");
var createElement = React.createElement;
var Fragment = React.Fragment || "div";

























































































































































  

exports.empty = null;



  

function flattenDataProp(component, props) {
  var data = null;
  if (typeof component === "string" && props._data != null) {
    data = { _data: undefined };
    Object.entries(props._data).forEach(function(entry) {
      data["data-" + entry[0]] = entry[1];
    });
  }
  return data == null ? props : Object.assign({}, props, data);
}

exports.element_ = function(component, props, areChildrenDynamic) {
  var args = [component, flattenDataProp(component, props)];
  return createElement.apply(
    null,
    areChildrenDynamic || props.children == null
      ? args
      : args.concat(props.children)
  );
};



  

exports.fragment = function(children) {
  return createElement.apply(null, [Fragment, null].concat(children));
};
























































  
