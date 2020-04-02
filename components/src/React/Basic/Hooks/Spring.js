"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var react_spring_1 = require("react-spring");
exports.useSpringImpl = function (tuple) { return function (styles) { return function () {
    var result = react_spring_1.useSpring(styles);
    return tuple(result[0])(result[1]);
}; }; };
exports.useTransitionImpl = react_spring_1.useTransition;
exports.animatedComponentImpl = function (name) { return react_spring_1.animated[name]; };
exports.animatedImpl = react_spring_1.animated;
