"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
var react_spring_1 = require("react-spring");
exports.useSpringImpl = function (mkStyles) { return function () {
    var result = react_spring_1.useSpring(mkStyles);
    return { style: result[0], set: result[1], stop: result[2] };
}; };
exports.useTransitionImpl = react_spring_1.useTransition;
exports.animatedComponentImpl = function (name) { return react_spring_1.animated[name]; };
exports.animatedImpl = react_spring_1.animated;
